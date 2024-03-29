#include "bytecode/llvm_builder.h"

#include "atom.h"
#include "common.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "platform/filesystem.h"
#include "type.h"
#include "type_info.h"
#include "util/logger.h"
#include "util/string_builder.h"
#include "zodiac_context.h"

#ifdef _MSC_VER
zodiac_disable_msvc_llvm_warnings()
#endif // _MSC_VER
#include <llvm/ADT/ilist_iterator.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/IR/Argument.h>

// iwyu complains about this on some platforms, constantfolder is used by the Create*Cast functions from llvm.
#include <llvm/IR/ConstantFolder.h> // IWYU pragma: keep

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalObject.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TypeSize.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif // _MSC_VER

#include <iterator>
#include <stdio.h>
#include <string>
#include <system_error>

// Override c's assert again...
#include "util/asserts.h"

namespace Zodiac { namespace Bytecode {

LLVM_Builder llvm_builder_create(Allocator *allocator, Bytecode_Builder *bytecode_builder)
{
    LLVM_Builder result = {};
    llvm_builder_init(&result, allocator, bytecode_builder);
    return result;
}

void llvm_builder_init(LLVM_Builder *builder, Allocator *allocator, Bytecode_Builder *bytecode_builder)
{
    assert(builder);
    assert(allocator);

    builder->allocator = allocator;
    builder->bytecode_builder = bytecode_builder;
    assert(bytecode_builder->zodiac_context);
    builder->zodiac_context = bytecode_builder->zodiac_context;

    auto _target_triple = llvm::sys::getDefaultTargetTriple();
    builder->target_triple = string_copy(allocator,_target_triple);

    if (string_contains(builder->target_triple, "windows")) {
        builder->target_platform = LLVM_Target_Platform::WINDOWS;
    } else if (string_contains(builder->target_triple, "linux")) {
        builder->target_platform = LLVM_Target_Platform::LINUX;
    } else {
        assert(false);
    }

    assert(builder->target_platform != LLVM_Target_Platform::INVALID);

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    builder->llvm_context = new llvm::LLVMContext();
    builder->llvm_module = new llvm::Module("root_module", *builder->llvm_context);
    builder->ir_builder = new llvm::IRBuilder<>(*builder->llvm_context);
    builder->llvm_datalayout = (llvm::DataLayout *)&builder->llvm_module->getDataLayout();

    bool platform_info_found = platform_info(allocator, &builder->platform_info);
    if (!platform_info_found) {
        assert(builder->platform_info.err);
        assert_msg(false, "Failed to get platform info");
    }

    auto out_name = builder->zodiac_context->options.output_file_name;
    assert(out_name.length && out_name.data);
    builder->out_file_name = builder->zodiac_context->options.output_file_name;

    hash_table_create(allocator, &builder->functions);
    hash_table_create(allocator, &builder->struct_types);
    hash_table_create(allocator, &builder->string_literals);
    hash_table_create(allocator, &builder->type_infos);

    builder->current_function = nullptr;

    hash_table_create(allocator, &builder->stored_registers);
    hash_table_create(allocator, &builder->globals);
    stack_init(allocator, &builder->arg_stack);
}

void llvm_builder_free(LLVM_Builder *builder)
{
    delete builder->ir_builder;
    delete builder->llvm_module;
    delete builder->llvm_context;

    free(builder->allocator, builder->target_triple.data);

    hash_table_free(&builder->functions);
    hash_table_free(&builder->struct_types);
    hash_table_free(&builder->string_literals);
    hash_table_free(&builder->stored_registers);
    hash_table_free(&builder->globals);

    stack_free(&builder->arg_stack);

    free_platform_info(&builder->platform_info);
}

void llvm_builder_emit_program(LLVM_Builder *builder, Bytecode_Program *program)
{
    for (s64 i = 0; i < program->globals.count; i++) {
        auto &glob = program->globals[i];

        if (glob.constant && glob.type->kind == Type_Kind::FUNCTION) {
            continue;
        }
        llvm_builder_emit_global(builder, i);
    }

    for (s64 i = 0; i < program->functions.count; i++) {
        if (!(program->functions[i].flags & BC_FUNCTION_FLAG_RUN_WRAPPER)) {
            llvm_builder_register_function(builder, i);
        }
    }

    for (s64 i = 0; i < program->functions.count; i++) {
        if (!(program->functions[i].flags & BC_FUNCTION_FLAG_RUN_WRAPPER)) {
            bool result = llvm_builder_emit_function(builder, i);
            assert(result);
        }
    }
}

void llvm_builder_emit_global(LLVM_Builder *builder, Bytecode_Global_Handle glob_handle)
{
    assert(builder->llvm_context);
    assert(builder->llvm_module);

    assert(glob_handle >= 0 && glob_handle < builder->bytecode_builder->globals.count);
    auto glob = &builder->bytecode_builder->globals[glob_handle];

    auto llvm_type = llvm_type_from_ast_type(builder, glob->type);
    builder->llvm_module->getOrInsertGlobal(glob->atom.data, llvm_type);
    llvm::GlobalVariable *llvm_glob_var = builder->llvm_module->getNamedGlobal(glob->atom.data);

    llvm_glob_var->setConstant(glob->constant);
    llvm_glob_var->setLinkage(llvm::GlobalValue::PrivateLinkage);

    if (glob->initial_value.kind != Bytecode_Register_Kind::INVALID) {

        if (glob->initial_value.kind == Bytecode_Register_Kind::TEMPORARY) {
            // This assumes these non const literals point to already initialized globals...
            assert(glob->initial_value.kind == Bytecode_Register_Kind::TEMPORARY);
            assert((glob->initial_value.flags & BC_REGISTER_FLAG_CONSTANT) ||
                   (glob->initial_value.flags & BC_REGISTER_FLAG_LITERAL));
        } else {
            assert(glob->initial_value.kind == Bytecode_Register_Kind::ZEROINITIALIZER);
        }

        llvm::Constant *init_val = nullptr;

        if (glob->atom == "_type_info_pointers") {
            auto array_ptr = llvm_type_info_array_pointer(builder);
            auto array_count = llvm::ConstantInt::get(llvm_type_from_ast_type(builder, &builtin_type_s64), builder->zodiac_context->type_infos.count);

            auto struct_type = static_cast<llvm::StructType *>(llvm_type);
            llvm::Constant *members[2] = { array_ptr, array_count };
            init_val = llvm::ConstantStruct::get(struct_type, members);

        } else {
            init_val = llvm_builder_emit_constant(builder, glob->initial_value);
        }
        assert(init_val);

        llvm_glob_var->setInitializer(init_val);

    } else {
        llvm_glob_var->setInitializer(llvm::Constant::getNullValue(llvm_type));
    }

    hash_table_add(&builder->globals, glob_handle, llvm_glob_var);
}

void llvm_builder_register_function(LLVM_Builder *builder, Bytecode_Function_Handle fn_handle)
{
    assert(builder->llvm_context);
    assert(builder->llvm_module);

    assert(fn_handle >= 0 && fn_handle <  builder->bytecode_builder->functions.count);

    auto func = &builder->bytecode_builder->functions[fn_handle];

    llvm::Type *_llvm_func_type = llvm_type_from_ast_type(builder, func->type);
    auto llvm_func_type = static_cast<llvm::FunctionType*>(_llvm_func_type);
    llvm::StringRef fn_name(func->name.data, func->name.length);
    llvm::Function *llvm_func = llvm::Function::Create(llvm_func_type, llvm::Function::ExternalLinkage, fn_name, builder->llvm_module);
    assert(llvm_func);

    hash_table_add(&builder->functions, fn_handle, llvm_func);
}

bool llvm_builder_emit_function(LLVM_Builder *builder, Bytecode_Function_Handle fn_handle)
{
    assert(fn_handle >= 0 && fn_handle < builder->bytecode_builder->functions.count);
    assert(builder->current_bytecode_function == nullptr);
    Bytecode_Function *bc_func = &builder->bytecode_builder->functions[fn_handle];
    builder->current_bytecode_function = bc_func;

    llvm::Function *llvm_func = nullptr;
    bool registered = hash_table_find(&builder->functions, fn_handle, &llvm_func);
    assert(registered);
    if (!registered) {
        return false;
    }

    assert(builder->current_function == nullptr);
    builder->current_function = llvm_func;

    hash_table_reset(&builder->stored_registers);

    assert(stack_count(&builder->arg_stack) == 0);

    Bytecode_Block_Handle bc_block_handle = bc_func->first_block_handle;

    auto blocks = temp_array_create<LLVM_Block>(temp_allocator_allocator(), bc_func->blocks.count);

    for (s64 block_index = 0; block_index < bc_func->blocks.count; block_index++) {

        assert(bc_block_handle >= 0 && bc_block_handle < bc_func->blocks.count);
        Bytecode_Block *bc_block = &bc_func->blocks[bc_block_handle];

        llvm::StringRef block_name(bc_block->name.data, bc_block->name.length);
        auto llvm_block = llvm::BasicBlock::Create(*builder->llvm_context, block_name, llvm_func);

        dynamic_array_append(&blocks, { bc_block_handle, llvm_block });

        bc_block_handle = bc_block->next;
    }

    assert(bc_block_handle == -1);

    bool result = true;

    bc_block_handle = bc_func->first_block_handle;

    for (s64 block_index = 0; block_index < bc_func->blocks.count; block_index++) {
        assert(bc_block_handle >= 0 && bc_block_handle < bc_func->blocks.count);
        Bytecode_Block *bc_block = &bc_func->blocks[bc_block_handle];

        llvm::BasicBlock *llvm_block = get_llvm_block(builder, blocks.array, bc_block_handle);
        builder->ir_builder->SetInsertPoint(llvm_block);
        builder->current_block = { bc_block_handle, llvm_block };

        bool block_result = true;

        for (s64 inst_index = 0; inst_index < bc_block->instructions.count; inst_index++) {
            Bytecode_Instruction &bc_inst = bc_block->instructions[inst_index];

            bool inst_result = llvm_builder_emit_instruction(builder, bc_inst, blocks.array);
            assert(inst_result);
            if (!inst_result) {
                ZERROR("[llvm_builder] Unable to emit instruction (index '%lli') from function '%.*s'\n",
                        inst_index,
                        (int)bc_block->name.length, bc_block->name.data);
                block_result = false;
            }
        }

        if (!block_result) {
            ZERROR("[llvm_builder] Errors while emitting block '%.*s' from function '%.*s'\n",
                    (int)bc_block->name.length, bc_block->name.data,
                    (int)bc_func->name.length, bc_func->name.data);
            result = false;
        }

        bc_block_handle = bc_block->next;
    }

    assert(bc_block_handle == -1);

    assert(stack_count(&builder->arg_stack) == 0);
    builder->current_function = nullptr;

    bool verify_error = llvm::verifyFunction(*llvm_func, &llvm::errs());
    assert_msg(!verify_error, "LLVM ir verification failed...");

    builder->current_bytecode_function = nullptr;

    return result;
}

#define EMIT_INTEGER_BINOP_AUTO(op) { \
    llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a); \
    llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b); \
    assert(bc_inst.a.type->kind == Type_Kind::INTEGER); \
    llvm::Value *result = irb->Create##op(lhs, rhs); \
    llvm_builder_store_result(builder, bc_inst.dest, result); \
    break; \
}

#define EMIT_INTEGER_BINOP(op) { \
    llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a); \
    llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b); \
    assert(bc_inst.a.type->kind == Type_Kind::INTEGER); \
    llvm::Value *result = nullptr; \
    if (bc_inst.a.type->integer.sign) { \
        result = irb->CreateS##op(lhs, rhs); \
    } else { \
        result = irb->CreateU##op(lhs, rhs); \
    } \
    llvm_builder_store_result(builder, bc_inst.dest, result); \
    break; \
}

#define EMIT_INTEGER_CMP_BINOP_AUTO(op) { \
    llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a); \
    llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b); \
    assert(bc_inst.a.type == bc_inst.b.type); \
    assert(bc_inst.a.type->kind == Type_Kind::INTEGER || bc_inst.a.type->kind == Type_Kind::BOOLEAN || bc_inst.a.type->kind == Type_Kind::ENUM); \
    assert(bc_inst.dest.type->kind == Type_Kind::BOOLEAN); \
    llvm::Value *result = irb->CreateICmp##op(lhs, rhs); \
    assert(result); \
    llvm_builder_store_result(builder, bc_inst.dest, result); \
    break; \
}

#define EMIT_INTEGER_CMP_BINOP(op) { \
    llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a); \
    llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b); \
    assert(bc_inst.a.type == bc_inst.b.type); \
    assert(bc_inst.a.type->kind == Type_Kind::INTEGER || bc_inst.a.type->kind == Type_Kind::BOOLEAN || bc_inst.a.type->kind == Type_Kind::ENUM); \
    assert(bc_inst.dest.type->kind == Type_Kind::BOOLEAN); \
    bool sign = (bc_inst.a.type->kind == Type_Kind::INTEGER && bc_inst.a.type->integer.sign) || \
                (bc_inst.a.type->kind == Type_Kind::ENUM && bc_inst.a.type->enumeration.integer_type->integer.sign); \
    llvm::Value *result; \
    if (sign) { \
        result = irb->CreateICmpS##op(lhs, rhs); \
    } else {\
        result = irb->CreateICmpU##op(lhs, rhs); \
    } \
    assert(result); \
    llvm_builder_store_result(builder, bc_inst.dest, result); \
    break; \
}

#define EMIT_FLOAT_BINOP(op) { \
    llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a); \
    llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b); \
    assert(bc_inst.a.type->kind == Type_Kind::FLOAT); \
    llvm::Value *result = irb->Create##op(lhs, rhs); \
    llvm_builder_store_result(builder, bc_inst.dest, result); \
    break; \
}

#define EMIT_FLOAT_CMP_BINOP(op) { \
    llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a); \
    llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b); \
    assert(bc_inst.a.type->kind == Type_Kind::FLOAT); \
    assert(bc_inst.b.type->kind == Type_Kind::FLOAT); \
    assert(bc_inst.dest.type->kind == Type_Kind::BOOLEAN); \
    llvm::Value *result = irb->CreateFCmpO##op(lhs, rhs); \
    assert(result); \
    llvm_builder_store_result(builder, bc_inst.dest, result); \
    break; \
}

bool llvm_builder_emit_instruction(LLVM_Builder *builder, const Bytecode_Instruction &bc_inst, Array_Ref<LLVM_Block> blocks)
{
    auto irb = builder->ir_builder;
    auto ast_allocator = &builder->zodiac_context->ast_allocator;

    switch (bc_inst.op) {
        case Bytecode_Opcode::NOP: {
            assert(false);
        }

        case Bytecode_Opcode::I_ADD: EMIT_INTEGER_BINOP_AUTO(Add)
        case Bytecode_Opcode::I_SUB: EMIT_INTEGER_BINOP_AUTO(Sub)
        case Bytecode_Opcode::I_MUL: EMIT_INTEGER_BINOP_AUTO(Mul)
        case Bytecode_Opcode::I_DIV: EMIT_INTEGER_BINOP(Div)
        case Bytecode_Opcode::I_MOD: EMIT_INTEGER_BINOP(Rem);

        case Bytecode_Opcode::I_EQ:    EMIT_INTEGER_CMP_BINOP_AUTO(EQ)
        case Bytecode_Opcode::I_NEQ:   EMIT_INTEGER_CMP_BINOP_AUTO(NE)
        case Bytecode_Opcode::I_GT:    EMIT_INTEGER_CMP_BINOP(GT)
        case Bytecode_Opcode::I_LT:    EMIT_INTEGER_CMP_BINOP(LT)
        case Bytecode_Opcode::I_GT_EQ: EMIT_INTEGER_CMP_BINOP(GE)
        case Bytecode_Opcode::I_LT_EQ: EMIT_INTEGER_CMP_BINOP(LE)

        case Bytecode_Opcode::F_ADD: EMIT_FLOAT_BINOP(FAdd)
        case Bytecode_Opcode::F_SUB: EMIT_FLOAT_BINOP(FSub)
        case Bytecode_Opcode::F_MUL: EMIT_FLOAT_BINOP(FMul)
        case Bytecode_Opcode::F_DIV: EMIT_FLOAT_BINOP(FDiv)

        case Bytecode_Opcode::F_EQ:    EMIT_FLOAT_CMP_BINOP(EQ)
        case Bytecode_Opcode::F_NEQ:   EMIT_FLOAT_CMP_BINOP(NE)
        case Bytecode_Opcode::F_GT:    EMIT_FLOAT_CMP_BINOP(GT)
        case Bytecode_Opcode::F_LT:    EMIT_FLOAT_CMP_BINOP(LT)
        case Bytecode_Opcode::F_GT_EQ: EMIT_FLOAT_CMP_BINOP(GE)
        case Bytecode_Opcode::F_LT_EQ: EMIT_FLOAT_CMP_BINOP(LE)

        case Bytecode_Opcode::PTR_EQ: {
            auto int_type = llvm::Type::getIntNTy(*builder->llvm_context, bc_inst.a.type->bit_size);

            llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a);
            lhs = irb->CreatePtrToInt(lhs, int_type);
            llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b);
            rhs = irb->CreatePtrToInt(rhs, int_type);

            llvm::Value *result = irb->CreateICmpEQ(lhs, rhs);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::PTR_NEQ: {
            auto int_type = llvm::Type::getIntNTy(*builder->llvm_context, bc_inst.a.type->bit_size);

            llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a);
            lhs = irb->CreatePtrToInt(lhs, int_type);
            llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b);
            rhs = irb->CreatePtrToInt(rhs, int_type);

            llvm::Value *result = irb->CreateICmpNE(lhs, rhs);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::XOR: {
            llvm::Value *lhs = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *rhs = llvm_builder_emit_register(builder, bc_inst.b);
            llvm::Value *result = irb->CreateXor(lhs, rhs);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::SQRT: {
            llvm::Value *operand = llvm_builder_emit_register(builder, bc_inst.a);

            assert(bc_inst.a.type == bc_inst.dest.type);

            bool cast_from_int = false;

            if (bc_inst.a.type->kind == Type_Kind::INTEGER) {

                cast_from_int = true;

                assert(bc_inst.dest.type->kind == Type_Kind::INTEGER);

                if (bc_inst.a.type->integer.sign) {
                    operand = builder->ir_builder->CreateSIToFP(operand, llvm::Type::getDoubleTy(*builder->llvm_context));
                } else {
                    operand = builder->ir_builder->CreateUIToFP(operand, llvm::Type::getDoubleTy(*builder->llvm_context));
                }

            } else {

                assert(bc_inst.a.type->kind == Type_Kind::FLOAT);
                assert(bc_inst.dest.type->kind == Type_Kind::FLOAT);
            }

            assert(operand);

            Type *intrinsic_return_type = cast_from_int ? &builtin_type_r64 : bc_inst.a.type;
            Type *intrinsic_arg_types[] = { intrinsic_return_type };
            Type *intrinsic_type = get_function_type(intrinsic_return_type, intrinsic_arg_types, ast_allocator);

            llvm::Type *op_ty = operand->getType();
            assert(op_ty->isFloatTy() || op_ty->isDoubleTy());

            const char *intrinsic_name = nullptr;
            if (op_ty->isFloatTy()) intrinsic_name = "llvm.sqrt.f32";
            else if (op_ty->isDoubleTy()) intrinsic_name = "llvm.sqrt.f64";
            else { assert(false); }

            assert(intrinsic_name);

            llvm::Function *intrinsic_function = llvm_get_intrinsic(builder, intrinsic_type, intrinsic_name);
            assert(intrinsic_function);

            llvm::Value *llvm_args[] = { operand };
            llvm::Value *result = irb->CreateCall(intrinsic_function, llvm_args);

            if (cast_from_int) {
                if (bc_inst.a.type->integer.sign) {
                     result = builder->ir_builder->CreateFPToSI(result, llvm_type_from_ast_type(builder, bc_inst.a.type));
                } else {
                    result = builder->ir_builder->CreateFPToUI(result, llvm_type_from_ast_type(builder, bc_inst.b.type));
                }
            }

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::TRUNC: {
            assert(bc_inst.a.type->kind == Type_Kind::INTEGER);
            assert(bc_inst.dest.type->kind == Type_Kind::INTEGER);
            assert(bc_inst.dest.type->bit_size < bc_inst.a.type->bit_size);
            llvm::Type *llvm_target_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            llvm::Value *llvm_val = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *result = irb->CreateTrunc(llvm_val, llvm_target_type);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::SEXT: {
            assert(bc_inst.a.type->kind == Type_Kind::INTEGER);
            assert(bc_inst.dest.type->kind == Type_Kind::INTEGER);
            assert(bc_inst.dest.type->bit_size > bc_inst.a.type->bit_size);
            assert(bc_inst.a.type->integer.sign);
            assert(bc_inst.dest.type->integer.sign);

            llvm::Type *llvm_target_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            llvm::Value *llvm_val = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *result = irb->CreateSExt(llvm_val, llvm_target_type);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::ZEXT: {
            assert(bc_inst.a.type->kind == Type_Kind::INTEGER);
            assert(bc_inst.dest.type->kind == Type_Kind::INTEGER);
            assert(bc_inst.dest.type->bit_size > bc_inst.a.type->bit_size);
            assert(!bc_inst.a.type->integer.sign);
            assert(!bc_inst.dest.type->integer.sign);

            llvm::Type *llvm_target_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            llvm::Value *llvm_val = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *result = irb->CreateZExt(llvm_val, llvm_target_type);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::BITCAST: {
            llvm::Value *operand = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Type *dest_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            llvm::Value *result = irb->CreateBitOrPointerCast(operand, dest_type);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::FCAST: {
            auto type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            auto llvm_val = llvm_builder_emit_register(builder, bc_inst.a);
            auto result = irb->CreateFPCast(llvm_val, type);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::BOOL_TO_INT: {
            auto type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            auto llvm_val = llvm_builder_emit_register(builder, bc_inst.a);
            auto result = irb->CreateZExt(llvm_val, type);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::PUSH_ARG: {
            llvm::Value *arg_val = llvm_builder_emit_register(builder, bc_inst.a);
            stack_push(&builder->arg_stack, arg_val);
            break;
        }


        case Bytecode_Opcode::CALL_FOREIGN:
        case Bytecode_Opcode::CALL:
        case Bytecode_Opcode::CALL_PTR: {
            assert(bc_inst.b.kind == Bytecode_Register_Kind::TEMPORARY);
            assert(bc_inst.b.flags & BC_REGISTER_FLAG_LITERAL);
            assert(bc_inst.b.type == &builtin_type_s64);

            s64 arg_count = bc_inst.b.value.integer.s64;

            llvm::FunctionType *func_type = nullptr;
            llvm::Value *llvm_func_val = nullptr;

            if (bc_inst.a.kind == Bytecode_Register_Kind::FUNCTION) {

                llvm::Function *llvm_func = nullptr;
                bool found = hash_table_find(&builder->functions, bc_inst.a.value.function_handle, &llvm_func);
                assert(found);
                if (!found) {
                    ZERROR("[llvm_builder] Unable to find function with handle '%lli' for CALL instruction\n",
                            bc_inst.a.value.function_handle);
                    return false;
                }

                func_type = static_cast<llvm::FunctionType *>(llvm_type_from_ast_type(builder, bc_inst.a.type));
                llvm_func_val = llvm_func;

            } else {
                assert(bc_inst.a.kind == Bytecode_Register_Kind::TEMPORARY);
                assert(bc_inst.a.type->kind == Type_Kind::FUNCTION);

                llvm_func_val = llvm_builder_emit_register(builder, bc_inst.a);
                func_type = static_cast<llvm::FunctionType *>(llvm_type_from_ast_type(builder, bc_inst.a.type));
            }

            assert(llvm_func_val);
            assert(func_type);

            Dynamic_Array<llvm::Value *> llvm_args;
            dynamic_array_create(temp_allocator_allocator(), &llvm_args, arg_count);

            for (s64 i = 0; i < arg_count; i++) {

                auto arg = stack_peek(&builder->arg_stack, (arg_count - 1) - i);

                if (func_type->isVarArg() && arg->getType()->isFloatTy()) {
                    arg = irb->CreateFPExt(arg, llvm::Type::getDoubleTy(*builder->llvm_context));
                }

                dynamic_array_append(&llvm_args, arg);
            }

            stack_pop(&builder->arg_stack, arg_count);

            llvm::Value *result = irb->CreateCall(func_type, llvm_func_val, { llvm_args.data, (size_t)llvm_args.count });

            if (bc_inst.dest.index >= 0) {
                assert(bc_inst.dest.kind == Bytecode_Register_Kind::TEMPORARY);
                llvm_builder_store_result(builder, bc_inst.dest, result);
            }

            dynamic_array_free(&llvm_args);

            break;
        }

        case Bytecode_Opcode::RETURN_VOID: {
            irb->CreateRetVoid();
            break;
        }

        case Bytecode_Opcode::RETURN: {
            llvm::Value *llvm_ret_val = llvm_builder_emit_register(builder, bc_inst.a);
            irb->CreateRet(llvm_ret_val);
            break;
        }

        case Bytecode_Opcode::ALLOC: {
            assert(bc_inst.a.kind == Bytecode_Register_Kind::TYPE);
            assert(bc_inst.dest.kind == Bytecode_Register_Kind::ALLOC);

            llvm::Type *llvm_type = llvm_type_from_ast_type(builder, bc_inst.a.type);

            if (bc_inst.a.type->kind == Type_Kind::FUNCTION) {
                llvm_type = llvm_type->getPointerTo();
            }

            llvm::Twine alloc_name = bc_inst.dest.alloc_name ? bc_inst.dest.alloc_name : "";
            llvm::AllocaInst *llvm_alloca = irb->CreateAlloca(llvm_type, nullptr, alloc_name);

            assert(!hash_table_find(&builder->stored_registers, bc_inst.dest.index, (llvm::Value**)nullptr));

            hash_table_add(&builder->stored_registers, bc_inst.dest.index, (llvm::Value *)llvm_alloca);
            break;
        }

        case Bytecode_Opcode::ADDROF: {
            assert(bc_inst.a.kind == Bytecode_Register_Kind::ALLOC ||
                   bc_inst.a.kind == Bytecode_Register_Kind::GLOBAL);

            llvm::Value *llvm_alloc = llvm_builder_emit_register(builder, bc_inst.a);
            llvm_builder_store_result(builder, bc_inst.dest, llvm_alloc);
            break;
        }

        case Bytecode_Opcode::ADDROF_FUNC: {
            llvm::Value *llvm_fn_ptr = llvm_builder_emit_register(builder, bc_inst.a);
            llvm_builder_store_result(builder, bc_inst.dest, llvm_fn_ptr);
            break;
        }

        case Bytecode_Opcode::STORE_G: {
            assert(bc_inst.b.kind == Bytecode_Register_Kind::GLOBAL);
            assert(bc_inst.a.type == bc_inst.b.type);
            llvm::Value *llvm_source = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *llvm_glob_ = llvm_builder_emit_register(builder, bc_inst.b);

            llvm::GlobalVariable *llvm_glob = llvm::dyn_cast<llvm::GlobalVariable>(llvm_glob_);
            assert(llvm_glob);

            assert(llvm_glob->getType()->isPointerTy());
            assert(llvm_glob->getValueType() == llvm_source->getType());

            irb->CreateStore(llvm_source, llvm_glob);
            break;
        }

        case Bytecode_Opcode::LOAD_G: {
            assert(bc_inst.a.kind == Bytecode_Register_Kind::GLOBAL);
            assert(bc_inst.a.type == bc_inst.dest.type);

            llvm::Value *llvm_global_ = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::GlobalVariable *llvm_global = llvm::dyn_cast<llvm::GlobalVariable>(llvm_global_);
            assert(llvm_global);

            auto llvm_type = llvm_global->getValueType();
            if (llvm_type->isFunctionTy()) {
                assert(false); // probably change this to the pointer type
            }

            llvm::Value *llvm_result = irb->CreateLoad(llvm_type, llvm_global);
            llvm_builder_store_result(builder, bc_inst.dest, llvm_result);
            break;
        }

        case Bytecode_Opcode::STORE_A: {
            assert(bc_inst.b.kind == Bytecode_Register_Kind::ALLOC);
            assert(bc_inst.a.type == bc_inst.b.type);
            llvm::Value *llvm_source = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *llvm_alloca_ = llvm_builder_emit_register(builder, bc_inst.b);

            auto llvm_alloca = llvm::dyn_cast<llvm::AllocaInst>(llvm_alloca_);
            assert(llvm_alloca);

            assert(llvm_alloca->getType()->isPointerTy());
            assert(llvm_alloca->getAllocatedType() == llvm_source->getType());

            irb->CreateStore(llvm_source, llvm_alloca);

            break;
        }

        case Bytecode_Opcode::LOAD_A: {
            assert(bc_inst.a.kind == Bytecode_Register_Kind::ALLOC);
            assert(bc_inst.a.type == bc_inst.dest.type);

            llvm::Value *llvm_alloc = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Type *llvm_type = llvm_type_from_ast_type(builder, bc_inst.a.type);

            if (bc_inst.a.type->kind == Type_Kind::FUNCTION) {
                llvm_type = llvm_type->getPointerTo();
            }

            llvm::Value *llvm_result = irb->CreateLoad(llvm_type, llvm_alloc);
            llvm_builder_store_result(builder, bc_inst.dest, llvm_result);
            break;
        }

        case Bytecode_Opcode::STORE_PTR: {
            assert(bc_inst.b.type->kind == Type_Kind::POINTER);
            assert(bc_inst.b.type->pointer.base == bc_inst.a.type);

            llvm::Value *new_value = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *llvm_ptr = llvm_builder_emit_register(builder, bc_inst.b);
            irb->CreateStore(new_value, llvm_ptr);
            break;
        }

        case Bytecode_Opcode::LOAD_PTR: {
            assert(bc_inst.a.type->kind == Type_Kind::POINTER);

            llvm::Value *llvm_ptr = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Type *llvm_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);

            if (llvm_type->isFunctionTy()) {
                llvm_type = llvm_type->getPointerTo();
            }

            llvm::Value *llvm_result = irb->CreateLoad(llvm_type, llvm_ptr);

            llvm_builder_store_result(builder, bc_inst.dest, llvm_result);
            break;
        }

        case Bytecode_Opcode::INSERT_VALUE: {

            llvm::Value *agg_value = llvm_builder_emit_register(builder, bc_inst.a);
            assert(agg_value);

            llvm::Value *new_value = llvm_builder_emit_register(builder, bc_inst.b);

            unsigned indexes[] = { (unsigned)bc_inst.additional_index };

            llvm::Value *result = irb->CreateInsertValue(agg_value, new_value, indexes);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::EXTRACT_VALUE: {
            llvm::Value *llvm_agg = llvm_builder_emit_register(builder, bc_inst.a);
            assert(bc_inst.b.flags & BC_REGISTER_FLAG_LITERAL);
            assert(bc_inst.b.type == &builtin_type_s32);

            unsigned indexes[] = { (unsigned)bc_inst.b.value.integer.s32 };
            llvm::Value *result = irb->CreateExtractValue(llvm_agg, indexes);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::INSERT_ELEMENT: {

            llvm::Type *llvm_array_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            llvm::Value *array_value = nullptr;

            if (bc_inst.a.kind == Bytecode_Register_Kind::INVALID) {
                array_value = llvm::UndefValue::get(llvm_array_type);
            } else {
                array_value = llvm_builder_emit_register(builder, bc_inst.a);
            }

            assert(array_value);

            llvm::Value *new_value = llvm_builder_emit_register(builder, bc_inst.b);

            unsigned indexes[] = { (unsigned)bc_inst.additional_index };
            llvm::Value *result = irb->CreateInsertValue(array_value, new_value, indexes);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::EXTRACT_ELEMENT: {
            llvm::Value *llvm_array = llvm_builder_emit_register(builder, bc_inst.a);
            assert(bc_inst.b.flags & BC_REGISTER_FLAG_LITERAL);
            assert(bc_inst.b.type == &builtin_type_s64);

            unsigned indexes[] = { (unsigned)bc_inst.b.value.integer.s64 };
            llvm::Value *result = irb->CreateExtractValue(llvm_array, indexes);

            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }


        case Bytecode_Opcode::AGG_OFFSET_POINTER: {
            assert(bc_inst.b.flags & BC_REGISTER_FLAG_LITERAL);
            assert(bc_inst.b.type == &builtin_type_s32);

            llvm::Value *llvm_agg = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *index = llvm_builder_emit_register(builder, bc_inst.b);

            Type *struct_type = nullptr;

            if (bc_inst.a.kind == Bytecode_Register_Kind::ALLOC ||
                bc_inst.a.kind == Bytecode_Register_Kind::GLOBAL) {
                struct_type = bc_inst.a.type;
            } else {
                assert(bc_inst.a.kind == Bytecode_Register_Kind::TEMPORARY);
                assert(bc_inst.a.type->kind == Type_Kind::POINTER);
                struct_type = bc_inst.a.type->pointer.base;
            }

            assert(struct_type);

            if (struct_type->kind == Type_Kind::SLICE) {
                struct_type = struct_type->slice.struct_type;
            }

            assert(struct_type->flags & TYPE_FLAG_AGGREGATE);
            assert(struct_type->kind == Type_Kind::STRUCTURE);

            llvm::Type *llvm_struct_type = llvm_type_from_ast_type(builder, struct_type);

            llvm::Value *llvm_zero = llvm_builder_emit_integer_literal(builder, bc_inst.b.type, { .s32 = 0 });
            llvm::Value *indexes[] = { llvm_zero, index };

            llvm::Value *result = irb->CreateGEP(llvm_struct_type, llvm_agg, indexes);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::ARR_OFFSET_POINTER: {
            assert(bc_inst.b.type == &builtin_type_s64);

            llvm::Value *llvm_array = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *index = llvm_builder_emit_register(builder, bc_inst.b);

            Type *array_type = nullptr;

            if (bc_inst.a.kind == Bytecode_Register_Kind::ALLOC ||
                bc_inst.a.kind == Bytecode_Register_Kind::GLOBAL) {

                array_type = bc_inst.a.type;

            } else {
                assert(bc_inst.a.kind == Bytecode_Register_Kind::TEMPORARY);
                assert(bc_inst.a.type->kind == Type_Kind::POINTER);
                array_type = bc_inst.a.type->pointer.base;
            }

            assert(array_type);
            assert(array_type->kind == Type_Kind::STATIC_ARRAY);

            llvm::Type *llvm_array_type = llvm_type_from_ast_type(builder, array_type);

            llvm::Value *llvm_zero = llvm_builder_emit_integer_literal(builder, bc_inst.b.type, { .s64 = 0 });
            llvm::Value *indexes[] = { llvm_zero, index };

            llvm::Value *result = irb->CreateGEP(llvm_array_type, llvm_array, indexes);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::PTR_OFFSET_POINTER: {
            assert(bc_inst.b.type == &builtin_type_s64);

            llvm::Value *llvm_ptr = llvm_builder_emit_register(builder, bc_inst.a);
            llvm::Value *index = llvm_builder_emit_register(builder, bc_inst.b);

            llvm::Type *llvm_gep_base_type = llvm_type_from_ast_type(builder, bc_inst.a.type->pointer.base);

            if (llvm_gep_base_type->isFunctionTy()) {
                llvm_gep_base_type = llvm_gep_base_type->getPointerTo();
            }

            llvm::Value *indexes[] = { index };

            llvm::Value *result = irb->CreateGEP(llvm_gep_base_type, llvm_ptr, indexes);
            llvm_builder_store_result(builder, bc_inst.dest, result);
            break;
        }

        case Bytecode_Opcode::JMP: {
            assert(bc_inst.a.kind == Bytecode_Register_Kind::BLOCK);
            auto block_handle = bc_inst.a.block_handle;
            assert(block_handle >= 0 && (size_t)block_handle < builder->current_function->size());

            auto llvm_block = get_llvm_block(builder, blocks, block_handle);

            irb->CreateBr(llvm_block);
            break;
        }

        case Bytecode_Opcode::JMP_IF: {
            llvm::Value *llvm_cond = llvm_builder_emit_register(builder, bc_inst.a);

            assert(bc_inst.b.kind == Bytecode_Register_Kind::BLOCK);
            assert(bc_inst.dest.kind == Bytecode_Register_Kind::BLOCK);

            auto then_block_handle = bc_inst.b.block_handle;
            auto else_block_handle = bc_inst.dest.block_handle;

#ifndef NDEBUG
            auto block_count = builder->current_function->size();
#endif
            assert(then_block_handle >= 0 && (size_t)then_block_handle < block_count);
            assert(else_block_handle >= 0 && (size_t)else_block_handle < block_count);

            auto llvm_then_block = get_llvm_block(builder, blocks, then_block_handle);
            auto llvm_else_block = get_llvm_block(builder, blocks, else_block_handle);

            irb->CreateCondBr(llvm_cond, llvm_then_block, llvm_else_block);
            break;
        }

        case Bytecode_Opcode::SWITCH: {

            auto bc_cases = builder->current_bytecode_function->switches[bc_inst.b.switch_handle.index];

            auto switch_value = llvm_builder_emit_register(builder, bc_inst.a);

            llvm::BasicBlock *default_block = get_llvm_block(builder, blocks, bc_cases.default_or_post_block);

            auto switch_inst = irb->CreateSwitch(switch_value, default_block, bc_cases.cases.count);

            for (s64 i = 0; i < bc_cases.cases.count; i++) {

                if (!bc_cases.cases[i].is_default) {
                    auto case_val = static_cast<llvm::ConstantInt*>(llvm_builder_emit_register(builder, bc_cases.cases[i].case_val));
                    auto case_block = get_llvm_block(builder, blocks, bc_cases.cases[i].block_register.block_handle);
                    switch_inst->addCase(case_val, case_block);
                }
            }
            break;
        }

        case Bytecode_Opcode::PHI: {
            auto llvm_type = llvm_type_from_ast_type(builder, bc_inst.dest.type);
            auto llvm_phi_node = irb->CreatePHI(llvm_type, 2);

            assert(bc_inst.a.phi_args_handle < builder->current_bytecode_function->phi_args.count);
            auto phi_args = builder->current_bytecode_function->phi_args[bc_inst.a.phi_args_handle];

            auto llvm_true_value = llvm_builder_emit_register(builder, phi_args.true_value);
            auto llvm_false_value = llvm_builder_emit_register(builder, phi_args.false_value);

            auto llvm_block_it = builder->current_function->begin();
#ifndef NDEBUG
            auto block_count = builder->current_function->size();
#endif
            assert(phi_args.true_block_handle >= 0 && (size_t)phi_args.true_block_handle < block_count);
            assert(phi_args.false_block_handle >= 0 && (size_t)phi_args.false_block_handle < block_count);

            auto llvm_true_block = &*std::next(llvm_block_it, phi_args.true_block_handle);
            auto llvm_false_block = &*std::next(llvm_block_it, phi_args.false_block_handle);

            llvm_phi_node->addIncoming(llvm_true_value, llvm_true_block);
            llvm_phi_node->addIncoming(llvm_false_value, llvm_false_block);

            llvm_builder_store_result(builder, bc_inst.dest, llvm_phi_node);
        }
    }

    return true;
}

#undef EMIT_INTEGER_BINOP_S
#undef EMIT_INTEGER_BINOP
#undef EMIT_INTEGER_CMP_BINOP_S
#undef EMIT_INTEGER_CMP_BINOP
#undef EMIT_FLOAT_BINOP


llvm::Value *llvm_builder_emit_register(LLVM_Builder *builder, const Bytecode_Register &bc_reg)
{
    if (bc_reg.flags & BC_REGISTER_FLAG_CONSTANT) {
        return llvm_builder_emit_constant(builder, bc_reg);
    }

    // Literals should always be constant?
    assert(!(bc_reg.flags & BC_REGISTER_FLAG_LITERAL) || bc_reg.type->kind == Type_Kind::SLICE);


    switch (bc_reg.kind) {
        case Bytecode_Register_Kind::INVALID: assert(false); break;

        case Bytecode_Register_Kind::TEMPORARY: {

            if (bc_reg.flags & BC_REGISTER_FLAG_ARGUMENT) {

                assert(!builder->current_function->isVarArg());

                #ifndef NDEBUG
                    auto arg_count = builder->current_function->arg_size();
                    assert(bc_reg.index >= 0 && bc_reg.index < (s64)arg_count);
                #endif

                return builder->current_function->getArg((unsigned)bc_reg.index);

            } else {

                if (!(bc_reg.flags & BC_REGISTER_FLAG_LITERAL)) {

                    llvm::Value *result = nullptr;
                    bool found = hash_table_find(&builder->stored_registers, bc_reg.index, &result);
                    if (!found) {
                        ZFATAL("[llvm_builder] Unable to find temporary register with index '%lli'\n",
                                bc_reg.index);
                    }
                    assert(result);
                    return result;
                } else {

                    auto llvm_type = llvm_type_from_ast_type(builder, bc_reg.type);
                    llvm::Value *result = llvm::UndefValue::get(llvm_type);

                    for (s64 i = 0; i < bc_reg.value.compound.count; i++) {
                        auto value = llvm_builder_emit_register(builder, bc_reg.value.compound[i]);
                        result = builder->ir_builder->CreateInsertValue(result, value, (unsigned)i);
                    }
                    return result;
                }
            }
            break;
        }

        case Bytecode_Register_Kind::FUNCTION: {
            llvm::Function* llvm_fn = nullptr;
            bool found = hash_table_find(&builder->functions, bc_reg.value.function_handle, &llvm_fn);
            assert(found);
            assert_msg(found, "[LLVM_BUILDER] Failed to find function");
            assert(llvm_fn);
            return llvm_fn;
            break;
        }

        case Bytecode_Register_Kind::BLOCK: assert(false); break;

        case Bytecode_Register_Kind::ALLOC: {
            llvm::Value *llvm_alloca_value = nullptr;
            bool found = hash_table_find(&builder->stored_registers, bc_reg.index, &llvm_alloca_value);
            assert(found);
            assert(llvm_alloca_value);
            if (!found) {
                ZFATAL("[llvm_builder] Unable to find ALLOC register with index '%lli'\n", bc_reg.index);
            }
            llvm::AllocaInst *llvm_alloca = llvm::dyn_cast<llvm::AllocaInst>(llvm_alloca_value);
            assert(llvm_alloca);
            return llvm_alloca;
            break;
        }


        case Bytecode_Register_Kind::TYPE: assert(false); break;

        case Bytecode_Register_Kind::GLOBAL: {
            llvm::GlobalVariable *llvm_glob_var = nullptr;
            bool found = hash_table_find(&builder->globals, bc_reg.index, &llvm_glob_var);
            assert(found);
            assert(llvm_glob_var);
            if (!found) {
                ZFATAL("[llvm_builder] Unable to find GLOBAL register with index '%lli'\n", bc_reg.index);
            }
            return llvm_glob_var;
            break;
        }

        case Bytecode_Register_Kind::UNDEF: {
            auto aggregate_type = bc_reg.type;
            assert(aggregate_type);
            if (aggregate_type->kind != Type_Kind::STATIC_ARRAY) {
                assert(aggregate_type->flags & TYPE_FLAG_AGGREGATE);
                assert(aggregate_type->kind == Type_Kind::STRUCTURE);
            }

            auto llvm_aggregate_type = llvm_type_from_ast_type(builder, aggregate_type);
            return llvm::UndefValue::get(llvm_aggregate_type);
            break;
        }

        case Bytecode_Register_Kind::SWITCH_CASES: assert(false); break;
        case Bytecode_Register_Kind::PHI_ARGS: assert(false); break;

        case Bytecode_Register_Kind::ZEROINITIALIZER: {
            auto llvm_type = llvm_type_from_ast_type(builder, bc_reg.type);
            return llvm::Constant::getNullValue(llvm_type);
        }
    }

    assert(false);
    ZFATAL("[llvm_builder] Unhandled case in lvm_builder_emit_register\n");
    return nullptr;
}

llvm::Constant *llvm_builder_emit_constant(LLVM_Builder *builder, const Bytecode_Register &bc_reg)
{
    // This assumes these non const literals point to already initialized globals...
    assert((bc_reg.flags & BC_REGISTER_FLAG_CONSTANT) ||
           (bc_reg.flags & BC_REGISTER_FLAG_LITERAL) ||
           bc_reg.kind == Bytecode_Register_Kind::GLOBAL ||
           bc_reg.kind == Bytecode_Register_Kind::ZEROINITIALIZER);

    switch (bc_reg.kind) {
        case Bytecode_Register_Kind::INVALID: {
            assert(false);
            break;
        }

        case Bytecode_Register_Kind::TEMPORARY: {
            assert(!(bc_reg.flags & BC_REGISTER_FLAG_ARGUMENT));

            if (bc_reg.type == get_string_type(builder->zodiac_context)) {
                assert(bc_reg.flags & BC_REGISTER_FLAG_LITERAL);
                auto str_reg = bc_reg.value.compound[0];
                return llvm_builder_emit_string_literal(builder, str_reg.value.string);
            }

            if (bc_reg.flags & BC_REGISTER_FLAG_CSTRING) {
                return llvm_builder_emit_cstring_literal(builder, bc_reg.value.string);
            }

            switch (bc_reg.type->kind) {
                case Type_Kind::INVALID: { assert(false); break; }
                case Type_Kind::VOID: { assert(false); break; }
                case Type_Kind::UNSIZED_INTEGER: { assert(false); break; }

                case Type_Kind::INTEGER: {
                    return llvm_builder_emit_integer_literal(builder, bc_reg.type, bc_reg.value.integer);
                }

                case Type_Kind::FLOAT: {
                    return llvm_builder_emit_float_literal(builder, bc_reg.type, bc_reg.value.real);
                }

                case Type_Kind::POINTER: {
                    llvm::Type *type = llvm_type_from_ast_type(builder, bc_reg.type);
                    llvm::Type *inttype = llvm_type_from_ast_type(builder, &builtin_type_u64);
                    llvm::Constant *intval = llvm::ConstantInt::get(inttype, (u64)bc_reg.value.pointer);
                    return llvm::ConstantExpr::getIntToPtr(intval, type);
                }

                case Type_Kind::FUNCTION: {
                    llvm::Type *type = llvm_type_from_ast_type(builder, bc_reg.type)->getPointerTo();
                    llvm::Type *inttype = llvm_type_from_ast_type(builder, &builtin_type_u64);
                    llvm::Constant *intval = llvm::ConstantInt::get(inttype, (u64)bc_reg.value.pointer);
                    return llvm::ConstantExpr::getIntToPtr(intval, type);
                }

                case Type_Kind::BOOLEAN: {
                    return llvm_builder_emit_bool_literal(builder, bc_reg.type, bc_reg.value.boolean);
                }

                case Type_Kind::STRUCTURE: {
                    return llvm_builder_emit_struct_literal(builder, bc_reg.type, bc_reg.value.compound);
                }

                case Type_Kind::ENUM: {
                    return llvm_builder_emit_integer_literal(builder, bc_reg.type->enumeration.integer_type, bc_reg.value.integer);
                }

                case Type_Kind::STATIC_ARRAY: {
                    return llvm_builder_emit_array_literal(builder, bc_reg.type, bc_reg.value.compound);
                }

                case Type_Kind::SLICE: {
                    return llvm_builder_emit_struct_literal(builder, bc_reg.type->slice.struct_type, bc_reg.value.compound);
                }
            }
            break;
        }

        case Bytecode_Register_Kind::FUNCTION: {
            llvm::Function* llvm_fn = nullptr;
            bool found = hash_table_find(&builder->functions, bc_reg.value.function_handle, &llvm_fn);
            assert(found);
            assert_msg(found, "[LLVM_BUILDER] Failed to find function");
            assert(llvm_fn);
            return llvm_fn;
        }

        case Bytecode_Register_Kind::BLOCK: {
            assert(false);
            break;
        }

        case Bytecode_Register_Kind::ALLOC: {
            assert(false);
            break;
        }

        case Bytecode_Register_Kind::TYPE: {
            assert(false);
            break;
        }

        case Bytecode_Register_Kind::GLOBAL: {
            llvm::GlobalVariable *global;
            bool found = hash_table_find(&builder->globals, bc_reg.index, &global);
            assert(found);

            return global;
            break;
        }

        case Bytecode_Register_Kind::SWITCH_CASES: assert(false); break;
        case Bytecode_Register_Kind::PHI_ARGS: assert(false); break;
        case Bytecode_Register_Kind::UNDEF: assert(false); break;

        case Bytecode_Register_Kind::ZEROINITIALIZER: {

            auto llvm_type = llvm_type_from_ast_type(builder, bc_reg.type);
            return llvm::Constant::getNullValue(llvm_type);
            break;
        }
    }

    assert(false);
    return nullptr;
}

llvm::Constant *llvm_builder_emit_integer_literal(LLVM_Builder *builder, Type *type, Integer_Value integer)
{
    assert(type->kind == Type_Kind::INTEGER);

    llvm::Type *llvm_type = llvm_type_from_ast_type(builder, type);

    switch (type->bit_size) {
        default: assert(false && !"Unhandled integer bit size in llvm_builder_emit_integer_literal"); break;
        case 8: return llvm::ConstantInt::get(llvm_type, integer.u8, type->integer.sign);
        case 16: return llvm::ConstantInt::get(llvm_type, integer.u16, type->integer.sign);
        case 32: return llvm::ConstantInt::get(llvm_type, integer.u32, type->integer.sign);
        case 64: return llvm::ConstantInt::get(llvm_type, integer.u64, type->integer.sign);
    }

    assert(false);
    return nullptr;
}

llvm::Constant *llvm_builder_emit_float_literal(LLVM_Builder *builder, Type *type, Real_Value real)
{
    assert(type->kind == Type_Kind::FLOAT);

    llvm::Type *llvm_type = llvm_type_from_ast_type(builder, type);

    switch (type->bit_size) {
        default: assert(false && !"Unhandled float bit size in llvm_builder_emit_float_literal"); break;
        case 32: return llvm::ConstantFP::get(llvm_type, real.r32);
        case 64: return llvm::ConstantFP::get(llvm_type, real.r64);
    }

    assert(false);
    return nullptr;
}

llvm::Constant *llvm_builder_emit_bool_literal(LLVM_Builder *builder, Type *type, bool value)
{
    llvm::Type *llvm_type = llvm_type_from_ast_type(builder, type);
    return llvm::ConstantInt::get(llvm_type, value, false);
}

llvm::Constant *llvm_builder_emit_string_literal(LLVM_Builder *builder, String_Ref str)
{
    llvm::Type *llvm_string_type_ = llvm_type_from_ast_type(builder, get_string_type(builder->zodiac_context));
    llvm::StructType *llvm_string_type = static_cast<llvm::StructType *>(llvm_string_type_);

    auto members = temp_array_create<llvm::Constant *>(temp_allocator_allocator(), 2);

    llvm::Constant *length_val = llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { .s64 = str.length });
    llvm::Constant *cstr_val;
    if (str.length == 0) {
        auto cstr_type = llvm::IntegerType::get(*builder->llvm_context, 8)->getPointerTo();
        cstr_val = llvm::Constant::getNullValue(cstr_type);
    } else {
        cstr_val = llvm_builder_emit_cstring_literal(builder, str);
    }

    dynamic_array_append(&members, cstr_val);
    dynamic_array_append(&members, length_val);

    auto result = llvm::ConstantStruct::get(llvm_string_type, { members.array.data, (size_t)members.array.count });

    temp_array_destroy(&members);

    return result;
}

llvm::Constant *llvm_builder_emit_cstring_literal(LLVM_Builder *builder, String_Ref str)
{
    auto dest_type = llvm::IntegerType::get(*builder->llvm_context, 8)->getPointerTo();

    auto length = str.length;
    Atom atom = atom_get(&builder->zodiac_context->atoms, str.data, length);

    llvm::Constant *result = nullptr;
    if (hash_table_find(&builder->string_literals, atom, &result)) {
        assert(result);
        return result;
    }

    llvm::Constant *data = llvm::ConstantDataArray::getString(*builder->llvm_context, llvm::StringRef(str.data, length), true);
    llvm::GlobalValue *variable = new llvm::GlobalVariable(*builder->llvm_module, data->getType(), true, llvm::GlobalVariable::PrivateLinkage, data, "_string_literal_");
    variable->setUnnamedAddr(llvm::GlobalVariable::UnnamedAddr::Global);
    auto alignment = llvm::MaybeAlign(1);
    static_cast<llvm::GlobalObject*>(variable)->setAlignment(alignment);

    result = llvm::ConstantExpr::getPointerCast(variable, dest_type);
    hash_table_add(&builder->string_literals, atom, result);
    return result;
}

llvm::Constant *llvm_builder_emit_struct_literal(LLVM_Builder *builder, Type *type, Dynamic_Array<Bytecode_Register> compound)
{
    debug_assert(builder && type && compound.count);
    assert(type->flags & TYPE_FLAG_AGGREGATE);
    assert(type->kind == Type_Kind::STRUCTURE);

    llvm::Type *_llvm_struct_type = llvm_type_from_ast_type(builder, type);
    auto llvm_struct_type = static_cast<llvm::StructType *>(_llvm_struct_type);

    auto members = temp_array_create<llvm::Constant *>(temp_allocator_allocator(), compound.count);

    for (s64 i = 0; i < compound.count; i++) {
        auto mem_val = llvm_builder_emit_constant(builder, compound[i]);
        dynamic_array_append(&members, mem_val);
    }

    auto result = llvm::ConstantStruct::get(llvm_struct_type, { members.array.data, (size_t)members.array.count });

    temp_array_destroy(&members);

    return result;
}

llvm::Constant *llvm_builder_emit_array_literal(LLVM_Builder *builder, Type *type, Dynamic_Array<Bytecode_Register> compound)
{
    debug_assert(builder && type && compound.count);
    assert(type->kind == Type_Kind::STATIC_ARRAY);

    llvm::Type *_llvm_array_type = llvm_type_from_ast_type(builder, type);
    auto llvm_array_type = static_cast<llvm::ArrayType *>(_llvm_array_type);

    auto members = temp_array_create<llvm::Constant *>(temp_allocator_allocator(), compound.count);

    for (s64 i = 0; i < compound.count; i++) {
        auto mem_val = llvm_builder_emit_constant(builder, compound[i]);
        dynamic_array_append(&members, mem_val);
    }

    auto result = llvm::ConstantArray::get(llvm_array_type, { members.array.data, (size_t)members.array.count });

    temp_array_destroy(&members);

    return result;
}

void llvm_builder_store_result(LLVM_Builder *builder, const Bytecode_Register &bc_dest_reg, llvm::Value *result_val)
{
    assert(bc_dest_reg.kind == Bytecode_Register_Kind::TEMPORARY);
    assert(!(bc_dest_reg.flags & BC_REGISTER_FLAG_LITERAL));
    assert(!hash_table_find(&builder->stored_registers, bc_dest_reg.index, (llvm::Value**)nullptr));

    hash_table_add(&builder->stored_registers, bc_dest_reg.index, result_val);
}

llvm::Type *llvm_type_from_ast_type(LLVM_Builder *builder, Type *ast_type)
{
    assert(builder->llvm_module);

    auto ast_allocator = builder->allocator;

    switch (ast_type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::VOID: {
            return llvm::Type::getVoidTy(*builder->llvm_context);
        }

        case Type_Kind::INTEGER: {
            return llvm::Type::getIntNTy(*builder->llvm_context, (unsigned)ast_type->bit_size);
        }

        case Type_Kind::FLOAT: {

            if (ast_type->bit_size == 32) {
                return llvm::Type::getFloatTy(*builder->llvm_context);
            } else if (ast_type->bit_size == 64) {
                return llvm::Type::getDoubleTy(*builder->llvm_context);
            } else {
                assert(false && !"Unsupported float type width");
            }
            break;
        }

        case Type_Kind::POINTER: {
            auto base = ast_type->pointer.base;
            if (base->kind == Type_Kind::VOID) {
                // Void pointers are invalid in llvm
                llvm::Type *base_type = llvm_type_from_ast_type(builder, &builtin_type_u8);
                return base_type->getPointerTo();
            } else {
                llvm::Type *base_type = llvm_type_from_ast_type(builder, base);
                return base_type->getPointerTo();
            }
        }

        case Type_Kind::FUNCTION: {
            llvm::Type  *llvm_return_type = llvm_type_from_ast_type(builder, ast_type->function.return_type);
            if (llvm_return_type->isFunctionTy()) {
                llvm_return_type = llvm_return_type->getPointerTo();
            }

            Dynamic_Array<llvm::Type *> llvm_param_types;
            if (ast_type->function.parameter_types.count) {
                dynamic_array_create(ast_allocator, &llvm_param_types, ast_type->function.parameter_types.count);
            }

            for (s64 i = 0; i < ast_type->function.parameter_types.count; i++) {
                llvm::Type *param_type = llvm_type_from_ast_type(builder, ast_type->function.parameter_types[i]);

                if (param_type->isFunctionTy()) {
                    param_type = param_type->getPointerTo();
                }
                dynamic_array_append(&llvm_param_types, param_type);
            }

            llvm::Type *result = llvm::FunctionType::get(llvm_return_type,
                                                         { llvm_param_types.data, (size_t)llvm_param_types.count },
                                                         ast_type->function.is_c_vararg);
            if (llvm_param_types.count) {
                dynamic_array_free(&llvm_param_types);
            }

            return result;
        }

        case Type_Kind::BOOLEAN: {
            assert(ast_type->bit_size == 8);
            // return llvm::Type::getIntNTy(*builder->llvm_context, ast_type->bit_size);
            return llvm::Type::getIntNTy(*builder->llvm_context, 1);
        }

        case Type_Kind::STRUCTURE: {
            llvm::StructType *result = nullptr;

            if (!hash_table_find(&builder->struct_types, ast_type, &result)) {
                auto struct_name = ast_type->structure.name;
                llvm::StringRef struct_name_ref(struct_name.data, struct_name.length);
                result = llvm::StructType::create(*builder->llvm_context, struct_name_ref);

                hash_table_add(&builder->struct_types, ast_type, result);

                Dynamic_Array<llvm::Type *> mem_types;
                dynamic_array_create(temp_allocator_allocator(), &mem_types, ast_type->structure.member_types.count);

                for (s64 i = 0; i < ast_type->structure.member_types.count; i++) {

                    llvm::Type *llvm_mem_type = llvm_type_from_ast_type(builder, ast_type->structure.member_types[i]);
                    if (llvm_mem_type->isFunctionTy()) {
                        llvm_mem_type = llvm_mem_type->getPointerTo();
                    }

                    dynamic_array_append(&mem_types, llvm_mem_type);
                }
                result->setBody( { mem_types.data, (size_t)mem_types.count }, true);
            }

            assert(result);
            return result;
        }

        case Type_Kind::STATIC_ARRAY: {
            llvm::Type *elem_type = llvm_type_from_ast_type(builder, ast_type->static_array.element_type);

            if (elem_type->isFunctionTy()) {
                elem_type = elem_type->getPointerTo();
            }
            return llvm::ArrayType::get(elem_type, ast_type->static_array.count);
        }

        case Type_Kind::ENUM: {
            return llvm_type_from_ast_type(builder, ast_type->enumeration.integer_type);
        }

        case Type_Kind::SLICE: {
            return llvm_type_from_ast_type(builder, ast_type->slice.struct_type);
        }
    }

    assert(false);
    ZFATAL("[llvm_builder] Unhandled case in llvm_type_from_ast_type\n");
    return nullptr;
}

llvm::BasicBlock *get_llvm_block(LLVM_Builder *builder, Array_Ref<LLVM_Block> blocks, Bytecode_Block_Handle handle)
{
    assert(handle >= 0 && handle < blocks.count);

    if (blocks[handle].bc_block_handle == handle) return blocks[handle].llvm_block;

    for (s64 i = 0; i < blocks.count; i++) {
        if (blocks[i].bc_block_handle == handle) return blocks[i].llvm_block;
    }

    assert_msg(false, "llvm_get_block: Unable to find block");
    return nullptr;
}

llvm::Function *llvm_get_intrinsic(LLVM_Builder *builder, Type *fn_type, const char *name)
{
    assert(fn_type->kind == Type_Kind::FUNCTION);

    auto llvm_func_type = llvm::dyn_cast<llvm::FunctionType>(llvm_type_from_ast_type(builder, fn_type));
    assert(llvm_func_type);

    llvm::Function *result = builder->llvm_module->getFunction(name);
    if (result) {
        assert(result->getFunctionType() == llvm_func_type);
        return result;
    }

    result = llvm::Function::Create(llvm_func_type, llvm::GlobalValue::ExternalLinkage, name, builder->llvm_module);
    assert(result);
    return result;
}

llvm::Constant *llvm_type_info_array_pointer(LLVM_Builder *builder)
{
    auto &type_infos = builder->zodiac_context->type_infos;

    llvm::Type *type_info_type = llvm_type_from_ast_type(builder, get_type_info_type(builder->zodiac_context));
    llvm::Type *type_info_pointer_type = type_info_type->getPointerTo();


    Dynamic_Array<llvm::Constant *> llvm_infos;
    dynamic_array_create(builder->allocator, &llvm_infos, type_infos.count);

    for (s64 i = 0; i < type_infos.count; i++) {

        auto ti = type_infos[i];
        auto type = builder->zodiac_context->type_info_types[i];

        // Create a constant for the type info
        llvm::Constant *type_info_value = llvm_emit_type_info(builder, ti, type);
        dynamic_array_append(&llvm_infos, type_info_value);
    }

    auto array_type = llvm::ArrayType::get(type_info_pointer_type, type_infos.count);
    llvm::Constant *type_info_array_value = llvm::ConstantArray::get(array_type, { llvm_infos.data, (size_t)llvm_infos.count });

    llvm::GlobalVariable *array_glob_var = new llvm::GlobalVariable(*builder->llvm_module, type_info_array_value->getType(), true, llvm::GlobalValue::PrivateLinkage, type_info_array_value, "type_info_array");

    return static_cast<llvm::Constant *>(array_glob_var);
}

llvm::Constant *llvm_emit_type_info(LLVM_Builder *builder, Type_Info *ti, Type *type)
{
    llvm::Constant *result = nullptr;
    bool found = hash_table_find(&builder->type_infos, ti, &result);
    if (found) return result;

    auto base = llvm_emit_type_info_base(builder, ti, type);

    switch (ti->kind) {
        case Type_Info_Kind::INVALID: assert(false); break;

        case Type_Info_Kind::VOID:
        case Type_Info_Kind::REAL:
        case Type_Info_Kind::BOOL: {
            result = base;
            break;
        }

        case Type_Info_Kind::INTEGER: {

            auto ii = (Type_Info_Int *)ti;

            auto zodiac_type_info_int = get_type_info_int_type(builder->zodiac_context);
            auto type_info_int_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, zodiac_type_info_int));
            auto bool_type = zodiac_type_info_int->structure.member_types[1];

            llvm::Constant *members[2] = {
                base,
                llvm_builder_emit_bool_literal(builder, bool_type, ii->sign)
            };

            result =  llvm::ConstantStruct::get(type_info_int_type, members);
            break;
        }

        case Type_Info_Kind::POINTER: {

            auto pi = (Type_Info_Pointer *)ti;

            auto type_info_pointer_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, get_type_info_pointer_type(builder->zodiac_context)));

            llvm::Constant *members[2] = {
                base,
                llvm_emit_type_info(builder, pi->pointer_to, type->pointer.base)
            };
            assert(pi);

            result = llvm::ConstantStruct::get(type_info_pointer_type, members);
            break;
        }

        case Type_Info_Kind::STRUCT: {

            auto ast_struct_info_type = get_type_info_struct_type(builder->zodiac_context);
            auto ast_struct_member_info_type = get_type_info_struct_member_type(builder->zodiac_context);
            auto type_info_struct_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_struct_info_type));
            auto type_info_struct_member_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_struct_member_info_type));

            llvm::GlobalVariable *glob_var = new llvm::GlobalVariable(*builder->llvm_module, type_info_struct_type, true, llvm::GlobalValue::PrivateLinkage, nullptr, "type_info");
            hash_table_add(&builder->type_infos, ti, static_cast<llvm::Constant *>(glob_var));

            auto si = (Type_Info_Struct *)ti;

            auto name = llvm_builder_emit_string_literal(builder, si->name);

            auto members_array_type = llvm::ArrayType::get(type_info_struct_member_type, si->member_count);

            auto members = temp_array_create<llvm::Constant *>(temp_allocator_allocator(), si->member_count);
            defer { temp_array_destroy(&members); };
            members.array.count = si->member_count;

            auto llvm_type = llvm_type_from_ast_type(builder, type);
            auto layout = builder->llvm_datalayout->getStructLayout(static_cast<llvm::StructType *>(llvm_type));

            for (s64 i = 0; i < si->member_count; i++) {
                auto member_name = llvm_builder_emit_string_literal(builder, String_Ref(si->members[i].name));

                auto offset = (s64)layout->getElementOffset(i);

                assert(offset == si->members[i].offset);

                auto member_type = type->structure.member_types[i];

                llvm::Constant *member_info_members[3] = {
                    member_name,
                    llvm_emit_type_info(builder, si->members[i].type, member_type),
                    llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { offset })
                };

                members[i] = llvm::ConstantStruct::get(type_info_struct_member_type, member_info_members);
            }

            auto members_array = llvm::ConstantArray::get(members_array_type, { members.array.data, (size_t)si->member_count } );
            members_array = static_cast<llvm::Constant *>(new llvm::GlobalVariable(*builder->llvm_module, members_array->getType(), true, llvm::GlobalValue::PrivateLinkage, members_array, "struct_member_type_infos"));

            auto members_slice_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_struct_info_type->structure.member_types[2]));

            llvm::Constant *slice_members[2] = {
                members_array,
                llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { .s64 = si->member_count }),
            };

            auto members_slice = llvm::ConstantStruct::get(members_slice_type, slice_members);

            llvm::Constant *result_members[3] = {
                base,
                name,
                members_slice,
            };

            result = llvm::ConstantStruct::get(type_info_struct_type, result_members);
            glob_var->setInitializer(result);
            result = glob_var;
            break;
        }

        case Type_Info_Kind::ENUM: {

            auto ast_type_info_enum_type = get_type_info_enum_type(builder->zodiac_context);
            auto type_info_enum_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_type_info_enum_type));

            auto ast_type_info_enum_member_type = get_type_info_enum_member_type(builder->zodiac_context);
            auto type_info_enum_member_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_type_info_enum_member_type));

            auto members_slice_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_type_info_enum_type->structure.member_types[3]));

            auto ei = (Type_Info_Enum *)ti;

            auto name = llvm_builder_emit_string_literal(builder, ei->name);
            auto integer_type = llvm_emit_type_info(builder, ei->integer_type, type->enumeration.integer_type);

            auto members_array_type = llvm::ArrayType::get(type_info_enum_member_type, ei->member_count);

            auto members = temp_array_create<llvm::Constant *>(temp_allocator_allocator(), ei->member_count);
            defer { temp_array_destroy(&members); };
            members.array.count = ei->member_count;

            for (s64 i = 0; i < ei->member_count; i++) {
                auto member_name = llvm_builder_emit_string_literal(builder, String_Ref(ei->members[i].name));

                llvm::Constant *member_info_members[2] = {
                    member_name,
                    llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { .s64 = ei->members[i].value })
                };

                members[i] = llvm::ConstantStruct::get(type_info_enum_member_type, member_info_members);
            }

            auto members_array = llvm::ConstantArray::get(members_array_type, { members.array.data, (size_t)ei->member_count });
            members_array = static_cast<llvm::Constant *>(new llvm::GlobalVariable(*builder->llvm_module, members_array->getType(), true, llvm::GlobalValue::PrivateLinkage, members_array, "enum_member_type_infos"));

            llvm::Constant *slice_members[2] = {
                members_array,
                llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { .s64 = ei->member_count })
            };

            auto members_slice = llvm::ConstantStruct::get(members_slice_type, slice_members);

            llvm::Constant *result_members[4] = {
                base,
                name,
                integer_type,
                members_slice,
            };

            result = llvm::ConstantStruct::get(type_info_enum_type, result_members);
            break;
        }
        case Type_Info_Kind::STATIC_ARRAY: {

            auto ast_sa_type = get_type_info_static_array_type(builder->zodiac_context);
            auto type_info_static_array_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_sa_type));

            auto sai = (Type_Info_Static_Array *)ti;

            llvm::Constant *result_members[3] = {
                base,
                llvm_emit_type_info(builder, sai->element_type, type->static_array.element_type),
                llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { .s64 = sai->length })
            };

            result = llvm::ConstantStruct::get(type_info_static_array_type, result_members);
            break;
        }
        case Type_Info_Kind::SLICE: {

            auto ast_slice_info_type = get_type_info_slice_type(builder->zodiac_context);
            auto type_info_slice_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_slice_info_type));

            auto si = (Type_Info_Slice *)ti;

            llvm::Constant *result_members[2] = {
                base,
                llvm_emit_type_info(builder, si->element_type, type->slice.element_type)
            };

            result = llvm::ConstantStruct::get(type_info_slice_type, result_members);
            break;
        }

        case Type_Info_Kind::FUNCTION: {

            auto ast_ti_type = get_type_info_type(builder->zodiac_context);
            auto type_info_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_ti_type));
            auto ast_func_ti_type = get_type_info_function_type(builder->zodiac_context);
            auto type_info_func_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_func_ti_type));

            auto params_slice_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, ast_func_ti_type->structure.member_types[1]));

            auto fi = (Type_Info_Function *)ti;

            llvm::Constant *members_array = nullptr;
            if (fi->param_count) {
                auto members_array_type = llvm::ArrayType::get(type_info_type->getPointerTo(), fi->param_count);

                auto members = temp_array_create<llvm::Constant *>(temp_allocator_allocator(), fi->param_count);
                defer { temp_array_destroy(&members); };

                members.array.count = fi->param_count;

                for (s64 i = 0; i < fi->param_count; i++) {
                    members[i] = llvm_emit_type_info(builder, fi->param_types[i], type->function.parameter_types[i]);
                }

                members_array = llvm::ConstantArray::get(members_array_type, { members.array.data, (size_t)fi->param_count });
                members_array = new llvm::GlobalVariable(*builder->llvm_module, members_array->getType(), true, llvm::GlobalValue::PrivateLinkage, members_array, "function_parameter_type_infos");
            } else {
                members_array = llvm::Constant::getNullValue(type_info_type->getPointerTo()->getPointerTo());
            }

            llvm::Constant *params_slice_members[2] = {
                members_array,
                llvm_builder_emit_integer_literal(builder, &builtin_type_s64, { .s64 = fi->param_count })
            };

            auto parameters_slice = llvm::ConstantStruct::get(params_slice_type, params_slice_members);

            llvm::Constant *result_members[3] = {
                base,
                parameters_slice,
                llvm_emit_type_info(builder, fi->return_type, type->function.return_type),
            };

            result = llvm::ConstantStruct::get(type_info_func_type, result_members);
            break;
        }
    }

    assert(result);

    if (ti->kind != Type_Info_Kind::STRUCT) {
        llvm::GlobalVariable *glob_var = new llvm::GlobalVariable(*builder->llvm_module, result->getType(), true, llvm::GlobalValue::PrivateLinkage, result, "type_info");
        hash_table_add(&builder->type_infos, ti, static_cast<llvm::Constant *>(glob_var));
        return glob_var;
    }

    return result;

}

llvm::Constant *llvm_emit_type_info_base(LLVM_Builder *builder, Type_Info *ti, Type *type)
{
    auto type_info_type = static_cast<llvm::StructType *>(llvm_type_from_ast_type(builder, get_type_info_type(builder->zodiac_context)));
    auto type_info_kind_type = static_cast<llvm::IntegerType *>(llvm_type_from_ast_type(builder, get_type_info_kind_type(builder->zodiac_context)));
    auto size_type = static_cast<llvm::IntegerType *>(llvm_type_from_ast_type(builder, &builtin_type_s64));

    auto llvm_type = llvm_type_from_ast_type(builder, type);

    s64 size;

    if (type->kind == Type_Kind::VOID) {
        size = 0;
    } else if (type->kind == Type_Kind::FUNCTION) {
        size = builder->llvm_datalayout->getTypeAllocSize(llvm_type->getPointerTo());
    } else {
        size = builder->llvm_datalayout->getTypeAllocSize(llvm_type);
    }

    assert(size == ti->byte_size);

    llvm::Constant *members[2] = {
        llvm::ConstantInt::get(type_info_kind_type, (s64)ti->kind),
        llvm::ConstantInt::get(size_type, size)
    };

    return llvm::ConstantStruct::get(type_info_type, members);
}

void llvm_builder_emit_binary(LLVM_Builder *builder)
{
    bool verify_error = llvm::verifyModule(*builder->llvm_module, &llvm::errs());

    if (verify_error) {
        assert(false);
    }

    builder->llvm_module->setTargetTriple(builder->target_triple.data);
    std::string error;
    const llvm::Target *llvm_target = llvm::TargetRegistry::lookupTarget(builder->target_triple.data, error);
    assert(llvm_target);


    auto cpu = "generic";
    auto features = "";
    llvm::TargetOptions opt;
    auto reloc_model = llvm::Optional<llvm::Reloc::Model>();
    //auto reloc_model = llvm::Reloc::PIC_;

    llvm::TargetMachine *llvm_target_machine = llvm_target->createTargetMachine(builder->target_triple.data, cpu, features, opt, reloc_model);
    builder->llvm_module->setDataLayout(llvm_target_machine->createDataLayout());

    String_Builder _sb;
    String_Builder* sb = &_sb;
    string_builder_create(sb, temp_allocator_allocator());

    String_Ref ext;
    switch (builder->target_platform) {
        case LLVM_Target_Platform::INVALID: assert(false); break;
        case LLVM_Target_Platform::WINDOWS: ext = "obj"; break;
        case LLVM_Target_Platform::LINUX: ext  = "o"; break;
    }
    string_builder_append(sb, "% .*s.%.*s", (int)builder->out_file_name.length, builder->out_file_name.data,
                           (int)ext.length, ext.data);

    String obj_file_name = string_builder_to_string(sb);

    std::error_code err_code;
    llvm::raw_fd_ostream dest(obj_file_name.data, err_code, llvm::sys::fs::OF_None);
    if (err_code) {
        ZFATAL("Could not open file: %s\n", obj_file_name.data);
        assert(false);
    }

    llvm::legacy::PassManager pass;
    auto filetype = llvm::CGFT_ObjectFile;
    if (llvm_target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype)) {
        ZFATAL("TargetMachine can't emit file of this type...");
        assert(false);
    }

    pass.run(*builder->llvm_module);

    delete llvm_target_machine;

    dest.close();

    bool linker_result = llvm_builder_run_linker(builder);

    filesystem_remove(obj_file_name);

    assert(linker_result);

    if (!linker_result) {
        ZFATAL("Linking failed...");
    }
}

bool llvm_builder_run_linker(LLVM_Builder *builder)
{
    assert(builder);

    auto out_name = builder->out_file_name;
    assert(out_name.length && out_name.data);

    auto ta = temp_allocator_allocator();
    auto mark = temporary_allocator_get_mark((Temporary_Allocator *)ta->user_data);

    auto command_line = temp_array_create<String_Ref>(ta);

#define APPEND_COMMAND(x) dynamic_array_append(&command_line.array, String_Ref(x))
#define APPEND_COMMANDF(fmt, ...) APPEND_COMMAND(string_format(ta, (fmt), ##__VA_ARGS__));

#if ZPLATFORM_LINUX

    auto pi = builder->platform_info;
    assert(!pi.err);

    APPEND_COMMAND("ld");
    APPEND_COMMANDF("--dynamic-linker");
    APPEND_COMMAND(pi.dynamic_linker_path.data);

    APPEND_COMMANDF("%scrt1.o", pi.crt_path.data);
    APPEND_COMMANDF("%scrti.o", pi.crt_path.data);

    APPEND_COMMAND("-o");
    APPEND_COMMAND(out_name.data);

    APPEND_COMMAND("-lc");
    APPEND_COMMANDF("%.*s.o", (int)out_name.length, out_name.data);

    APPEND_COMMAND("-R");
    APPEND_COMMAND(builder->zodiac_context->compiler_exe_dir.data);

    APPEND_COMMAND("-L");
    APPEND_COMMAND(builder->zodiac_context->compiler_exe_dir.data);

    APPEND_COMMAND("-lzrs_s");

    APPEND_COMMANDF("%scrtn.o", pi.crt_path.data);

#elif ZPLATFORM_WINDOWS

    auto &sdk_info = builder->platform_info.sdk_info;
    auto vs_exe_path = Wide_String_Ref(sdk_info.vs_exe_path);

    auto wide_linker_path = string_append(ta, vs_exe_path, L"\\link.exe");
    auto linker_path = string_create(ta, wide_linker_path.data);

    auto um_lib_path = string_create(ta, sdk_info.windows_sdk_um_library_path);
    auto ucrt_lib_path = string_create(ta, sdk_info.windows_sdk_ucrt_library_path);
    auto vs_lib_path = string_create(ta, sdk_info.vs_library_path);

    String_Ref zrs_s_lib_path = builder->zodiac_context->support_lib_static_path;
    // String_Ref zrs_lib_path = builder->zodiac_context->support_lib_dynamic_path;

    APPEND_COMMANDF("\"%.*s\"", (int)linker_path.length, linker_path.data);

    APPEND_COMMAND("/nologo /wx /subsystem:CONSOLE /nodefaultlib /noimplib /noexp");

    APPEND_COMMANDF("/libpath:\"%.*s\"", (int)um_lib_path.length, um_lib_path.data);
    APPEND_COMMANDF("/libpath:\"%.*s\"", (int)ucrt_lib_path.length, ucrt_lib_path.data);
    APPEND_COMMANDF("/libpath:\"%.*s\"", (int)vs_lib_path.length, vs_lib_path.data);

    APPEND_COMMAND("kernel32.lib");
    APPEND_COMMAND("ucrt.lib");
    APPEND_COMMAND("msvcrt.lib");

    APPEND_COMMANDF(" %s.obj", builder->out_file_name.data);

    APPEND_COMMANDF(" /out:\"%.*s\"", (int)out_name.length, out_name.data);

    bool link_c = true;
    if (link_c) {
        APPEND_COMMAND("libvcruntime.lib");
        APPEND_COMMAND("legacy_stdio_definitions.lib");
        APPEND_COMMAND("legacy_stdio_wide_specifiers.lib");
    }

    APPEND_COMMANDF(" %.*s", (int)zrs_s_lib_path.length, zrs_s_lib_path.data);
    // APPEND_COMMANDF(" %.*s", (int)zrs_lib_path.length, zrs_lib_path.data);


#else
    assert(false);
    ZFATAL("[llvm_builder] Trying to run linker on unsupported platform!");
#endif

    if (builder->zodiac_context->options.verbose) {
        String link_cmd = string_append(ta, command_line.array, " ");
        ZTRACE("Running linker: %.*s", (int)link_cmd.length, link_cmd.data);
    }

    auto _command_line = Array_Ref<String_Ref>(command_line.array);
    Process_Result proc_res = platform_execute_process(&_command_line);

    temporary_allocator_reset((Temporary_Allocator *)ta->user_data, mark);

    bool result = true;

    if (!proc_res.success) {

        ZERROR("Link command failed with exit code: %d", proc_res.exit_code);

        if (proc_res.error_string.length) ZERROR("Linker error: %.*s", (int)proc_res.error_string.length, proc_res.error_string.data);
        if (proc_res.result_string.length) ZERROR("Linker output: %.*s", (int)proc_res.result_string.length, proc_res.result_string.data);

        result = false;
    }

    platform_free_process_result(&proc_res);

#undef APPEND_COMMAND
#undef APPEND_COMMANDF

    return result;
}

void llvm_builder_print(LLVM_Builder *builder)
{
    builder->llvm_module->print(llvm::outs(), nullptr, false, true);
}

} }
