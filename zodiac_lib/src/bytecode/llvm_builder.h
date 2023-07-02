#pragma once

#include "bytecode/bytecode.h"
#include "containers/hash_table.h"
#include "containers/stack.h"
#include "memory/allocator.h"
#include "util/zstring.h"

#define zodiac_disable_msvc_llvm_warnings() \
__pragma(warning(push)) \
__pragma(warning(disable:4100)) \
__pragma(warning(disable:4127)) \
__pragma(warning(disable:4244)) \
__pragma(warning(disable:4245)) \
__pragma(warning(disable:4267)) \
__pragma(warning(disable:4310)) \
__pragma(warning(disable:4324)) \
__pragma(warning(disable:4458)) \
__pragma(warning(disable:4624)) \
__pragma(warning(disable:4996)) \
__pragma(warning(disable:6011)) \
                                 \
__pragma(warning(disable:26800)) \
__pragma(warning(disable:26812)) \
__pragma(warning(disable:26495)) \
__pragma(warning(disable:26451)) \
__pragma(warning(disable:26450)) \
__pragma(warning(disable:26819)) \
__pragma(warning(disable:26819)) \
__pragma(warning(disable:26439)) \

#ifdef _MSC_VER
zodiac_disable_msvc_llvm_warnings()
#endif // _MSC_VER

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

#ifdef _MSC_VER
#pragma warning(pop)
#endif // _MSC_VER

#ifdef _WIN32
#include <microsoft_craziness.h>
#endif

namespace Zodiac { namespace Bytecode {

enum class LLVM_Target_Platform
{
    INVALID,
    LINUX,
    WINDOWS,
};

struct LLVM_Builder;
void llvm_builder_init(LLVM_Builder *builder, Allocator *allocator, Bytecode_Builder *bytecode_builder);

struct LLVM_Builder
{
    Allocator *allocator = nullptr;
    Bytecode_Builder *bytecode_builder = nullptr;
    Zodiac_Context *zodiac_context = nullptr;

    LLVM_Target_Platform target_platform = LLVM_Target_Platform::INVALID;
    llvm::LLVMContext *llvm_context = nullptr;
    llvm::Module *llvm_module = nullptr;
    llvm::IRBuilder<> *ir_builder = nullptr;
    String target_triple = {};

#if _WIN32
    Windows_SDK_Info sdk_info = {};
#endif

    String_Ref out_file_name = {};

    Hash_Table<Bytecode_Function_Handle, llvm::Function*> functions = {};
    Hash_Table<Type *, llvm::StructType *> struct_types = {};
    Hash_Table<Atom, llvm::Constant *> string_literals = {};

    llvm::Function *current_function = nullptr;
    Bytecode_Function *current_bytecode_function = nullptr;
    Hash_Table<s64, llvm::Value *> stored_registers = {}; // Result registers/temps for current_function
    Hash_Table<s64, llvm::GlobalVariable *> globals = {};
    Stack<llvm::Value *> arg_stack = {};

    LLVM_Builder() {}

    LLVM_Builder(Allocator *allocator, Bytecode_Builder *bc_builder) {
        llvm_builder_init(this, allocator, bc_builder);
    }
};

LLVM_Builder llvm_builder_create(Allocator *allocator, Bytecode_Builder *bytecode_builder);
void llvm_builder_free(LLVM_Builder *builder);

void llvm_builder_emit_global(LLVM_Builder *builder, Bytecode_Global_Handle glob_handle);
void llvm_builder_register_function(LLVM_Builder *builder, Bytecode_Function_Handle fn_handle);
bool llvm_builder_emit_function(LLVM_Builder *builder, Bytecode_Function_Handle fn_handle);

bool llvm_builder_emit_instruction(LLVM_Builder *builder, const Bytecode_Instruction &bc_inst);

llvm::Value *llvm_builder_emit_register(LLVM_Builder *builder, const Bytecode_Register &bc_reg);
llvm::Constant *llvm_builder_emit_constant(LLVM_Builder *builder, const Bytecode_Register &bc_reg);
llvm::Constant *llvm_builder_emit_integer_literal(LLVM_Builder *builder, Type *type, Integer_Value integer);
llvm::Constant *llvm_builder_emit_float_literal(LLVM_Builder *builder, Type *type, Real_Value real);
llvm::Constant *llvm_builder_emit_bool_literal(LLVM_Builder *builder, Type *type, bool value);
llvm::Constant *llvm_builder_emit_string_literal(LLVM_Builder *builder, String_Ref str);
void llvm_builder_store_result(LLVM_Builder *builder, const Bytecode_Register &bc_dest_reg, llvm::Value *result_val);

llvm::Type *llvm_type_from_ast_type(LLVM_Builder *builder, Type *ast_type);

llvm::Function *llvm_get_intrinsic(LLVM_Builder *builder, Type *fn_type, const char *name);

void llvm_builder_emit_binary(LLVM_Builder *builder);
bool llvm_builder_run_linker(LLVM_Builder *builder);

void llvm_builder_print(LLVM_Builder *builder);

} }
