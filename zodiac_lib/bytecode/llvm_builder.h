#pragma once

#include "bytecode/bytecode.h"
#include "containers/dynamic_array.h"
#include "containers/hash_table.h"
#include "containers/stack.h"
#include "defines.h"
#include "platform/platform.h"
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

//  llvm includes should go here

#ifdef _MSC_VER
#pragma warning(pop)
#endif // _MSC_VER

// #ifdef ZPLATFORM_WINDOWS
// #include "platform/microsoft_craziness.h"
// #endif

namespace llvm {

class BasicBlock;
class Constant;
class Function;
class GlobalVariable;
class LLVMContext;
class Module;
class StructType;
class Type;
class Value;

class ConstantFolder;
class IRBuilderDefaultInserter;

template <typename FolderTy,
          typename InserterTy>
class IRBuilder;

}

namespace Zodiac {

struct Allocator;
struct Atom;
struct Real_Value;
struct Type;
struct Type_Info;
struct Zodiac_Context;
union Integer_Value;

namespace Bytecode {

struct LLVM_Builder;

enum class LLVM_Target_Platform
{
    INVALID,
    LINUX,
    WINDOWS,
};

ZAPI void llvm_builder_init(LLVM_Builder *builder, Allocator *allocator, Bytecode_Builder *bytecode_builder);

struct LLVM_Block
{
    Bytecode_Block_Handle bc_block_handle;
    llvm::BasicBlock *llvm_block;
};

struct LLVM_Builder
{
    Allocator *allocator = nullptr;
    Bytecode_Builder *bytecode_builder = nullptr;
    Zodiac_Context *zodiac_context = nullptr;

    LLVM_Target_Platform target_platform = LLVM_Target_Platform::INVALID;
    llvm::LLVMContext *llvm_context = nullptr;
    llvm::Module *llvm_module = nullptr;
    llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> *ir_builder = nullptr;
    String target_triple = {};

    Platform_Info platform_info = {};

    String_Ref out_file_name = {};

    Hash_Table<Bytecode_Function_Handle, llvm::Function*> functions = {};
    Hash_Table<Type *, llvm::StructType *> struct_types = {};
    Hash_Table<Atom, llvm::Constant *> string_literals = {};

    llvm::Function *current_function = nullptr;
    Bytecode_Function *current_bytecode_function = nullptr;
    LLVM_Block current_block = {};

    Hash_Table<s64, llvm::Value *> stored_registers = {}; // Result registers/temps for current_function
    Hash_Table<s64, llvm::GlobalVariable *> globals = {};
    Stack<llvm::Value *> arg_stack = {};

    LLVM_Builder() {}

    LLVM_Builder(Allocator *allocator, Bytecode_Builder *bc_builder) {
        llvm_builder_init(this, allocator, bc_builder);
    }
};

ZAPI LLVM_Builder llvm_builder_create(Allocator *allocator, Bytecode_Builder *bytecode_builder);
ZAPI void llvm_builder_free(LLVM_Builder *builder);

ZAPI void llvm_builder_emit_program(LLVM_Builder *builder, Bytecode_Program *program);
ZAPI void llvm_builder_emit_global(LLVM_Builder *builder, Bytecode_Global_Handle glob_handle);
ZAPI void llvm_builder_register_function(LLVM_Builder *builder, Bytecode_Function_Handle fn_handle);
ZAPI bool llvm_builder_emit_function(LLVM_Builder *builder, Bytecode_Function_Handle fn_handle);

ZAPI bool llvm_builder_emit_instruction(LLVM_Builder *builder, const Bytecode_Instruction &bc_inst, Array_Ref<LLVM_Block> block_indices);

ZAPI void llvm_builder_emit_print_instruction(LLVM_Builder *builder, Array_Ref<LLVM_Block> blocks, Type *type, llvm::Value *llvm_val, bool quote_strings = false);

ZAPI llvm::Value *llvm_builder_emit_register(LLVM_Builder *builder, const Bytecode_Register &bc_reg);
ZAPI llvm::Constant *llvm_builder_emit_constant(LLVM_Builder *builder, const Bytecode_Register &bc_reg);
ZAPI llvm::Constant *llvm_builder_emit_integer_literal(LLVM_Builder *builder, Type *type, Integer_Value integer);
ZAPI llvm::Constant *llvm_builder_emit_float_literal(LLVM_Builder *builder, Type *type, Real_Value real);
ZAPI llvm::Constant *llvm_builder_emit_bool_literal(LLVM_Builder *builder, Type *type, bool value);
ZAPI llvm::Constant *llvm_builder_emit_string_literal(LLVM_Builder *builder, String_Ref str);
ZAPI llvm::Constant *llvm_builder_emit_cstring_literal(LLVM_Builder *builder, String_Ref str);
ZAPI llvm::Constant *llvm_builder_emit_struct_literal(LLVM_Builder *builder, Type *type, Dynamic_Array<Bytecode_Register> compound);
ZAPI llvm::Constant *llvm_builder_emit_array_literal(LLVM_Builder *builder, Type *type, Dynamic_Array<Bytecode_Register> compound);
ZAPI void llvm_builder_store_result(LLVM_Builder *builder, const Bytecode_Register &bc_dest_reg, llvm::Value *result_val);

ZAPI llvm::Type *llvm_type_from_ast_type(LLVM_Builder *builder, Type *ast_type);

ZAPI llvm::BasicBlock *get_llvm_block(LLVM_Builder *builder, Array_Ref<LLVM_Block> blocks, Bytecode_Block_Handle handle);

ZAPI llvm::Function *llvm_get_intrinsic(LLVM_Builder *builder, Type *fn_type, const char *name);

ZAPI llvm::Constant *llvm_type_info_array_pointer(LLVM_Builder *builder);
ZAPI llvm::Constant *llvm_emit_type_info(LLVM_Builder *builder, Type_Info *ti);
ZAPI llvm::Constant *llvm_emit_type_info_base(LLVM_Builder *builder, Type_Info *ti);

ZAPI void llvm_builder_emit_binary(LLVM_Builder *builder);
ZAPI bool llvm_builder_run_linker(LLVM_Builder *builder);

ZAPI void llvm_builder_print(LLVM_Builder *builder);

} }
