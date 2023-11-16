#pragma once

#include "bytecode/bytecode.h"
#include "bytecode/interpreter.h"
#include "containers/dynamic_array.h"
#include "containers/hash_table.h"
#include "containers/stack.h"
#include "defines.h"

namespace Zodiac {

struct Allocator;
struct AST_Declaration;
struct AST_Directive;
struct AST_Expression;
struct AST_Statement;
struct File_Handle;
struct Resolver;
struct Scope;
struct Source_Range;
struct Type;
struct Zodiac_Context;

namespace Bytecode {

struct Bytecode_Switch_Case_Info
{
    AST_Statement *switch_stmt;
    s64 current_case_index;

    Array_Ref<Bytecode_Block_Handle> case_blocks;
    Bytecode_Block_Handle post_block;
};

struct Break_Block
{
    AST_Statement *stmt;
    Bytecode_Block_Handle block_handle;
};

struct Bytecode_Converter
{
    Allocator *allocator;
    Zodiac_Context *context;
    Bytecode_Builder *builder;

    Hash_Table<AST_Declaration *, Bytecode_Function_Handle> functions;
    Stack<AST_Statement *> defer_stack;

    Stack<Bytecode_Switch_Case_Info> switch_case_stack;

    // TODO: should these be separated per function?
    Hash_Table<AST_Declaration *, Bytecode_Register> allocations;

    // TODO: should these be separated per function?
    Hash_Table<AST_Expression *, Bytecode_Register> implicit_lvalues;

    // TODO: should these be separated per file?
    Hash_Table<AST_Declaration *, Bytecode_Global_Handle> globals;

    Stack<Break_Block> break_blocks;
    Stack<Break_Block> continue_blocks;

    s64 run_directive_count; // Used to generate unique names
    Hash_Table<AST_Directive *, Bytecode_Function_Handle> run_directives;
    Hash_Table<AST_Directive *, Bytecode_Register> run_results;
};

struct Run_Wrapper_Result
{
    Allocator *allocator;
    Interpreter *interpreter;
    Interpreter_Register value;
};

ZAPI Bytecode_Converter bytecode_converter_create(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb);
ZAPI void bytecode_converter_init(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb, Bytecode_Converter *out_bc);
ZAPI void bytecode_converter_destroy(Bytecode_Converter *bc);

ZAPI bool emit_bytecode(Resolver *resolver, Bytecode_Converter *bc);
ZAPI bool ast_decl_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl);
ZAPI void ast_function_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl);
ZAPI bool ast_stmt_to_bytecode(Bytecode_Converter *bc, AST_Statement *stmt);

ZAPI Bytecode_Register ast_lvalue_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr);
ZAPI Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type * enforce_type = nullptr);
ZAPI Bytecode_Register ast_compound_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *compound_expr);

ZAPI int emit_varargs(Bytecode_Converter *bc, AST_Expression *call_expr, s64 va_start_index);

ZAPI Bytecode_Register ast_const_lvalue_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr);
ZAPI Bytecode_Register ast_const_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type *enforce_type = nullptr);
ZAPI Bytecode_Register ast_const_compound_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *compound_expr);

ZAPI void assignment_to_bytecode(Bytecode_Converter *bc, AST_Expression *value_expr, Bytecode_Register lvalue_reg);

ZAPI Break_Block find_break_block(Bytecode_Converter *bc, AST_Statement *break_from);
ZAPI Break_Block find_continue_block(Bytecode_Converter *bc, AST_Statement *break_from);

ZAPI Bytecode_Register emit_type_info(Bytecode_Converter *bc, Type *target_type);

ZAPI Bytecode_Register emit_any(Bytecode_Converter *bc, AST_Expression *expr);

ZAPI Bytecode_Function_Handle create_run_wrapper(Bytecode_Converter *bc, AST_Directive *run_directive);

ZAPI Run_Wrapper_Result execute_run_wrapper(Bytecode_Converter *bc, Bytecode_Function_Handle fn_handle);
ZAPI Run_Wrapper_Result execute_run_wrapper(Bytecode_Converter *bc, Bytecode_Function_Handle fn_handle, File_Handle stdout_file);

ZAPI AST_Expression *interpreter_register_to_ast_expression(Bytecode_Converter *bc, Interpreter_Register &reg, Scope *scope, Source_Range range);
ZAPI AST_Expression *interpreter_memory_to_ast_expression(Bytecode_Converter *bc, u8* mem, Type *type, Scope *scope, Source_Range range);

} }
