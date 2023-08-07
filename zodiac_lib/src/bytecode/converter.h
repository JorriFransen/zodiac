#pragma once

#include "defines.h"
#include "containers/hash_table.h"
#include "bytecode/bytecode.h"

namespace Zodiac {

struct Allocator;
struct AST_Declaration;
struct AST_Expression;
struct AST_Statement;
struct Resolver;
struct Zodiac_Context;

namespace Bytecode {

struct Bytecode_Converter
{
    Allocator *allocator;
    Zodiac_Context *context;
    Bytecode_Builder *builder;

    Hash_Table<AST_Declaration *, Bytecode_Function_Handle> functions;

    // TODO: should these be separated per function?
    Hash_Table<AST_Declaration *, Bytecode_Register> allocations;

    // TODO: should these be separated per file?
    Hash_Table<AST_Declaration *, Bytecode_Global_Handle> globals;

    s64 run_directive_count; // Used to generate unique names
};

ZAPI Bytecode_Converter bytecode_converter_create(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb);
ZAPI void bytecode_converter_destroy(Bytecode_Converter *bc);

ZAPI void emit_bytecode(Resolver *resolver, Bytecode_Converter *bc);
ZAPI void ast_decl_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl);
ZAPI void ast_function_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl);
ZAPI void ast_stmt_to_bytecode(Bytecode_Converter *bc, AST_Statement *stmt);

ZAPI Bytecode_Register ast_lvalue_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr);
ZAPI Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr);

ZAPI Bytecode_Register ast_const_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr);

} }
