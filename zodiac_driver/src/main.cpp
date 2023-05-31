#include "ast.h"
#include "bytecode/bytecode.h"
#include "bytecode/printer.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

#include <stdio.h>

using namespace Zodiac;
using namespace Bytecode;

void flat_resolve_test(Zodiac_Context *ctx, AST_File *file);

void ast_file_to_bytecode(Bytecode_Builder *bb, AST_File *file);
void ast_decl_to_bytecode(Bytecode_Builder *bb, AST_Declaration *decl);
void ast_function_to_bytecode(Bytecode_Builder *bb, AST_Declaration *decl);
void ast_stmt_to_bytecode(Bytecode_Builder *bb, AST_Statement *stmt);
Bytecode_Register ast_expr_to_bytecode(Bytecode_Builder *bb, AST_Expression *expr);

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;
    if (!Zodiac::type_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    Lexer lexer;
    lexer_create(&c, &lexer);
    String stream = {};

    auto filename = "tests/simple.zc";
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, filename, &stream);
    assert(read_result);
    if (!read_result) {
        return 1;
    }

    lexer_init_stream(&lexer, stream, filename);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    AST_File *file = parse_file(&parser);
    if (parser.error) return 1;;
    assert(file);

    flat_resolve_test(&c, file);

    for (u64 i = 0; i < c.errors.count; i++) {

        auto err = c.errors[i];

        if (!c.fatal_resolve_error) assert(!err.fatal);

        bool print = c.fatal_resolve_error == err.fatal;

        if (print) {
            auto start = err.source_range.start;
            printf("%s:%llu:%llu: error: %s\n", start.name.data, start.line, start.index_in_line, err.message.data);
        }
    }

    free(&dynamic_allocator, stream.data);

    // Assume this stage won't cause any errors atm
    // This bytecode builder should maybe be part of the context?
    Bytecode_Builder bb = bytecode_builder_create(&c.bytecode_allocator, &c);

    ast_file_to_bytecode(&bb, file);

    bytecode_print(&bb, temp_allocator_allocator());

    return 0;
}

#define add_builtin_type_symbol(type) { \
    auto sym = add_resolved_symbol(ctx, global_scope, Symbol_Kind::TYPE, (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), atom_##type, nullptr); \
    sym->builtin_type = &builtin_type_##type; \
}

void flat_resolve_test(Zodiac_Context *ctx, AST_File *file)
{
    assert(file);

    Scope *global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);

    Resolver resolver;
    resolver_create(&resolver, ctx, global_scope);

    add_builtin_type_symbol(u64);
    add_builtin_type_symbol(s64);
    add_builtin_type_symbol(u32);
    add_builtin_type_symbol(s32);
    add_builtin_type_symbol(u16);
    add_builtin_type_symbol(s16);
    add_builtin_type_symbol(u8);
    add_builtin_type_symbol(s8);

    add_builtin_type_symbol(r32);
    add_builtin_type_symbol(r64);

    add_builtin_type_symbol(String);

    for (u64 i = 0; i < file->declarations.count; i++) {
        resolver_add_declaration(ctx, &resolver, file->declarations[i]);
    }

    bool names_done = resolve_names(&resolver);

    bool types_done = false;
    if (names_done) types_done = resolve_types(&resolver);

    if ((!names_done) || (!types_done)) assert(ctx->errors.count == 0);
}


void ast_file_to_bytecode(Bytecode_Builder *bb, AST_File *file)
{
    assert(bb);
    assert(file);

    for (u64 i = 0 ; i < file->declarations.count; i++) {
        AST_Declaration *decl = file->declarations[i];

        ast_decl_to_bytecode(bb, decl);
    }
}

void ast_decl_to_bytecode(Bytecode_Builder *bb, AST_Declaration *decl)
{
    assert(bb);
    assert(decl);
    assert(decl->kind != AST_Declaration_Kind::INVALID);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;
        case AST_Declaration_Kind::VARIABLE: assert(false); break;
        case AST_Declaration_Kind::CONSTANT_VARIABLE: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {
            ast_function_to_bytecode(bb, decl);
            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false); break;
        case AST_Declaration_Kind::UNION: assert(false); break;
    }
}

void ast_function_to_bytecode(Bytecode_Builder *bb, AST_Declaration *decl)
{
    assert(bb);
    assert(decl);
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    assert(decl->function.type);
    assert(decl->function.type->kind == Type_Kind::FUNCTION);
    assert(decl->identifier.name.data);

    Bytecode_Function_Handle fn_handle = bytecode_function_create(bb, decl->identifier.name, decl->function.type);
    Bytecode_Block_Handle entry_block_handle = bytecode_append_block(bb, fn_handle, "entry");

    bytecode_set_insert_point(bb, fn_handle, entry_block_handle);

    for (u64 i = 0; i < decl->function.body.count; i++) {
        AST_Statement *stmt = decl->function.body[i];
        ast_stmt_to_bytecode(bb, stmt);
    }
}

void ast_stmt_to_bytecode(Bytecode_Builder *bb, AST_Statement *stmt)
{
    assert(bb);
    assert(stmt);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::BLOCK: assert(false); break;
        case AST_Statement_Kind::DECLARATION: assert(false); break;
        case AST_Statement_Kind::ASSIGN: assert(false); break;
        case AST_Statement_Kind::CALL: assert(false); break;
        case AST_Statement_Kind::IF: assert(false); break;
        case AST_Statement_Kind::WHILE: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                Bytecode_Register return_value_register = ast_expr_to_bytecode(bb, stmt->return_stmt.value);
                bytecode_emit_return(bb, return_value_register);
            } else {
                bytecode_emit_return(bb);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: assert(false); break;
    }
}

Bytecode_Register ast_expr_to_bytecode(Bytecode_Builder *bb, AST_Expression *expr)
{
    assert(bb);
    assert(expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            Type *literal_type = expr->resolved_type;
            if (literal_type->kind == Type_Kind::UNSIZED_INTEGER) {
                assert(false); // This should have been resolved to an integer type at this point.
            } else {
                assert(literal_type->kind == Type_Kind::INTEGER);
            }
            return bytecode_integer_literal(bb, literal_type, expr->integer_literal.value);
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER:
        {
            assert(false);
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;

        case AST_Expression_Kind::BINARY: {
            Bytecode_Register lhs_reg = ast_expr_to_bytecode(bb, expr->binary.lhs);
            Bytecode_Register rhs_reg = ast_expr_to_bytecode(bb, expr->binary.rhs);

            switch (expr->binary.op) {
                case AST_Binary_Operator::INVALID: assert(false); break;

                case AST_Binary_Operator::ADD: {
                    return bytecode_emit_add(bb, lhs_reg, rhs_reg);
                }

                case AST_Binary_Operator::SUB: assert(false); break;
                case AST_Binary_Operator::MUL: assert(false); break;
                case AST_Binary_Operator::DIV: assert(false); break;
                case AST_Binary_Operator::EQ: assert(false); break;
                case AST_Binary_Operator::NEQ: assert(false); break;
                case AST_Binary_Operator::LT: assert(false); break;
                case AST_Binary_Operator::GT: assert(false); break;
                case AST_Binary_Operator::LTEQ: assert(false); break;
                case AST_Binary_Operator::GTEQ: assert(false); break;
            }
            break;
        }
    }

    assert(false);
    return {};
}
