#include "lexer.h"
#include "ast.h"
#include "atom.h"
#include "bytecode/bytecode.h"
#include "bytecode/printer.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "memory/temporary_allocator.h"
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

void flat_resolve_test(Resolver *resolver, AST_File *file);

void emit_bytecode(Resolver *resolver, Bytecode_Builder *bb);
void ast_decl_to_bytecode(Bytecode_Builder *bb, AST_Declaration *decl);
void ast_function_to_bytecode(Bytecode_Builder *bb, AST_Declaration *decl);
void ast_stmt_to_bytecode(Bytecode_Builder *bb, AST_Statement *stmt);
Bytecode_Register ast_expr_to_bytecode(Bytecode_Builder *bb, AST_Expression *expr, Type *actual_type = nullptr);

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

    Scope *global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);

    Resolver resolver;
    resolver_create(&resolver, &c, global_scope);

    flat_resolve_test(&resolver, file);

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

    emit_bytecode(&resolver, &bb);

    bytecode_print(&bb, temp_allocator_allocator());

    return 0;
}

#define add_builtin_type_symbol(type) { \
    auto sym = add_resolved_symbol(resolver->ctx, resolver->global_scope, Symbol_Kind::TYPE, (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), atom_##type, nullptr); \
    sym->builtin_type = &builtin_type_##type; \
}

void flat_resolve_test(Resolver *resolver, AST_File *file)
{
    assert(file);

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
        resolver_add_declaration(resolver->ctx, resolver, file->declarations[i]);
    }

    bool names_done = resolve_names(resolver);

    bool types_done = false;
    if (names_done) types_done = resolve_types(resolver);

    if (names_done && types_done) assert(resolver->ctx->errors.count == 0);
}


void emit_bytecode(Resolver *resolver, Bytecode_Builder *bb)
{
    assert(resolver);
    assert(bb);

    for (u64 i = 0; i < resolver->nodes_to_emit_bytecode.count; i++) {

        Flat_Root_Node root_node = resolver->nodes_to_emit_bytecode[i];

        switch (root_node.root.kind) {

            case Flat_Node_Kind::DECL: {
                ast_decl_to_bytecode(bb, root_node.root.decl);
                break;
            }

            case Flat_Node_Kind::STMT: assert(false); break;
            case Flat_Node_Kind::EXPR: assert(false); break;
            case Flat_Node_Kind::TS: assert(false); break;
            case Flat_Node_Kind::PARAM_DECL: assert(false); break;
            case Flat_Node_Kind::FIELD_DECL: assert(false); break;
            case Flat_Node_Kind::FUNCTION_PROTO: assert(false); break;
        }
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

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {

            if (decl->variable.resolved_type->kind == Type_Kind::INTEGER ||
                decl->variable.resolved_type == &builtin_type_unsized_integer) {
                // No need to emit anything
                break;
            } else {
                assert_msg(false, "Not implemented")
            }

            assert(false);
            break;
        }

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

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_bytecode(bb, stmt->decl.decl);
            break;
        }

        case AST_Statement_Kind::ASSIGN: assert(false); break;
        case AST_Statement_Kind::CALL: assert(false); break;
        case AST_Statement_Kind::IF: assert(false); break;
        case AST_Statement_Kind::WHILE: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                assert(stmt->return_stmt.scope);
                AST_Declaration *func_decl = enclosing_function(stmt->return_stmt.scope);
                assert(func_decl);

                assert(func_decl->function.type);
                Type *return_type = func_decl->function.type->function.return_type;

                Bytecode_Register return_value_register = ast_expr_to_bytecode(bb, stmt->return_stmt.value, return_type);
                bytecode_emit_return(bb, return_value_register);
            } else {
                bytecode_emit_return(bb);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: assert(false); break;
    }
}

Bytecode_Register ast_expr_to_bytecode(Bytecode_Builder *bb, AST_Expression *expr, Type *actual_type/*=nullptr*/)
{
    assert(bb);
    assert(expr);

    if (actual_type) {
        assert(expr->resolved_type == &builtin_type_unsized_integer ||
               expr->resolved_type == actual_type);
        assert(actual_type != &builtin_type_unsized_integer);
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            Type *literal_type = expr->resolved_type;
            if (literal_type->kind == Type_Kind::UNSIZED_INTEGER) {
                assert(actual_type);
                literal_type = actual_type;
            }
            assert(literal_type->kind == Type_Kind::INTEGER);
            return bytecode_integer_literal(bb, literal_type, expr->integer_literal.value);
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {

            assert(expr->identifier.scope);
            Symbol *ident_sym = scope_get_symbol(expr->identifier.scope, expr->identifier);
            assert(ident_sym);
            assert(ident_sym->decl);
            AST_Declaration *ident_decl = ident_sym->decl;

            switch (ident_decl->kind) {
                case AST_Declaration_Kind::INVALID: assert(false); break;
                case AST_Declaration_Kind::VARIABLE: assert(false); break;

                case AST_Declaration_Kind::CONSTANT_VARIABLE: {
                    assert(ident_decl->variable.value);
                    Type *type = expr->resolved_type;
                    if (ident_decl->variable.resolved_type == &builtin_type_unsized_integer) {
                        assert(actual_type);
                        type = actual_type;
                    }
                    return ast_expr_to_bytecode(bb, ident_decl->variable.value, type);
                }

                case AST_Declaration_Kind::FUNCTION: assert(false); break;
                case AST_Declaration_Kind::STRUCT: assert(false); break;
                case AST_Declaration_Kind::UNION: assert(false); break;
            }

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
