#include "ast.h"
#include "atom.h"
#include "bytecode/bytecode.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "containers/dynamic_array.h"
#include "containers/hash_table.h"
#include "defines.h"
#include "error.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
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

struct Bytecode_Converter
{
    Allocator *allocator;
    Zodiac_Context *context;
    Bytecode_Builder *builder;

    // TODO: should these be separated per function?
    Hash_Table<AST_Declaration *, Bytecode_Register> allocations;

    // TODO: should these be separated per file?
    Hash_Table<AST_Declaration *, Bytecode_Global_Handle> globals;
};

Bytecode_Converter bytecode_converter_create(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb);

void emit_bytecode(Resolver *resolver, Bytecode_Converter *bc);
void ast_decl_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl);
void ast_function_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl);
void ast_stmt_to_bytecode(Bytecode_Converter *bc, AST_Statement *stmt);
Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type *actual_type = nullptr);

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
    if (parser.error) return 1;
    assert(file);

    Scope *global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);

    Resolver resolver;
    resolver_create(&resolver, &c, global_scope);

    flat_resolve_test(&resolver, file);

    for (s64 i = 0; i < c.errors.count; i++) {

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

    Bytecode_Converter bc = bytecode_converter_create(&c.bytecode_allocator, &c, &bb);

    emit_bytecode(&resolver, &bc);

    bytecode_print(&bb, temp_allocator_allocator());

    Bytecode_Validator validator = {};
    bytecode_validator_init(&c, &c.bytecode_allocator, &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    if (!bytecode_valid) {
        assert(validator.errors.count);

        bytecode_validator_print_errors(&validator);
    }

    return 0;
}

#define add_builtin_type_symbol(type) { \
    auto sym = add_resolved_symbol(resolver->ctx, resolver->global_scope, Symbol_Kind::TYPE, (SYM_FLAG_BUILTIN), atom_##type, nullptr); \
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

    for (s64 i = 0; i < file->declarations.count; i++) {
        resolver_add_declaration(resolver->ctx, resolver, file->declarations[i]);
    }

    bool names_done = resolve_names(resolver);

    bool types_done = false;
    if (names_done) types_done = resolve_types(resolver);

    if (names_done && types_done) assert(resolver->ctx->errors.count == 0);
}


Bytecode_Converter bytecode_converter_create(Allocator *allocator, Zodiac_Context *context, Bytecode_Builder *bb)
{
    Bytecode_Converter result = {};

    result.allocator = allocator;
    result.context = context;
    result.builder = bb;

    hash_table_create(allocator, &result.allocations);
    hash_table_create(allocator, &result.globals);

    return result;
}

void emit_bytecode(Resolver *resolver, Bytecode_Converter *bc)
{
    assert(resolver);
    assert(bc);

    for (s64 i = 0; i < resolver->nodes_to_emit_bytecode.count; i++) {

        Flat_Root_Node root_node = resolver->nodes_to_emit_bytecode[i];

        switch (root_node.root.kind) {

            case Flat_Node_Kind::DECL: {
                ast_decl_to_bytecode(bc, root_node.root.decl);
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

void ast_decl_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl)
{
    assert(bc);
    assert(decl);
    assert(decl->kind != AST_Declaration_Kind::INVALID);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (decl->flags & AST_DECL_FLAG_GLOBAL) {

                Type *global_type = decl->variable.resolved_type;
                assert(global_type);
                auto global_name = decl->identifier.name;

                Bytecode_Register initial_value_reg;
                if (decl->variable.value) {
                    initial_value_reg = ast_expr_to_bytecode(bc, decl->variable.value, decl->variable.resolved_type);
                }

                auto global_handle = bytecode_create_global(bc->builder, global_name, global_type, initial_value_reg);

                hash_table_add(&bc->globals, decl, global_handle);

            } else {
                // Leaf
            }
            break;
        }

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
            ast_function_to_bytecode(bc, decl);
            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false); break;
        case AST_Declaration_Kind::UNION: assert(false); break;
    }
}

void ast_function_to_bytecode(Bytecode_Converter *bc, AST_Declaration *decl)
{
    assert(bc);
    assert(decl);
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);

    assert(decl->function.type);
    assert(decl->function.type->kind == Type_Kind::FUNCTION);
    assert(decl->identifier.name.data);

    Bytecode_Function_Handle fn_handle = bytecode_function_create(bc->builder, decl->identifier.name, decl->function.type);

    if (decl->function.variables.count) {

        Bytecode_Block_Handle allocs_block_handle = bytecode_append_block(bc->builder, fn_handle, "allocs");
        bytecode_set_insert_point(bc->builder, fn_handle, allocs_block_handle);

        bool has_inits = false;

        for (s64 i = 0; i < decl->function.variables.count; i++) {
            AST_Declaration *var_decl = decl->function.variables[i];
            assert(var_decl->variable.resolved_type);

            Type *alloc_type = var_decl->variable.resolved_type;
            auto alloc_name = var_decl->identifier.name;
            auto alloc_reg = bytecode_emit_alloc(bc->builder, alloc_type, alloc_name.data);

            debug_assert(!hash_table_find(&bc->allocations, var_decl));

            hash_table_add(&bc->allocations, var_decl, alloc_reg);

            if (var_decl->variable.value) has_inits = true;
        }

        if (has_inits) {
            Bytecode_Block_Handle inits_block_handle = bytecode_append_block(bc->builder, fn_handle, "inits");
            bytecode_set_insert_point(bc->builder, fn_handle, inits_block_handle);

            for (s64 i = 0; i < decl->function.variables.count; i++) {
                AST_Declaration *var_decl = decl->function.variables[i];
                if (var_decl->variable.value) {
                    Bytecode_Register value = ast_expr_to_bytecode(bc, var_decl->variable.value);

                    // TODO: Temporarily store this on the stack from before?
                    Bytecode_Register alloc_reg;
                    bool found = hash_table_find(&bc->allocations, var_decl, &alloc_reg);
                    assert(found)

                    bytecode_emit_store_alloc(bc->builder, value, alloc_reg);
                }
            }
        }
        Bytecode_Block_Handle start_block_handle = bytecode_append_block(bc->builder, fn_handle, "start");
        bytecode_set_insert_point(bc->builder, fn_handle, start_block_handle);
    } else {
        Bytecode_Block_Handle entry_block_handle = bytecode_append_block(bc->builder, fn_handle, "entry");
        bytecode_set_insert_point(bc->builder, fn_handle, entry_block_handle);
    }

    for (s64 i = 0; i < decl->function.body.count; i++) {
        AST_Statement *stmt = decl->function.body[i];
        ast_stmt_to_bytecode(bc, stmt);
    }
}

void ast_stmt_to_bytecode(Bytecode_Converter *bc, AST_Statement *stmt)
{
    assert(bc);
    assert(stmt);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::BLOCK: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_bytecode(bc, stmt->decl.decl);
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

                Bytecode_Register return_value_register = ast_expr_to_bytecode(bc, stmt->return_stmt.value, return_type);
                bytecode_emit_return(bc->builder, return_value_register);
            } else {
                bytecode_emit_return(bc->builder);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            assert(stmt->print_expr->resolved_type->kind == Type_Kind::INTEGER);
            Bytecode_Register value_reg = ast_expr_to_bytecode(bc, stmt->print_expr);
            bytecode_emit_print(bc->builder, value_reg);
            break;
        }
    }
}

Bytecode_Register ast_expr_to_bytecode(Bytecode_Converter *bc, AST_Expression *expr, Type *actual_type/*=nullptr*/)
{
    assert(bc);
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
            return bytecode_integer_literal(bc->builder, literal_type, expr->integer_literal.value);
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

                case AST_Declaration_Kind::VARIABLE: {
                    assert(ident_decl->variable.resolved_type);

                    if (ident_decl->flags & AST_DECL_FLAG_GLOBAL) {

                        Bytecode_Global_Handle global_handle;
                        bool found = hash_table_find(&bc->globals, ident_decl, &global_handle);
                        assert(found);

                        return bytecode_emit_load_global(bc->builder, global_handle);

                    } else {
                        Bytecode_Register alloc_reg;
                        bool found = hash_table_find(&bc->allocations, ident_decl, &alloc_reg);
                        assert(found);

                        return bytecode_emit_load_alloc(bc->builder, alloc_reg);
                    }
                }

                case AST_Declaration_Kind::CONSTANT_VARIABLE: {
                    assert(ident_decl->variable.value);
                    Type *type = expr->resolved_type;
                    if (ident_decl->variable.resolved_type == &builtin_type_unsized_integer) {
                        assert(actual_type);
                        type = actual_type;
                    }
                    return ast_expr_to_bytecode(bc, ident_decl->variable.value, type);
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

            if (actual_type) {
                assert(actual_type == expr->resolved_type);
            } else {
                actual_type = expr->resolved_type;
            }
            assert(actual_type->kind == Type_Kind::INTEGER);

            Bytecode_Register lhs_reg = ast_expr_to_bytecode(bc, expr->binary.lhs, actual_type);
            Bytecode_Register rhs_reg = ast_expr_to_bytecode(bc, expr->binary.rhs, actual_type);

            switch (expr->binary.op) {
                case AST_Binary_Operator::INVALID: assert(false); break;

                case AST_Binary_Operator::ADD: {
                    return bytecode_emit_add(bc->builder, lhs_reg, rhs_reg);
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
