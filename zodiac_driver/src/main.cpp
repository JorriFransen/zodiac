
#include "asserts.h"
#include "ast.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h"
#include "logger.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
// #include "resolve.h"
#include "scope.h"
#include "zodiac_context.h"
#include "zstring.h"

using namespace Zodiac;

void flat_resolve_test(AST_File *file);

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    // TODO: CLEANUP: Used in the resolver
    // ctx = &c;

    Lexer lexer;
    lexer_create(&c, &lexer);
    String stream = {};

    auto filename = "tests/test.zc";
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

    // resolve_test(&c, file);
    flat_resolve_test(file);

    free(&dynamic_allocator, stream.data);

    return 0;
}

enum class Flat_Node_Kind
{
    DECL,
    STMT,
    EXPR,
    TS,

    PARAM_DECL,
};

struct Flat_Node
{
    Flat_Node_Kind kind;
    Scope *scope;

    union
    {
        AST_Declaration *decl;
        AST_Statement *stmt;
        AST_Expression *expr;
        AST_Type_Spec *ts;

        AST_Field_Declaration param;
    };
};

struct Flat_Root_Node
{
    Flat_Node root;
    Dynamic_Array<Flat_Node> nodes;
};

Scope *global_scope;

Dynamic_Array<Flat_Root_Node> nodes_to_name_resolve;

void name_resolve_node(Flat_Node *node);
void name_resolve_decl(AST_Declaration *decl);
void name_resolve_stmt(AST_Statement *stmt);
void name_resolve_expr(AST_Expression *expr, Scope *scope);
void name_resolve_ts(AST_Type_Spec *ts);

void flatten_declaration(AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest);
void flatten_statement(AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest);
void flatten_expression(AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest);
void flatten_type_spec(AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest);

Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope);
Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope);
Flat_Node to_flat_node(AST_Expression *expr, Scope *scope);
Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope);
Flat_Node to_flat_node(const AST_Field_Declaration param, Scope *scope);

#define add_builtin_symbol(kind, atom) {                                                              \
    add_resolved_symbol(global_scope, (kind), (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), (atom), nullptr); \
}

void flat_resolve_test(AST_File *file)
{
    global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);
    dynamic_array_create(&dynamic_allocator, &nodes_to_name_resolve);

    add_builtin_symbol(Symbol_Kind::TYPE, atom_s64);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_r32);
    add_builtin_symbol(Symbol_Kind::TYPE, atom_String);

    for (u64 i = 0; i < file->declarations.count; i++) {

        Flat_Root_Node node = {};
        node.root.kind = Flat_Node_Kind::DECL;
        node.root.decl = file->declarations[i];

        dynamic_array_create(&dynamic_allocator, &node.nodes);

        AST_Declaration *decl = file->declarations[i];

        flatten_declaration(decl, global_scope, &node.nodes);
        add_unresolved_decl_symbol(global_scope, decl, true);

        dynamic_array_append(&nodes_to_name_resolve, node);
    }

    for (u64 root_index = 0; root_index < nodes_to_name_resolve.count; root_index++) {

        Flat_Root_Node *node = &nodes_to_name_resolve[root_index];

        for (u64 i = 0; i < node->nodes.count; i++) {

            name_resolve_node(&node->nodes[i]);
        }
    }

    assert(file);
}

void name_resolve_node(Flat_Node *node)
{
    assert(node);

    switch (node->kind) {
        case Flat_Node_Kind::DECL: assert(false);
        case Flat_Node_Kind::STMT: assert(false);

        case Flat_Node_Kind::EXPR: {
            name_resolve_expr(node->expr, node->scope);
            break;
        }

        case Flat_Node_Kind::TS: assert(false);
        case Flat_Node_Kind::PARAM_DECL: assert(false);
    }
}

void name_resolve_decl(AST_Declaration *decl)
{
    assert (false);
}

void name_resolve_stmt(AST_Statement *stmt)
{
    assert (false);
}

void name_resolve_expr(AST_Expression *expr, Scope *scope)
{
    assert(expr && scope);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false);
        case AST_Expression_Kind::STRING_LITERAL: assert(false);
        case AST_Expression_Kind::NULL_LITERAL: assert(false);

        case AST_Expression_Kind::IDENTIFIER: {
            Symbol *sym = scope_get_symbol(scope, expr->identifier.name);

            if (!sym) {
                // resolve_error(expr, "Undefined symbol: '%s'", expr->identifier.name.data);
                assert_msg(false, "Undefined symbol");
                return;
            }

            if (sym->state == Symbol_State::RESOLVING) {

                // fatal_resolve_error(expr, "Circular dependency detected");
                assert_msg(false, "Circular dependency");
                return;

            } else if (sym->state == Symbol_State::UNRESOLVED) {

                bool global = sym->flags & SYM_FLAG_GLOBAL;
                switch (sym->kind) {
                    case Symbol_Kind::INVALID: assert(false);

                    case Symbol_Kind::FUNC:
                    case Symbol_Kind::TYPE:
                    case Symbol_Kind::MEMBER: {
                        assert(global);
                        // resolve_error(expr, "Unresolved symbol: '%s'", expr->identifier.name.data);
                        assert_msg(false, "Unresolved symbol");
                        return;
                    }

                    case Symbol_Kind::VAR:
                    case Symbol_Kind::CONST:
                    case Symbol_Kind::PARAM: {
                        assert(global);
                        assert(sym->decl);
                        name_resolve_decl(sym->decl);
                        break;
                    }
                }
            } else {
                assert(sym->state == Symbol_State::RESOLVED);
            }
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);
        case AST_Expression_Kind::CALL: assert(false);
        case AST_Expression_Kind::UNARY: assert(false);
        case AST_Expression_Kind::BINARY: assert(false);
    }
}

void name_resolve_ts(AST_Type_Spec *ts)
{
    assert (false);
}

void flatten_declaration(AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(decl && scope && dest);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->variable.type_spec;
            auto val = decl->variable.value;

            if (ts) flatten_type_spec(ts, scope, dest);
            if (val) flatten_expression(val, scope, dest);

            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            Scope *param_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_PARAMETER, scope);

            for (u64 i = 0; i < decl->function.params.count; i++) {

                flatten_type_spec(decl->function.params[i].type_spec, scope, dest);
                Flat_Node param_node = to_flat_node(decl->function.params[i], param_scope);
                dynamic_array_append(dest, param_node);
            }

            if (decl->function.return_ts) {
                flatten_type_spec(decl->function.return_ts, scope, dest);
            }

            // At this point we should emit some kind of function header/prototype node

            Scope *body_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, param_scope);

            for (u64 i = 0; i < decl->function.body.count; i++) {
                flatten_statement(decl->function.body[i], body_scope, dest);
            }

            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false);
        case AST_Declaration_Kind::UNION: assert(false);
    }

    Flat_Node flat_decl = to_flat_node(decl, scope);
    dynamic_array_append(dest, flat_decl);
}

void flatten_statement(AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(stmt && scope && dest);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {

            for (u64 i = 0; i < stmt->block.statements.count; i++) {
                flatten_statement(stmt->block.statements[i], scope, dest);
            }
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            flatten_declaration(stmt->decl.decl, scope, dest);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            flatten_expression(stmt->assign.dest, scope, dest);
            flatten_expression(stmt->assign.value, scope, dest);
            break;
        }

        case AST_Statement_Kind::CALL: {
            flatten_expression(stmt->call.call, scope, dest);
            break;
        }

        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                flatten_expression(stmt->return_stmt.value, scope, dest);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            flatten_expression(stmt->print_expr, scope, dest);
            break;
        }
    }

    Flat_Node flat_stmt = to_flat_node(stmt, scope);
    dynamic_array_append(dest, flat_stmt);
}

void flatten_expression(AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(expr && scope && dest);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::IDENTIFIER: {
            // Leaf expression
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false);
        case AST_Expression_Kind::INDEX: assert(false);

        case AST_Expression_Kind::CALL: {
            flatten_expression(expr->call.base, scope, dest);

            for (u64 i = 0 ; i < expr->call.args.count; i++) {
                flatten_expression(expr->call.args[i], scope, dest);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            flatten_expression(expr->binary.lhs, scope, dest);
            flatten_expression(expr->binary.rhs, scope, dest);
            break;
        }
    }

    Flat_Node flat_expr = to_flat_node(expr, scope);
    dynamic_array_append(dest, flat_expr);
}

void flatten_type_spec(AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest)
{
    assert(ts && scope && dest);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            // Leaf
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            flatten_type_spec(ts->base, scope, dest);
            break;
        }
    }

    Flat_Node flat_ts = to_flat_node(ts, scope);
    dynamic_array_append(dest, flat_ts);
}

Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope)
{
    assert(decl && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::DECL;
    result.scope = scope;
    result.decl = decl;
    return result;
}

Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope)
{
    assert(stmt && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::STMT;
    result.scope = scope;
    result.stmt = stmt;
    return result;
}

Flat_Node to_flat_node(AST_Expression *expr, Scope *scope)
{
    assert(expr && scope);

    Flat_Node result = {};
    result.scope = scope;
    result.kind = Flat_Node_Kind::EXPR;
    result.expr = expr;
    return result;
}

Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope)
{
    assert(ts && scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TS;
    result.scope = scope;
    result.ts = ts;
    return result;
}

Flat_Node to_flat_node(const AST_Field_Declaration param, Scope *scope)
{
    assert(scope);

    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TS;
    result.scope = scope;
    result.param = param;
    return result;
}
