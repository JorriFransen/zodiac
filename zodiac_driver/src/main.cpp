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


Dynamic_Array<Flat_Root_Node> nodes_to_name_resolve;

void flatten_declaration(AST_Declaration *decl, Dynamic_Array<Flat_Node> *dest);
void flatten_statement(AST_Statement *stmt, Dynamic_Array<Flat_Node> *dest);
void flatten_expression(AST_Expression *expr, Dynamic_Array<Flat_Node> *dest);
void flatten_type_spec(AST_Type_Spec *ts, Dynamic_Array<Flat_Node> *dest);

Flat_Node to_flat_node(AST_Declaration *decl);
Flat_Node to_flat_node(AST_Statement *stmt);
Flat_Node to_flat_node(AST_Expression *expr);
Flat_Node to_flat_node(AST_Type_Spec *ts);
Flat_Node to_flat_node(const AST_Field_Declaration param);

void flat_resolve_test(AST_File *file)
{

    dynamic_array_create(&dynamic_allocator, &nodes_to_name_resolve);

    for (u64 i = 0; i < file->declarations.count; i++) {

        Flat_Root_Node node = {};
        node.root.kind = Flat_Node_Kind::DECL;
        node.root.decl = file->declarations[i];

        dynamic_array_create(&dynamic_allocator, &node.nodes);

        flatten_declaration(file->declarations[i], &node.nodes);

        dynamic_array_append(&nodes_to_name_resolve, node);
    }

    // for (u64 i = 0; i < nodes_to_name_resolve.count; i++) {
    // }

    assert(file);
}

void flatten_declaration(AST_Declaration *decl, Dynamic_Array<Flat_Node> *dest)
{
    assert(decl && dest);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->variable.type_spec;
            auto val = decl->variable.value;

            if (ts) flatten_type_spec(ts, dest);
            if (val) flatten_expression(val, dest);

            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            for (u64 i = 0; i < decl->function.params.count; i++) {
                flatten_type_spec(decl->function.params[i].type_spec, dest);
                Flat_Node param_node = to_flat_node(decl->function.params[i]);
                dynamic_array_append(dest, param_node);
            }

            if (decl->function.return_ts) {
                flatten_type_spec(decl->function.return_ts, dest);
            }

            // At this point we should emit some kind of function header/prototype node

            for (u64 i = 0; i < decl->function.body.count; i++) {
                flatten_statement(decl->function.body[i], dest);
            }

            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false);
        case AST_Declaration_Kind::UNION: assert(false);
    }

    Flat_Node flat_decl = to_flat_node(decl);
    dynamic_array_append(dest, flat_decl);
}

void flatten_statement(AST_Statement *stmt, Dynamic_Array<Flat_Node> *dest)
{
    assert(stmt && dest);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {

            for (u64 i = 0; i < stmt->block.statements.count; i++) {
                flatten_statement(stmt->block.statements[i], dest);
            }
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            flatten_declaration(stmt->decl.decl, dest);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            flatten_expression(stmt->assign.dest, dest);
            flatten_expression(stmt->assign.value, dest);
            break;
        }

        case AST_Statement_Kind::CALL: {
            flatten_expression(stmt->call.call, dest);
            break;
        }

        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value) {
                flatten_expression(stmt->return_stmt.value, dest);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            flatten_expression(stmt->print_expr, dest);
            break;
        }
    }

    Flat_Node flat_stmt = to_flat_node(stmt);
    dynamic_array_append(dest, flat_stmt);
}

void flatten_expression(AST_Expression *expr, Dynamic_Array<Flat_Node> *dest)
{
    assert(expr && dest);

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
            flatten_expression(expr->call.base, dest);

            for (u64 i = 0 ; i < expr->call.args.count; i++) {
                flatten_expression(expr->call.args[i], dest);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false);

        case AST_Expression_Kind::BINARY: {
            flatten_expression(expr->binary.lhs, dest);
            flatten_expression(expr->binary.rhs, dest);
            break;
        }
    }

    Flat_Node flat_expr = to_flat_node(expr);
    dynamic_array_append(dest, flat_expr);
}

void flatten_type_spec(AST_Type_Spec *ts, Dynamic_Array<Flat_Node> *dest)
{
    assert(ts && dest);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            // Leaf
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            flatten_type_spec(ts->base, dest);
            break;
        }
    }

    Flat_Node flat_ts = to_flat_node(ts);
    dynamic_array_append(dest, flat_ts);
}

Flat_Node to_flat_node(AST_Declaration *decl)
{
    Flat_Node result = {};
    result.kind = Flat_Node_Kind::DECL;
    result.decl = decl;
    return result;
}

Flat_Node to_flat_node(AST_Statement *stmt)
{
    Flat_Node result = {};
    result.kind = Flat_Node_Kind::STMT;
    result.stmt = stmt;
    return result;
}

Flat_Node to_flat_node(AST_Expression *expr)
{
    Flat_Node result = {};
    result.kind = Flat_Node_Kind::EXPR;
    result.expr = expr;
    return result;
}

Flat_Node to_flat_node(AST_Type_Spec *ts)
{
    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TS;
    result.ts = ts;
    return result;
}

Flat_Node to_flat_node(const AST_Field_Declaration param)
{
    Flat_Node result = {};
    result.kind = Flat_Node_Kind::TS;
    result.param = param;
    return result;
}
