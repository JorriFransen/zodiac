#include "asserts.h"
#include "lexer.h"
#include "logger.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "zodiac_context.h"
#include "zstring.h"

using namespace Zodiac;

namespace Zodiac { struct AST_File; }

void flat_resolve_test(AST_File *file);

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    // TODO: CLEANUP: Used in the resolver
    ctx = &c;

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

void flat_resolve_test(AST_File *file)
{

    dynamic_array_create(&dynamic_allocator, &nodes_to_name_resolve);

    for (u64 i = 0; i < file->declarations.count; i++) {

        Flat_Root_Node node;
        node.root.kind = Flat_Node_Kind::DECL;
        node.root.decl = file->declarations[i];

        dynamic_array_create(&dynamic_allocator, &node.nodes);

        flatten_declaration(file->declarations[i], &node.nodes);
    }

    assert(file);
}

void flatten_declaration(AST_Declaration *decl, Dynamic_Array<Flat_Node> *dest)
{
    assert(decl && dest);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);
        case AST_Declaration_Kind::VARIABLE: assert(false);

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            auto ts = decl->constant_variable.type_spec;
            auto val = decl->constant_variable.value;

            if (ts) flatten_type_spec(ts, dest);
            if (val) flatten_expression(val, dest);

            break;
        }

        case AST_Declaration_Kind::FUNCTION: assert(false);

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
        case AST_Statement_Kind::BLOCK: assert(false);
        case AST_Statement_Kind::DECLARATION: assert(false);
        case AST_Statement_Kind::ASSIGN: assert(false);
        case AST_Statement_Kind::CALL: assert(false);
        case AST_Statement_Kind::IF: assert(false);
        case AST_Statement_Kind::WHILE: assert(false);
        case AST_Statement_Kind::RETURN: assert(false);
        case AST_Statement_Kind::PRINT: assert(false);
    }
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
        case AST_Expression_Kind::CALL: assert(false);
        case AST_Expression_Kind::UNARY: assert(false);
        case AST_Expression_Kind::BINARY: assert(false);
    }

    Flat_Node flat_expr = to_flat_node(expr);
    dynamic_array_append(dest, flat_expr);
}

void flatten_type_spec(AST_Type_Spec *ts, Dynamic_Array<Flat_Node> *dest)
{
    assert(ts && dest);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);
        case AST_Type_Spec_Kind::NAME: assert(false);
        case AST_Type_Spec_Kind::POINTER: assert(false);
    }
}

Flat_Node to_flat_node(AST_Declaration *decl)
{
    Flat_Node result;
    result.kind = Flat_Node_Kind::DECL;
    result.decl = decl;
    return result;
}

Flat_Node to_flat_node(AST_Statement *stmt)
{
    Flat_Node result;
    result.kind = Flat_Node_Kind::STMT;
    result.stmt = stmt;
    return result;
}

Flat_Node to_flat_node(AST_Expression *expr)
{
    Flat_Node result;
    result.kind = Flat_Node_Kind::EXPR;
    result.expr = expr;
    return result;
}

Flat_Node to_flat_node(AST_Type_Spec *ts)
{
    Flat_Node result;
    result.kind = Flat_Node_Kind::TS;
    result.ts = ts;
    return result;
}
