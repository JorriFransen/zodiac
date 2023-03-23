#pragma once

#include "ast.h"
#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct Scope;
struct Zodiac_Context;

enum class Flat_Node_Kind
{
    DECL,
    STMT,
    EXPR,
    TS,

    PARAM_DECL,
    FIELD_DECL,
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

        AST_Field_Declaration param, field;
    };
};

struct Flat_Root_Node
{
    Flat_Node root;
    Dynamic_Array<Flat_Node> nodes;
    u64 current_index = 0;
};

struct Resolver
{
    Zodiac_Context *ctx;
    Scope *global_scope;
Dynamic_Array<Flat_Root_Node> nodes_to_name_resolve;
    Dynamic_Array<Flat_Root_Node> nodes_to_type_resolve;
};

ZAPI void resolver_create(Resolver *resolver, Zodiac_Context *ctx, Scope *global_scope);
ZAPI void resolver_add_declaration(Resolver *resolver, AST_Declaration *decl);
ZAPI void resolve_names(Resolver *resolver);
ZAPI void resolve_types(Resolver *resolver);

ZAPI void flatten_declaration(AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_statement(AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_expression(AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_type_spec(AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest);

ZAPI Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Expression *expr, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope);
ZAPI Flat_Node to_flat_node(const AST_Field_Declaration param, Scope *scope);

ZAPI bool name_resolve_node(Flat_Node *node);
ZAPI bool name_resolve_decl(AST_Declaration *decl, Scope *scope);
ZAPI bool name_resolve_stmt(AST_Statement *stmt, Scope *scope);
ZAPI bool name_resolve_expr(AST_Expression *expr, Scope *scope);
ZAPI bool name_resolve_ts(AST_Type_Spec *ts, Scope *scope);

}
