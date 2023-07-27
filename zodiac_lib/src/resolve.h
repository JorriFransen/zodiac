#pragma once

#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct AST_Declaration;
struct AST_Expression;
struct AST_File;
struct AST_Statement;
struct AST_Type_Spec;
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

    FUNCTION_PROTO,
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
    };
};

struct Flat_Root_Node
{
    Flat_Node root;
    Dynamic_Array<Flat_Node> nodes;

    u64 name_index = 0;
    u64 type_index = 0;
};

struct Resolver
{
    Zodiac_Context *ctx;
    Scope *global_scope;

    Allocator *node_allocator;

    Dynamic_Array<Flat_Root_Node *> nodes_to_name_resolve;
    Dynamic_Array<Flat_Root_Node *> nodes_to_type_resolve;
    Dynamic_Array<Flat_Root_Node *> nodes_to_emit_bytecode;
};

typedef u64 Resolve_Results;
enum Resolve_Result : Resolve_Results
{
    RESOLVE_RESULT_NONE     = 0x00,
    RESOLVE_RESULT_PROGRESS = 0x01,
    RESOLVE_RESULT_DONE     = 0x02,
};

ZAPI void resolver_create(Resolver *resolver, Zodiac_Context *ctx, Scope *global_scope);
ZAPI void resolve_file(Resolver *resolver, AST_File *file);
ZAPI void resolver_add_declaration(Zodiac_Context *ctx, Resolver *resolver, AST_Declaration *decl, Scope *scope);
ZAPI Resolve_Results resolve_names(Resolver *resolver);
ZAPI Resolve_Results resolve_types(Resolver *resolver);

ZAPI void flatten_declaration(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_statement(Zodiac_Context *ctx, AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_expression(AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest, AST_Type_Spec *infer_type_from);
ZAPI void flatten_type_spec(AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest);

ZAPI Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Expression *expr, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope);

ZAPI Flat_Node to_flat_proto(AST_Declaration *decl, Scope *scope);

ZAPI bool name_resolve_node(Resolver *resolver, Flat_Node *node);
ZAPI bool name_resolve_decl(Resolver *resolver, AST_Declaration *decl, Scope *scope);
ZAPI bool name_resolve_stmt(AST_Statement *stmt, Scope *scope);
ZAPI bool name_resolve_expr(Zodiac_Context *ctx, AST_Expression *expr, Scope *scope);
ZAPI bool name_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope);

ZAPI bool type_resolve_node(Zodiac_Context *ctx, Flat_Node *node);
ZAPI bool type_resolve_declaration(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope);
ZAPI bool type_resolve_statement(AST_Statement *stmt, Scope *scope);
ZAPI bool type_resolve_expression(Zodiac_Context *ctx, AST_Expression *expr, Scope *scope);
ZAPI bool type_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope);

}
