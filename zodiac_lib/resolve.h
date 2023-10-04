#pragma once

#include "ast.h"
#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct Allocator;
struct Scope;
struct Source_Range;
struct Type;
struct Type_Enum_Member;
struct Zodiac_Context;

enum class Infer_Source
{
    TYPE,
    TYPE_SPEC,
    EXPR,
    INFER_NODE,
};

enum class Infer_Target
{
    DEFAULT,
    ARGUMENT,
    COMPOUND,
};

struct Infer_Node
{
    Infer_Source source_kind;
    Infer_Target target_kind;

    union {
        Type *type;
        AST_Type_Spec *type_spec;
        AST_Expression *expr;
        Infer_Node *infer_node;
    } source;

    union {
        s64 index;
    }target;
};

enum class Flat_Node_Kind
{
    DECL,
    STMT,
    EXPR,
    TYPE_SPEC,

    FUNCTION_PROTO,
    IMPLICIT_LVALUE,
    RUN,
};

struct Flat_Node
{
    Flat_Node_Kind kind;
    Scope *scope;

    union
    {
        AST_Declaration *decl;
        AST_Statement *stmt;
        struct {
            AST_Expression *expr;
            Infer_Node *infer_type_from;
        } expr;
        AST_Implicit_LValue implicit_lvalue;

        struct {
            AST_Type_Spec *ts;
            bool via_pointer;
        } type_spec;

        struct {
            AST_Expression *expr;
        } run;
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
    Dynamic_Array<Flat_Root_Node *> nodes_to_run_bytecode;
};

typedef u64 Resolve_Results;
enum Resolve_Result : Resolve_Results
{
    RESOLVE_RESULT_NONE     = 0x00,
    RESOLVE_RESULT_PROGRESS = 0x01,
    RESOLVE_RESULT_DONE     = 0x02,
};

ZAPI void resolver_create(Resolver *resolver, Zodiac_Context *ctx);
ZAPI void resolver_destroy(Resolver *resolver);

ZAPI void resolver_add_file(Resolver *resolver, AST_File *file);
ZAPI bool resolver_cycle(Resolver *resolver);
ZAPI bool resolver_report_errors(Resolver *resolver);
ZAPI void resolver_add_declaration(Zodiac_Context *ctx, Resolver *resolver, AST_Declaration *decl, Scope *scope);
ZAPI Resolve_Results resolve_names(Resolver *resolver);
ZAPI Resolve_Results resolve_types(Resolver *resolver);

ZAPI Infer_Node *infer_node_new(Zodiac_Context *ctx, Infer_Source source, Infer_Target target);
ZAPI Infer_Node *infer_node_new(Zodiac_Context *ctx, AST_Type_Spec *ts);
ZAPI Infer_Node *infer_node_new(Zodiac_Context *ctx, Type *type);
ZAPI Infer_Node *infer_node_new(Zodiac_Context *ctx, AST_Expression *expr);

ZAPI Infer_Node *arg_infer_node_new(Zodiac_Context *ctx, Infer_Node *infer_node, s64 arg_index);
ZAPI Infer_Node *compound_infer_node_new(Zodiac_Context *ctx, Infer_Node *infer_node, s64 member_index);

ZAPI Type *infer_type(Zodiac_Context *ctx, Infer_Node *infer_node, Source_Range error_loc);

ZAPI void flatten_declaration(Resolver *resolver, AST_Declaration *decl, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_enum_declaration(Resolver *resolver, AST_Declaration *decl, Scope *scope);
ZAPI void flatten_statement(Resolver *resolver, AST_Statement *stmt, Scope *scope, Dynamic_Array<Flat_Node> *dest);
ZAPI void flatten_expression(Resolver *resolver, AST_Expression *expr, Scope *scope, Dynamic_Array<Flat_Node> *dest, Infer_Node *infer_node = nullptr);
ZAPI void flatten_type_spec(Resolver *resolver, AST_Type_Spec *ts, Scope *scope, Dynamic_Array<Flat_Node> *dest, bool via_pointer = false);
ZAPI void flatten_directive(Resolver *resolver, AST_Directive *directive, Scope *scope, Dynamic_Array<Flat_Node> *dest);

ZAPI Flat_Node to_flat_node(AST_Declaration *decl, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Statement *stmt, Scope *scope);
ZAPI Flat_Node to_flat_node(AST_Expression *expr, Scope *scope, Infer_Node *infer_type_from = nullptr);
ZAPI Flat_Node to_flat_node(AST_Type_Spec *ts, Scope *scope, bool via_pointer = false);

ZAPI Flat_Node to_flat_proto(AST_Declaration *decl, Scope *scope);

ZAPI bool name_resolve_node(Resolver *resolver, Flat_Node *node);
ZAPI bool name_resolve_decl(Resolver *resolver, AST_Declaration *decl, Scope *scope);
ZAPI bool name_resolve_stmt(Zodiac_Context *ctx, AST_Statement *stmt, Scope *scope);
ZAPI bool name_resolve_expr(Zodiac_Context *ctx, AST_Expression *expr, Scope *scope);
ZAPI bool name_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope, bool via_pointer);

ZAPI Dynamic_Array<Type_Enum_Member> resolve_missing_enum_values(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope);

ZAPI bool type_resolve_node(Resolver *resolver, Flat_Node *node);
ZAPI bool type_resolve_declaration(Zodiac_Context *ctx, AST_Declaration *decl, Scope *scope);
ZAPI bool type_resolve_statement(Resolver *resolver, AST_Statement *stmt, Scope *scope);
ZAPI bool type_resolve_expression(Resolver *resolver, AST_Expression *expr, Scope *scope, Infer_Node *infer_type_from);
ZAPI bool type_resolve_ts(Zodiac_Context *ctx, AST_Type_Spec *ts, Scope *scope, bool via_pointer);

ZAPI bool run_directive_is_const(Zodiac_Context *ctx, AST_Directive *dir);
ZAPI bool run_directive_expr_is_const(Zodiac_Context *ctx, AST_Expression *expr);
ZAPI bool run_directive_stmt_is_const(Zodiac_Context *ctx, AST_Statement *stmt);

}
