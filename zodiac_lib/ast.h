#pragma once

#include "atom.h"
#include "common.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "source_pos.h"
#include "util/zstring.h"

namespace Zodiac
{

struct Allocator;
struct AST_Declaration;
struct AST_Directive;
struct AST_Expression;
struct AST_Statement;
struct AST_Type_Spec;
struct Scope;
struct String_Builder;
struct Type;
struct Zodiac_Context;

struct AST_Identifier
{
    Atom name;

    Source_Range range;

    Scope *scope;
};

struct AST_Integer_Literal_Expression
{
    Integer_Value value;
};

struct AST_Real_Literal_Expression
{
    Real_Value value;
};

struct AST_String_Literal_Expression
{
    Atom atom;
};

struct AST_Member_Expression
{
    AST_Expression *base;
    Atom member_name;

    s64 index_in_parent;

    // Used for nested member expressions
    AST_Declaration *type_decl;
};

struct AST_Index_Expression
{
    AST_Expression *base;
    AST_Expression *index;
};

struct AST_Call_Expression
{
    AST_Expression *base;
    Dynamic_Array<AST_Expression *> args;
};

enum class AST_Unary_Operator
{
    INVALID,

    PLUS       = '+',
    MINUS      = '-',
    ADDRESS_OF = '*',
    DEREF      = '<',
};

struct AST_Unary_Expression
{
    AST_Unary_Operator op;
    AST_Expression *operand;
};

enum class AST_Binary_Operator
{
    INVALID = 0,

    FIRST_BINOP,

    FIRST_ARITHMETIC_OP = FIRST_BINOP,
    ADD = FIRST_BINOP,
    SUB,
    MUL,
    DIV,
    LAST_ARITHMETIC_OP = DIV,

    EQ,
    FIRST_CMP_OP = EQ,
    NEQ,
    LT,
    GT,
    LTEQ,
    GTEQ,
    LAST_CMP_OP = GTEQ,

    LAST_BINOP = GTEQ,
};

struct AST_Binary_Expression
{
    AST_Binary_Operator op;
    AST_Expression *lhs;
    AST_Expression *rhs;
};

struct AST_Cast_Expression
{
    Type *type;
    AST_Type_Spec *type_spec;

    AST_Expression *value;
};

struct AST_Directive_Expression
{
    AST_Directive *directive;
    AST_Expression *generated_expression;
};

struct AST_Compound_Expression
{
    Dynamic_Array<AST_Expression *> expressions;
};

enum class AST_Expression_Kind
{
    INVALID,

    INTEGER_LITERAL,
    REAL_LITERAL,
    STRING_LITERAL,
    CHAR_LITERAL,
    NULL_LITERAL,
    BOOL_LITERAL,

    IDENTIFIER,

    MEMBER,
    INDEX,
    CALL,

    UNARY,
    BINARY,

    CAST,

    RUN_DIRECTIVE,

    COMPOUND,
};

typedef u32 AST_Expression_Flags;
enum AST_Expression_Flag : AST_Expression_Flags
{
    AST_EXPR_FLAG_NONE     = 0x00,
    AST_EXPR_FLAG_TYPED    = 0x01,
    AST_EXPR_FLAG_CONST    = 0x02,
    AST_EXPR_FLAG_LITERAL  = 0x04,
    AST_EXPR_FLAG_LVALUE   = 0x08,
};


#define EXPR_IS_TYPED(e) ((e)->flags & AST_EXPR_FLAG_TYPED)
#define EXPR_IS_CONST(e) ((e)->flags & AST_EXPR_FLAG_CONST)
#define EXPR_IS_LITERAL(e) ((e)->flags & AST_EXPR_FLAG_LITERAL)
#define EXPR_IS_LVALUE(e) ((e)->flags & AST_EXPR_FLAG_LVALUE)

struct AST_Expression
{
    AST_Expression_Kind kind;
    AST_Expression_Flags flags;

    Source_Range range;

    Type *resolved_type;

    union
    {
        AST_Integer_Literal_Expression integer_literal;
        AST_Real_Literal_Expression real_literal;
        AST_String_Literal_Expression string_literal;
        char character_literal;
        bool bool_literal;
        AST_Identifier identifier;
        AST_Member_Expression member;
        AST_Index_Expression index;
        AST_Call_Expression call;
        AST_Unary_Expression unary;
        AST_Binary_Expression binary;
        AST_Cast_Expression cast;
        AST_Directive_Expression directive;
        AST_Compound_Expression compound;
    };
};

struct AST_Block_Statement
{
    Scope *scope;
    Dynamic_Array<AST_Statement *> statements;
};

struct AST_Declaration_Statement
{
    AST_Declaration *decl;
};

struct AST_Assign_Statement
{
    AST_Expression *dest;
    AST_Expression *value;
};

struct AST_Call_Statement
{
    AST_Expression *call;
};

struct AST_If_Block
{
    AST_Expression *cond;
    AST_Statement *then;
    Scope *then_scope;
};

struct AST_If_Statement
{
    Dynamic_Array<AST_If_Block> blocks;
    AST_Statement *else_stmt;
    Scope *else_scope;
};

struct AST_While_Statement
{
    AST_Expression *cond;
    AST_Statement *do_stmt;
    Scope *scope;
};

struct AST_Return_Statement
{
    AST_Expression *value;
    Scope *scope;
};

struct AST_Print_Expression
{
    Dynamic_Array<AST_Expression *> expressions;
};

enum class AST_Statement_Kind
{
    INVALID,

    BLOCK,

    DECLARATION,
    ASSIGN,
    CALL,

    IF,
    WHILE,

    RETURN,

    // Temporary stuff
    PRINT,
};

typedef u32 AST_Statement_Flags;

enum AST_Statement_Flag : AST_Statement_Flags
{
    AST_STMT_FLAG_NONE  = 0x00,
    AST_STMT_FLAG_TYPED = 0x01,
};

#define STMT_IS_TYPED(s) ((s)->flags & AST_STMT_FLAG_TYPED)

struct AST_Statement
{
    AST_Statement_Kind kind;
    AST_Statement_Flags flags;

    Source_Range range;

    union
    {
        AST_Block_Statement block;
        AST_Declaration_Statement decl;
        AST_Assign_Statement assign;
        AST_Call_Statement call;
        AST_If_Statement if_stmt;
        AST_While_Statement while_stmt;
        AST_Return_Statement return_stmt;
        AST_Print_Expression print_expr;
    };
};

// Also used for constant variables
struct AST_Variable_Declaration
{
    AST_Type_Spec *type_spec;
    AST_Expression *value;

    Type *resolved_type;
};

struct AST_Const_LValue
{
    AST_Expression *expr; // The expression that 'identifies' the declaration
    AST_Declaration *decl;
};

struct AST_Function_Declaration
{
    Dynamic_Array<AST_Declaration *> params;
    Dynamic_Array<AST_Declaration *> variables;
    Dynamic_Array<AST_Const_LValue> const_lvalues;

    AST_Type_Spec *return_ts;

    Dynamic_Array<AST_Statement *> body;

    Type *type;
    Type *inferred_return_type;

    Scope *parameter_scope;
    Scope *local_scope;
};

struct AST_Aggregate_Declaration
{
    Dynamic_Array<AST_Declaration *> fields;
    Type *resolved_type;
};

enum class AST_Declaration_Kind
{
    INVALID,

    VARIABLE,
    CONSTANT_VARIABLE,
    PARAMETER,
    FIELD,

    FUNCTION,

    STRUCT,
    UNION,

    RUN_DIRECTIVE,
};

typedef u32 AST_Declaration_Flags;

enum AST_Declaration_Flag : AST_Declaration_Flags
{
    AST_DECL_FLAG_NONE    = 0x00,
    AST_DECL_FLAG_GLOBAL  = 0x01,
    AST_DECL_FLAG_TYPED   = 0x02,
    AST_DECL_FLAG_FOREIGN = 0x04,
};

#define DECL_IS_GLOBAL(d) (((d)->flags & AST_DECL_FLAG_GLOBAL) == AST_DECL_FLAG_GLOBAL)
#define DECL_IS_TYPED(d) (((d)->flags & AST_DECL_FLAG_TYPED) == AST_DECL_FLAG_TYPED)

struct AST_Declaration
{
    AST_Declaration_Kind kind;
    AST_Declaration_Flags flags;

    Source_Range range;

    AST_Identifier identifier;

    union
    {
        union { AST_Variable_Declaration variable, field, parameter; }; // Also used for constant variables
        AST_Function_Declaration function;
        AST_Aggregate_Declaration aggregate;
        AST_Directive *directive;
    };
};

enum class AST_Type_Spec_Kind
{
    INVALID,

    TYPE,
    NAME,
    POINTER,
    STATIC_ARRAY,
};

struct AST_Type_Spec
{
    AST_Type_Spec_Kind kind;

    Source_Range range;

    Type *resolved_type;

    union
    {
        AST_Identifier identifier;
        AST_Type_Spec *pointer_base;

        struct {
            AST_Expression *length_expr;
            AST_Type_Spec *element_ts;
        } static_array;
    };
};

enum class AST_Directive_Kind
{
    INVALID,

    RUN,
};

enum class AST_Run_Directive_Kind
{
    INVALID,

    EXPR,
    STMT,
};

struct AST_Directive
{
    AST_Directive_Kind kind;

    Source_Range range;

    union
    {
        struct
        {
            AST_Run_Directive_Kind kind;
            Scope *scope;

            AST_Expression *expr;
            AST_Statement *stmt;
        } run;
    };
};

struct AST_File
{
    Dynamic_Array<AST_Declaration *> declarations;
};

ZAPI void ast_identifier_create(Atom name, Source_Range range, AST_Identifier *out_ident);

ZAPI void ast_integer_literal_expr_create(Integer_Value value, AST_Expression *out_expr);
ZAPI void ast_real_literal_expr_create(Real_Value value, AST_Expression *out_expr);
ZAPI void ast_string_literal_expr_create(Atom atom, AST_Expression *out_expr);
ZAPI void ast_character_literal_expr_create(char c, AST_Expression *out_expr);
ZAPI void ast_null_literal_expr_create(AST_Expression *out_expr);
ZAPI void ast_bool_literal_expr_create(AST_Expression *out_expr, bool value);
ZAPI void ast_identifier_expr_create(AST_Identifier ident, AST_Expression *out_expr);
ZAPI void ast_member_expr_create(AST_Expression *base, Atom atom, AST_Expression *out_expr);
ZAPI void ast_call_expr_create(AST_Expression *base, Dynamic_Array<AST_Expression *> args, AST_Expression *out_expr);
ZAPI void ast_index_expr_create(AST_Expression *base, AST_Expression *index, AST_Expression *out_expr);
ZAPI void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Expression *out_expr);
ZAPI void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Expression *out_expr);
ZAPI void ast_cast_expr_create(AST_Type_Spec *ts, AST_Expression *value, AST_Expression *out_expr);
ZAPI void ast_cast_expr_create(Type *type, AST_Expression *value, AST_Expression *out_expr);
ZAPI void ast_run_directive_expr_create(AST_Directive *directive, AST_Expression *out_expr);
ZAPI void ast_compound_expr_create(Dynamic_Array<AST_Expression *> expressions, AST_Expression *out_expr);
ZAPI void ast_expression_create(AST_Expression_Kind kind, AST_Expression_Flags flags, AST_Expression *out_expr);

ZAPI void ast_block_stmt_create(Dynamic_Array<AST_Statement *> statements, AST_Statement *out_stmt);
ZAPI void ast_declaration_stmt_create(AST_Declaration *decl, AST_Statement *out_stmt);
ZAPI void ast_assign_stmt_create(AST_Expression *dest, AST_Expression *value, AST_Statement *out_stmt);
ZAPI void ast_call_stmt_create(AST_Expression *call, AST_Statement *out_stmt);
ZAPI void ast_if_stmt_create(Dynamic_Array<AST_If_Block> blocks, AST_Statement *else_stmt, AST_Statement *out_stmt);
ZAPI void ast_while_stmt_create(AST_Expression *cond, AST_Statement *do_stmt, AST_Statement *out_stmt);
ZAPI void ast_return_stmt_create(AST_Expression *value, AST_Statement *out_stmt);
ZAPI void ast_print_stmt_create(Dynamic_Array<AST_Expression *> exprs, AST_Statement *out_stmt);
ZAPI void ast_statement_create(AST_Statement_Kind kind, AST_Statement *out_stmt);

ZAPI void ast_variable_decl_create(AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value, AST_Declaration *out_decl);
ZAPI void ast_constant_variable_decl_create(AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value, AST_Declaration *out_decl);
ZAPI void ast_parameter_decl_create(AST_Identifier ident, AST_Type_Spec *ts, Source_Range range, AST_Declaration *out_decl);
ZAPI void ast_field_decl_create(AST_Identifier ident, AST_Type_Spec *ts, Source_Range range, AST_Declaration *out_decl);
ZAPI void ast_function_decl_create(Allocator *allocator, AST_Identifier ident, Dynamic_Array<AST_Declaration *> args, AST_Type_Spec *return_ts, Dynamic_Array<AST_Statement *> body, AST_Declaration *out_decl, AST_Declaration_Flags flags);
ZAPI void ast_aggregate_decl_create(AST_Identifier *ident, AST_Declaration_Kind kind, Dynamic_Array<AST_Declaration *> fields, AST_Declaration *out_decl);
ZAPI void ast_run_directive_decl_create(AST_Directive *run_directive, AST_Declaration *out_decl);
ZAPI void ast_declaration_create(AST_Declaration_Kind kind, AST_Declaration_Flags flags, AST_Declaration *out_decl);

ZAPI void ast_type_ts_create(Type *type, AST_Type_Spec *out_ts);
ZAPI void ast_name_ts_create(AST_Identifier ident, AST_Type_Spec *out_ts);
ZAPI void ast_pointer_ts_create(AST_Type_Spec *base, AST_Type_Spec *out_ts);
ZAPI void ast_static_array_ts_create(AST_Expression *length_expr, AST_Type_Spec *element_ts, AST_Type_Spec *out_ts);
ZAPI void ast_type_spec_create(AST_Type_Spec_Kind kind, AST_Type_Spec *out_ts);

ZAPI void ast_run_directive_create(AST_Expression *expr, AST_Directive *out_dir);
ZAPI void ast_run_directive_create(AST_Statement *stmt, AST_Directive *out_dir);
ZAPI void ast_directive_create(AST_Directive_Kind kind, AST_Directive *out_dir);

ZAPI void ast_file_create(Dynamic_Array<AST_Declaration *> decls, AST_File *out_file);

ZAPI AST_Expression *ast_integer_literal_expr_new(Zodiac_Context *ctx, Source_Range range, Integer_Value value);
ZAPI AST_Expression *ast_real_literal_expr_new(Zodiac_Context *ctx, Source_Range range, Real_Value value);
ZAPI AST_Expression *ast_string_literal_expr_new(Zodiac_Context *ctx, Source_Range range, Atom atom);
ZAPI AST_Expression *ast_character_literal_expr_new(Zodiac_Context *ctx, Source_Range range, char character);
ZAPI AST_Expression *ast_null_literal_expr_new(Zodiac_Context *ctx, Source_Range range);
ZAPI AST_Expression *ast_bool_literal_expr_new(Zodiac_Context *ctx, Source_Range range, bool value);
ZAPI AST_Expression *ast_identifier_expr_new(Zodiac_Context *ctx, Source_Range range, Atom atom);
ZAPI AST_Expression *ast_member_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *base, Atom atom);
ZAPI AST_Expression *ast_index_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *base, AST_Expression *index);
ZAPI AST_Expression *ast_call_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *base, Dynamic_Array<AST_Expression *> args);
ZAPI AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Unary_Operator op, AST_Expression *operand);
ZAPI AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs);
ZAPI AST_Expression *ast_cast_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Type_Spec *ts, AST_Expression *value);
ZAPI AST_Expression *ast_cast_expr_new(Zodiac_Context *ctx, Source_Range range, Type *type, AST_Expression *value);
ZAPI AST_Expression *ast_run_directive_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Directive *directive);
ZAPI AST_Expression *ast_compound_expr_new(Zodiac_Context *ctx, Source_Range range, Dynamic_Array<AST_Expression *> expressions);
ZAPI AST_Expression *ast_expression_new(Zodiac_Context *ctx, Source_Range range);

ZAPI AST_Statement *ast_block_stmt_new(Zodiac_Context *ctx, Source_Range range, Dynamic_Array<AST_Statement *> statements);
ZAPI AST_Statement *ast_declaration_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Declaration *decl);
ZAPI AST_Statement *ast_assign_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *dest, AST_Expression *value);
ZAPI AST_Statement *ast_call_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *call);
ZAPI AST_Statement *ast_if_stmt_new(Zodiac_Context *ctx, Source_Range range, Dynamic_Array<AST_If_Block> blocks, AST_Statement *else_stmt);
ZAPI AST_Statement *ast_while_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *cond, AST_Statement *do_stmt);
ZAPI AST_Statement *ast_return_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *value);
ZAPI AST_Statement *ast_print_statement_new(Zodiac_Context *ctx, Source_Range range, Dynamic_Array<AST_Expression *> exprs);
ZAPI AST_Statement *ast_statement_new(Zodiac_Context *ctx, Source_Range range);

ZAPI AST_Declaration *ast_variable_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value);
ZAPI AST_Declaration *ast_constant_variable_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value);
ZAPI AST_Declaration *ast_parameter_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts);
ZAPI AST_Declaration *ast_field_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts);
ZAPI AST_Declaration *ast_function_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, Dynamic_Array<AST_Declaration *> args, AST_Type_Spec *return_ts, Dynamic_Array<AST_Statement *> body, AST_Declaration_Flags flags);
ZAPI AST_Declaration *ast_aggregate_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Declaration_Kind kind, Dynamic_Array<AST_Declaration *> fields);
ZAPI AST_Declaration *ast_run_directive_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Directive *run_directive);
ZAPI AST_Declaration *ast_declaration_new(Zodiac_Context *ctx, Source_Range range);

ZAPI AST_Type_Spec *ast_type_ts_new(Zodiac_Context *ctx, Source_Range range, Type *type);
ZAPI AST_Type_Spec *ast_name_ts_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident);
ZAPI AST_Type_Spec *ast_pointer_ts_new(Zodiac_Context *ctx, Source_Range range, AST_Type_Spec *base);
ZAPI AST_Type_Spec *ast_static_array_ts_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *length_expr, AST_Type_Spec *element_ts);
ZAPI AST_Type_Spec *ast_type_spec_new(Zodiac_Context *ctx, Source_Range range);

ZAPI AST_Directive *ast_run_directive_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *expr);
ZAPI AST_Directive *ast_run_directive_new(Zodiac_Context *ctx, Source_Range range, AST_Statement *stmt);
ZAPI AST_Directive *ast_directive_new(Zodiac_Context *ctx, Source_Range range);

ZAPI AST_File *ast_file_new(Zodiac_Context *ctx, Dynamic_Array<AST_Declaration *> decls);

ZAPI void ast_print_expression(String_Builder *sb, AST_Expression *expr);
ZAPI String ast_print_expression(AST_Expression *expr, Allocator *allocator);

ZAPI void ast_print_statement(String_Builder *sb, AST_Statement *stmt, int indent = 0);

ZAPI void ast_print_declaration(String_Builder *sb, AST_Declaration *decl, int indent = 0);
ZAPI void ast_print_declaration(AST_Declaration *decl);

ZAPI void ast_print_type_spec(String_Builder *sb, AST_Type_Spec *ts, int indent = 0);

ZAPI void ast_print_file(String_Builder *sb, AST_File *file);
ZAPI void ast_print_file(AST_File *file);

ZAPI bool is_binary_arithmetic_op(AST_Binary_Operator op);
ZAPI bool is_binary_cmp_op(AST_Binary_Operator op);

}