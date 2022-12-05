#pragma once

#include <common.h>
#include <containers/dynamic_array.h>
#include <lexer.h>
#include <string_builder.h>
#include <zodiac_context.h>

namespace Zodiac
{

struct AST_Expression;
struct AST_Statement;

struct AST_Integer_Literal_Expression
{
    Integer_Value value;
};

struct AST_Identifier_Expression
{
    Atom atom;
};

struct AST_Member_Expression
{
    AST_Expression *base;
    Atom member_name;
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

    PLUS = '+',
    MINUS = '-',
};

struct AST_Unary_Expression
{
    AST_Unary_Operator op;
    AST_Expression *operand;
};

enum class AST_Binary_Operator
{
    INVALID = 0,

    ADD,
    SUB,
    MUL,
    DIV,

    EQ,
    NEQ,
    LT,
    GT,
    LTEQ,
    GTEQ,

    LAST_BINOP = GTEQ,
};

struct AST_Binary_Expression
{
    AST_Binary_Operator op;
    AST_Expression *lhs;
    AST_Expression *rhs;
};

enum class AST_Expression_Kind
{
    INVALID,

    INTEGER_LITERAL,
    IDENTIFIER,

    MEMBER,
    INDEX,
    CALL,

    UNARY,
    BINARY,
};

struct AST_Expression
{
    AST_Expression_Kind kind;

    union
    {
        AST_Integer_Literal_Expression integer_literal;
        AST_Identifier_Expression identifier;
        AST_Member_Expression member;
        AST_Index_Expression index;
        AST_Call_Expression call;
        AST_Unary_Expression unary;
        AST_Binary_Expression binary;
    };
};

struct AST_Block_Statement
{
    Dynamic_Array<AST_Statement *> statements;
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

struct AST_Else_If
{
    AST_Expression *cond;
    AST_Statement *then;
};

struct AST_If_Statement
{
    AST_Expression *cond;
    AST_Statement *then_stmt;
    Dynamic_Array<AST_Else_If> else_ifs;
    AST_Statement *else_stmt;
};

struct AST_While_Statement
{
    AST_Expression *cond;
    AST_Statement *do_stmt;
};

struct AST_Return_Statement
{
    AST_Expression *value;
};

enum class AST_Statement_Kind
{
    INVALID,

    BLOCK,

    ASSIGN,
    CALL,

    IF,
    WHILE,

    RETURN,
};

struct AST_Statement
{
    AST_Statement_Kind kind;

    union
    {
        AST_Block_Statement block;
        AST_Assign_Statement assign;
        AST_Call_Statement call;
        AST_If_Statement if_stmt;
        AST_While_Statement while_stmt;
        AST_Return_Statement return_stmt;
    };
};

ZAPI void ast_integer_literal_expr_create(Integer_Value value, AST_Expression *out_expr);
ZAPI void ast_identifier_expr_create(Atom atom, AST_Expression *out_expr);
ZAPI void ast_member_expr_create(AST_Expression *base, Atom atom, AST_Expression *out_expr);
ZAPI void ast_call_expr_create(AST_Expression *base, Dynamic_Array<AST_Expression *> args, AST_Expression *out_expr);
ZAPI void ast_index_expr_create(AST_Expression *base, AST_Expression *index, AST_Expression *out_expr);
ZAPI void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Expression *out_expr);
ZAPI void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Expression *out_expr);
ZAPI void ast_expression_create(AST_Expression_Kind kind, AST_Expression *out_expr);

ZAPI void ast_block_stmt_create(Dynamic_Array<AST_Statement *> statements, AST_Statement *out_stmt);
ZAPI void ast_assign_stmt_create(AST_Expression *dest, AST_Expression *value, AST_Statement *out_stmt);
ZAPI void ast_call_stmt_create(AST_Expression *call, AST_Statement *out_stmt);
ZAPI void ast_if_stmt_create(AST_Expression *cond, AST_Statement *then_stmt, Dynamic_Array<AST_Else_If> else_ifs, AST_Statement *else_stmt, AST_Statement *out_stmt);
ZAPI void ast_while_stmt_create(AST_Expression *cond, AST_Statement *do_stmt, AST_Statement *out_stmt);
ZAPI void ast_return_stmt_create(AST_Expression *value, AST_Statement *out_stmt);
ZAPI void ast_statement_create(AST_Statement_Kind kind, AST_Statement *out_stmt);

ZAPI AST_Expression *ast_integer_literal_expr_new(Zodiac_Context *ctx, Integer_Value value);
ZAPI AST_Expression *ast_identifier_expr_new(Zodiac_Context *ctx, Atom atom);
ZAPI AST_Expression *ast_member_expr_new(Zodiac_Context *ctx, AST_Expression *base, Atom atom);
ZAPI AST_Expression *ast_index_expr_new(Zodiac_Context *ctx, AST_Expression *base, AST_Expression *index);
ZAPI AST_Expression *ast_call_expr_new(Zodiac_Context *ctx, AST_Expression *base, Dynamic_Array<AST_Expression *> args);
ZAPI AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, AST_Unary_Operator op, AST_Expression *operand);
ZAPI AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs);
ZAPI AST_Expression *ast_expression_new(Zodiac_Context *ctx);

ZAPI AST_Statement *ast_block_stmt_new(Zodiac_Context *ctx, Dynamic_Array<AST_Statement *> statements);
ZAPI AST_Statement *ast_assign_stmt_new(Zodiac_Context *ctx, AST_Expression *dest, AST_Expression *value);
ZAPI AST_Statement *ast_call_stmt_new(Zodiac_Context *ctx, AST_Expression *call);
ZAPI AST_Statement *ast_if_stmt_new(Zodiac_Context *ctx, AST_Expression *cond, AST_Statement *then_stmt, Dynamic_Array<AST_Else_If> else_ifs, AST_Statement *else_stmt);
ZAPI AST_Statement *ast_while_stmt_new(Zodiac_Context *ctx, AST_Expression *cond, AST_Statement *do_stmt);
ZAPI AST_Statement *ast_return_stmt_new(Zodiac_Context *ctx, AST_Expression *value);
ZAPI AST_Statement *ast_statement_new(Zodiac_Context *ctx);

ZAPI void ast_print_expression(String_Builder *sb, AST_Expression *expr);
ZAPI void ast_print_statement(String_Builder *sb, AST_Statement *stmt, int indent = 0);

}
