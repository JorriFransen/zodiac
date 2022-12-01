#pragma once

#include <common.h>
#include <string_builder.h>
#include <zodiac_context.h>

namespace Zodiac
{

enum class AST_Expression_Kind
{
    INVALID,

    INTEGER_LITERAL,

    UNARY,
    BINARY,
};

struct AST_Expression;

struct AST_Integer_Literal_Expression
{
    Integer_Value value;
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
    INVALID,

    ADD = '+',
    SUB = '-',
    MUL = '*',
    DIV = '/',
};

struct AST_Binary_Expression
{
    AST_Binary_Operator op;
    AST_Expression *lhs;
    AST_Expression *rhs;
};

struct AST_Expression
{
    AST_Expression_Kind kind;

    union
    {
        AST_Integer_Literal_Expression integer_literal;
        AST_Unary_Expression unary;
        AST_Binary_Expression binary;
    };
};

ZAPI void ast_integer_literal_expr_create(Integer_Value value, AST_Expression *out_expr);
ZAPI void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Expression *out_expr);
ZAPI void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Expression *out_expr);
ZAPI void ast_expression_create(AST_Expression_Kind kind, AST_Expression *out_expr);


ZAPI AST_Expression *ast_integer_literal_expr_new(Zodiac_Context *ctx, Integer_Value value);
ZAPI AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, AST_Unary_Operator op, AST_Expression *operand);
ZAPI AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs);
ZAPI AST_Expression *ast_expression_new(Zodiac_Context *ctx);

ZAPI void ast_print_expression(String_Builder *sb, AST_Expression *expr);
}
