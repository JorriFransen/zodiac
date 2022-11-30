#include "ast.h"

namespace Zodiac
{


void ast_integer_literal_expr_create(Integer_Value value, AST_Integer_Literal_Expression *out_expr)
{
    out_expr->value = value;
}

void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Unary_Expression *out_expr)
{
    out_expr->op = op;
    out_expr->operand = operand;
}

void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Binary_Expression *out_expr)
{
    out_expr->op = op;
    out_expr->lhs = lhs;
    out_expr->rhs = rhs;
}

void ast_expression_create(AST_Expression_Kind kind, AST_Expression *out_expr)
{
    out_expr->kind = kind;
}

}
