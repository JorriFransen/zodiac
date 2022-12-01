#include "ast.h"

#include <asserts.h>
#include <string_builder.h>

namespace Zodiac
{

void ast_integer_literal_expr_create(Integer_Value value, AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::INTEGER_LITERAL, out_expr);

    out_expr->integer_literal.value = value;
}

void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Expression *out_expr)
{
    assert(operand && out_expr);
    assert(op != AST_Unary_Operator::INVALID);

    ast_expression_create(AST_Expression_Kind::UNARY, out_expr);

    out_expr->unary.op = op;
    out_expr->unary.operand = operand;
}

void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Expression *out_expr)
{
    assert(lhs && rhs && out_expr);
    assert(op != AST_Binary_Operator::INVALID);

    ast_expression_create(AST_Expression_Kind::BINARY, out_expr);

    out_expr->binary.op = op;
    out_expr->binary.lhs = lhs;
    out_expr->binary.rhs = rhs;
}

void ast_expression_create(AST_Expression_Kind kind, AST_Expression *out_expr)
{
    out_expr->kind = kind;
}

AST_Expression *ast_integer_literal_expr_new(Zodiac_Context *ctx, Integer_Value value)
{
    auto expr = ast_expression_new(ctx);
    ast_integer_literal_expr_create(value, expr);
    return expr;
}

AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, AST_Unary_Operator op, AST_Expression *operand)
{
    auto expr = ast_expression_new(ctx);
    ast_unary_expr_create(op, operand, expr);
    return expr;
}

AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs)
{
    auto expr = ast_expression_new(ctx);
    ast_binary_expr_create(op, lhs, rhs, expr);
    return expr;
}

AST_Expression *ast_expression_new(Zodiac_Context *ctx)
{
    return alloc<AST_Expression>(ctx->expression_allocator);
}

void ast_print_expression(String_Builder *sb, AST_Expression *expr)
{
    assert(sb && expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL: {
            string_builder_append(sb, "%llu", expr->integer_literal.value.u64);
            break;
        }

        case AST_Expression_Kind::UNARY: {
            string_builder_append(sb, "%c(", (char)expr->unary.op);
            ast_print_expression(sb, expr->unary.operand);
            string_builder_append(sb, ")");
            break;
        }

        case AST_Expression_Kind::BINARY: {
            string_builder_append(sb, "%c(", (char)expr->binary.op);
            ast_print_expression(sb, expr->binary.lhs);
            string_builder_append(sb, ", ");
            ast_print_expression(sb, expr->binary.rhs);
            string_builder_append(sb, ")");
            break;
        }

    }
}

}
