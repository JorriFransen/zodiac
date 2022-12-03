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

void ast_identifier_expr_create(Atom atom, AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::IDENTIFIER, out_expr);

    out_expr->identifier.atom = atom;
}

void ast_member_expr_create(AST_Expression *base, Atom atom, AST_Expression *out_expr)
{
    assert(base && out_expr);

    ast_expression_create(AST_Expression_Kind::MEMBER, out_expr);

    out_expr->member.base = base;
    out_expr->member.member_name = atom;
}

void ast_index_expr_create(AST_Expression *base, AST_Expression *index, AST_Expression *out_expr)
{
    assert(base && index && out_expr);

    ast_expression_create(AST_Expression_Kind::INDEX, out_expr);

    out_expr->index.base = base;
    out_expr->index.index = index;
}

void ast_call_expr_create(AST_Expression *base, Dynamic_Array<AST_Expression *> args, AST_Expression *out_expr)
{
    assert(base && out_expr);

    ast_expression_create(AST_Expression_Kind::CALL, out_expr);

    out_expr->call.base = base;
    out_expr->call.args = args;
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
    assert(ctx);

    auto expr = ast_expression_new(ctx);
    ast_integer_literal_expr_create(value, expr);
    return expr;
}

AST_Expression *ast_identifier_expr_new(Zodiac_Context *ctx, Atom atom)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx);
    ast_identifier_expr_create(atom, expr);
    return expr;
}

AST_Expression *ast_member_expr_new(Zodiac_Context *ctx, AST_Expression *base, Atom atom)
{
    assert(ctx && base);

    auto expr = ast_expression_new(ctx);
    ast_member_expr_create(base, atom, expr);
    return expr;
}

AST_Expression *ast_index_expr_new(Zodiac_Context *ctx, AST_Expression *base, AST_Expression *index)
{
    assert(ctx && base && index);

    auto expr = ast_expression_new(ctx);
    ast_index_expr_create(base, index, expr);
    return expr;
}

AST_Expression *ast_call_expr_new(Zodiac_Context *ctx, AST_Expression *base, Dynamic_Array<AST_Expression *> args)
{
    assert(ctx && base);

    auto expr = ast_expression_new(ctx);
    ast_call_expr_create(base, args, expr);
    return expr;

}

AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, AST_Unary_Operator op, AST_Expression *operand)
{
    assert(ctx && operand);

    auto expr = ast_expression_new(ctx);
    ast_unary_expr_create(op, operand, expr);
    return expr;
}

AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs)
{
    assert(ctx && lhs && rhs);

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
            string_builder_append(sb, "%i", expr->integer_literal.value.u64);
            break;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            string_builder_append(sb, "%s", expr->identifier.atom.data);
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            ast_print_expression(sb, expr->member.base);
            string_builder_append(sb, ".%s", expr->member.member_name.data);
            break;
        }

        case AST_Expression_Kind::INDEX: {
            ast_print_expression(sb, expr->index.base);
            string_builder_append(sb, "[");
            ast_print_expression(sb, expr->index.index);
            string_builder_append(sb, "]");
            break;
        }

        case AST_Expression_Kind::CALL: {
            ast_print_expression(sb, expr->index.base);
            string_builder_append(sb, "(");
            for (u64 i = 0; i < expr->call.args.count; i++) {
                if (i != 0) string_builder_append(sb, ", ");
                ast_print_expression(sb, expr->call.args[i]);
            }
            string_builder_append(sb, ")");
            break;
        }

        case AST_Expression_Kind::UNARY: {
            string_builder_append(sb, "%c", (char)expr->unary.op);
            ast_print_expression(sb, expr->unary.operand);
            break;
        }

        case AST_Expression_Kind::BINARY: {
            string_builder_append(sb, "(");
            ast_print_expression(sb, expr->binary.lhs);
            string_builder_append(sb, " %c ", (char)expr->binary.op);
            ast_print_expression(sb, expr->binary.rhs);
            string_builder_append(sb, ")");
            break;
        }
    }
}

void ast_print_statement(String_Builder *sb, AST_Statement *stmt)
{
    assert(sb && stmt);

    assert_msg(false, "TODO: Implement ast_print_statement");
}

}
