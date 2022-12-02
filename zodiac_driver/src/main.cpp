
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <ast.h>
#include <lexer.h>

#include <memory/allocator.h>
#include <memory/pool_allocator.h>

using namespace Zodiac;

file_local AST_Expression *parse_expr_operand();
file_local AST_Expression *parse_expr_base();
file_local AST_Expression *parse_expr2();
file_local AST_Expression *parse_expr1();
file_local AST_Expression *parse_expression();

Zodiac_Context *ctx;
Lexer *lxr;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Pool_Allocator expression_pool;
    pool_allocator_create(1024, sizeof(AST_Expression), &expression_pool);
    Allocator expr_allocator = pool_allocator_allocator(&expression_pool);

    Zodiac_Context c;
    zodiac_context_create(&expr_allocator, &c);
    ctx = &c;

    Lexer lexer;
    lexer_create(&c, &lexer);
    // lexer_init_stream(&lexer, "1 + x * -3");
    lexer_init_stream(&lexer, "abc.def");


    lxr = &lexer;
    auto result = parse_expression();

    String_Builder sb;
    string_builder_create(&sb);

    ast_print_expression(&sb, result);
    string_builder_append(&sb, "\n");

    String ast_str = string_builder_to_string(&sb);
    printf("%.*s", (int)ast_str.length, ast_str.data);
    free(sb.allocator, ast_str.data);

    string_builder_destroy(&sb);

    return 0;
}

// expr_operand = INT | NAME | '(' expr ')'
// expr_base = expr_operand ( '(' call_args ')' | '[' expr ']' | '.' NAME )*
// expr2 = ([+-] expr2) | expr_base
// expr1 = expr2 ([/*] expr2)*
// expr = expr1 ([+-] expr1)*

file_local AST_Expression *parse_expr_operand()
{
    if (is_token(lxr, TOK_INT)) {
        u64 value = lxr->token.integer;
        next_token(lxr);

        return ast_integer_literal_expr_new(ctx, { .u64 = value });
    } else if (is_token(lxr, TOK_NAME)) {
        Atom name_atom = lxr->token.atom;
        next_token(lxr);

        return ast_identifier_expr_new(ctx, name_atom);
    } else {

        if (match_token(lxr, '(')) {
            auto result = parse_expression();
            expect_token(lxr, ')');
            return result;
        } else {
            ZFATAL("Expected '('");
            return 0;
        }
    }
}

file_local AST_Expression *parse_expr_base()
{
    AST_Expression *expr = parse_expr_operand();

    while (is_token(lxr, '(') || is_token(lxr, '[') || is_token(lxr, '.')) {

        if (match_token(lxr, '(')) {
            assert_msg(false, "TODO: Implement");
        } else if (match_token(lxr, '[')) {
            assert_msg(false, "TODO: Implement");
        } else if (match_token(lxr, '.')) {
            assert_msg(false, "TODO: Implement");
        } else {
            auto tmp_tok_str = tmp_token_string(lxr->token);
            ZFATAL("Unexpected token: '%.*s'", (int)tmp_tok_str.length, tmp_tok_str.data);
        }
    }

    return expr;
}

file_local AST_Expression *parse_expr2()
{
    if (is_token(lxr, '+')) {
        next_token(lxr);
        return parse_expr2();
    } else if (is_token(lxr, '-')) {
        next_token(lxr);
        AST_Expression *operand = parse_expr2();
        return ast_unary_expr_new(ctx, AST_Unary_Operator::MINUS, operand);
    } else {
        return parse_expr_base();
    }
}

static AST_Binary_Operator char_to_ast_binop[256] = {
    ['+'] = AST_Binary_Operator::ADD,
    ['-'] = AST_Binary_Operator::SUB,
    ['*'] = AST_Binary_Operator::MUL,
    ['/'] = AST_Binary_Operator::DIV,
};

file_local AST_Expression *parse_expr1()
{
    AST_Expression *lhs = parse_expr2();

    while (is_token(lxr, '/') || is_token(lxr, '*')) {
        char op = lxr->token.kind;
        next_token(lxr);

        AST_Expression *rhs = parse_expr2();

        auto ast_op = char_to_ast_binop[op];
        assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(ctx, ast_op, lhs, rhs);
    }

    return lhs;
}

file_local AST_Expression *parse_expression()
{
    AST_Expression *lhs = parse_expr1();

    while (is_token(lxr, '+') || is_token(lxr, '-')) {
        char op = lxr->token.kind;
        next_token(lxr);

        AST_Expression *rhs = parse_expr1();

        auto ast_op = char_to_ast_binop[op];
        assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(ctx, ast_op, lhs, rhs);
    }

    return lhs;
}
