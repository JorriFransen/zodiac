#include "parser.h"

#include <containers/dynamic_array.h>
#include <logger.h>

namespace Zodiac
{

void parser_create(Zodiac_Context *ctx, Lexer *lxr, Parser *out_parser)
{
    assert(ctx && lxr && out_parser);
    out_parser->context = ctx;
    out_parser->lxr = lxr;
}

// call_args = (expr ( ',' expr )* )*
// expr_operand = INT | NAME | '(' expr ')'
// expr_base = expr_operand ( '(' call_args ')' | '[' expr ']' | '.' NAME )*
// expr_unary = ([+-] expr_unary) | expr_base
// expr_mul = expr_unary ([/*] expr_unary)*
// expr_add = expr_mul ([+-] expr_mul)*
// expr = expr_add;

AST_Expression *parse_expr_operand(Parser *parser)
{
    assert(parser);

    if (is_token(parser, TOK_INT)) {
        u64 value = cur_tok(parser).integer;
        next_token(parser);

        return ast_integer_literal_expr_new(parser->context, { .u64 = value });
    } else if (is_token(parser, TOK_NAME)) {
        Atom name_atom = cur_tok(parser).atom;
        next_token(parser);

        return ast_identifier_expr_new(parser->context, name_atom);
    } else {

        if (match_token(parser, '(')) {
            auto result = parse_expression(parser);
            expect_token(parser, ')');
            return result;
        } else {
            ZFATAL("Expected '('");
            return 0;
        }
    }
}

AST_Expression *parse_expr_base(Parser *parser)
{
    AST_Expression *expr = parse_expr_operand(parser);

    while (is_token(parser, '(') || is_token(parser, '[') || is_token(parser, '.')) {

        if (match_token(parser, '(')) {

            Dynamic_Array<AST_Expression*> args;
            dynamic_array_create(&dynamic_allocator, &args, 0);

            while (!match_token(parser, ')')) {

                if (args.count != 0) {
                    expect_token(parser, ',');
                }

                AST_Expression *arg = parse_expression(parser);
                dynamic_array_append(&args, arg);
            }

            expr = ast_call_expr_new(parser->context, expr, args);


        } else if (match_token(parser, '[')) {

            AST_Expression *index = parse_expression(parser);
            expect_token(parser, ']');
            expr = ast_index_expr_new(parser->context, expr, index);


        } else if (match_token(parser, '.')) {

            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);
            expr = ast_member_expr_new(parser->context, expr, name_tok.atom);

        } else {
            auto tmp_tok_str = tmp_token_string(cur_tok(parser));
            ZFATAL("Unexpected token: '%.*s'", (int)tmp_tok_str.length, tmp_tok_str.data);
        }
    }

    return expr;
}

AST_Expression *parse_expr_unary(Parser *parser)
{
    if (is_token(parser, '+')) {
        next_token(parser);
        return parse_expr_unary(parser);
    } else if (is_token(parser, '-')) {
        next_token(parser);
        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, AST_Unary_Operator::MINUS, operand);
    } else {
        return parse_expr_base(parser);
    }
}

file_local AST_Binary_Operator char_to_ast_binop[256] = {
    ['+'] = AST_Binary_Operator::ADD,
    ['-'] = AST_Binary_Operator::SUB,
    ['*'] = AST_Binary_Operator::MUL,
    ['/'] = AST_Binary_Operator::DIV,
};

AST_Expression *parse_expr_mul(Parser *parser)
{
    AST_Expression *lhs = parse_expr_unary(parser);

    while (is_token(parser, '*') || is_token(parser, '/')) {
        char op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_unary(parser);

        auto ast_op = char_to_ast_binop[(int)op];
        assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expr_add(Parser *parser)
{
    AST_Expression *lhs = parse_expr_mul(parser);

    while (is_token(parser, '+') || is_token(parser, '-')) {
        char op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_mul(parser);

        auto ast_op = char_to_ast_binop[(int)op];
        assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expression(Parser *parser)
{
    return parse_expr_add(parser);
}


bool is_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);
    return is_token(parser->lxr, kind);
}

bool is_token(Parser *parser, char c)
{
    assert(parser && parser->lxr);
    return is_token(parser->lxr, c);
}

bool next_token(Parser *parser)
{
    assert(parser && parser->lxr);
    return next_token(parser->lxr);
}

bool match_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);
    return match_token(parser->lxr, kind);
}

bool match_token(Parser *parser, char c)
{
    assert(parser && parser->lxr);
    return match_token(parser->lxr, c);
}

bool expect_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);
    return expect_token(parser->lxr, kind);
}

bool expect_token(Parser *parser, char c)
{
    assert(parser && parser->lxr);
    return expect_token(parser->lxr, c);
}

Token cur_tok(Parser *parser)
{
    assert(parser && parser->lxr);
    return parser->lxr->token;
}

}
