#pragma once

#include <defines.h>

#include <ast.h>
#include <lexer.h>

namespace Zodiac
{

struct Parser
{
    Zodiac_Context *context;
    Lexer *lxr;
};

ZAPI void parser_create(Zodiac_Context *ctx, Lexer *lxr, Parser *out_parser);

// expr_operand = INT | NAME | '(' expr ')'
// call_args = (expr ( ',' expr )* )*
// expr_base = expr_operand ( '(' call_args ')' | '[' expr ']' | '.' NAME )*
// expr_unary = ([+-] expr_unary) | expr_base
// expr_mul = expr_unary ([/*] expr_unary)*
// expr_add = expr_mul ([+-] expr_mul)*
// expr = expr_add;

ZAPI AST_Expression *parse_expr_operand(Parser *parser);
ZAPI AST_Expression *parse_expr_base(Parser *parser);
ZAPI AST_Expression *parse_expr_unary(Parser *parser);
ZAPI AST_Expression *parse_expr_mul(Parser *parser);
ZAPI AST_Expression *parse_expr_add(Parser *parser);
ZAPI AST_Expression *parse_expression(Parser *parser);

ZAPI AST_Statement *parse_statement(Parser *parser);

ZAPI bool is_token(Parser *parser, Token_Kind kind);
ZAPI bool is_token(Parser *parser, char c);
ZAPI bool next_token(Parser *parser);
ZAPI bool match_token(Parser *parser, Token_Kind kind);
ZAPI bool match_token(Parser *parser, char c);
ZAPI bool expect_token(Parser *parser, Token_Kind kind);
ZAPI bool expect_token(Parser *parser, char c);
ZAPI Token cur_tok(Parser *parser);


}
