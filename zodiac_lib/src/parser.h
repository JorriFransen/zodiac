#pragma once

#include <defines.h>
#include <stdarg.h>

#include "ast.h"
#include "atom.h"
#include "containers/queue.h"
#include "lexer.h"
#include "util/zstring.h"

namespace Zodiac
{

struct Zodiac_Context;

struct Parser
{
    Zodiac_Context *context;
    Lexer *lxr;

    bool error = true;

    Queue<Token> peeked_tokens;
};

ZAPI void parser_create(Zodiac_Context *ctx, Lexer *lxr, Parser *out_parser);

// expr_operand = INT | NAME | '(' expr ')' | 'null'
// call_args = (expr ( ',' expr )* )*
// expr_base = expr_operand ( '(' call_args ')' | '[' expr ']' | '.' NAME )*
// expr_unary = ([+-] expr_unary) | expr_base
// expr_mul = expr_unary ([/*] expr_unary)*
// expr_add = expr_mul ([+-] expr_mul)*
// expr_cmp = expr_add (([<>] | '==' | '!=' | '<=' | '>=') expr_add )*
// expr = expr_cmp;

ZAPI AST_Identifier parse_identifier(Parser *parser);

ZAPI AST_Expression *parse_expr_operand(Parser *parser);
ZAPI AST_Expression *parse_expr_base(Parser *parser);
ZAPI AST_Expression *parse_expr_unary(Parser *parser);
ZAPI AST_Expression *parse_expr_mul(Parser *parser);
ZAPI AST_Expression *parse_expr_add(Parser *parser);
ZAPI AST_Expression *parse_expr_cmp(Parser *parser);
ZAPI AST_Expression *parse_expression(Parser *parser);

ZAPI AST_Statement *parse_keyword_statement(Parser *parser);
ZAPI AST_Statement *parse_statement(Parser *parser);

ZAPI AST_Declaration *parse_function_declaration(Parser *parser, AST_Identifier ident);
ZAPI AST_Declaration *parse_aggregate_decl(Parser *parser, AST_Identifier ident);
ZAPI AST_Declaration *parse_declaration(Parser *parser);

#define ZODIAC_BUILTIN_TYPES         \
    ZODIAC_NUMERIC_TYPE_DEF(s, 64)   \
    ZODIAC_NUMERIC_TYPE_DEF(s, 8)    \
    ZODIAC_NUMERIC_TYPE_DEF(u, 32)   \
    ZODIAC_NUMERIC_TYPE_DEF(u, 16)   \
    ZODIAC_NUMERIC_TYPE_DEF(r, 32)   \
    ZODIAC_NAME_TYPE_DEF(String)     \

// Builtin type atoms
#define ZODIAC_NUMERIC_TYPE_DEF(type, size) ZAPI extern Atom atom_##type##size;
#define ZODIAC_NAME_TYPE_DEF(name) ZAPI extern Atom atom_##name;
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_NAME_TYPE_DEF
#undef ZODIAC_NUMERIC_TYPE_DEF



ZAPI AST_Type_Spec *parse_type_spec(Parser *parser);

ZAPI AST_File *parse_file(Parser *parser);

ZAPI bool is_keyword(Parser *parser, Atom keyword);
ZAPI bool match_keyword(Parser *parser, Atom keyword);
ZAPI bool expect_keyword(Parser *parser, Atom keyword);

ZAPI bool is_token(Parser *parser, Token_Kind kind);
ZAPI bool is_token(Parser *parser, char c);
ZAPI bool next_token(Parser *parser);
ZAPI bool match_token(Parser *parser, Token_Kind kind);
ZAPI bool match_token(Parser *parser, char c);
ZAPI bool expect_token(Parser *parser, Token_Kind kind);
ZAPI bool expect_token(Parser *parser, char c);
ZAPI Token cur_tok(Parser *parser);
ZAPI Token peek_token(Parser *parser, u64 offset = 1);

ZAPI void syntax_error(Parser *parser, const String_Ref fmt, ...);
ZAPI void syntax_error(Parser *parser, const String_Ref fmt, va_list args);

ZAPI void fatal_syntax_error(Parser *parser, const String_Ref fmt, ...);


}
