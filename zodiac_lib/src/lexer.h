#pragma once

#include <defines.h>

#include <zodiac_context.h>
#include <zstring.h>

namespace Zodiac
{

enum Token_Kind
{
    TOK_INVALID = 0,

    // Ascii range
    TOK_STAR = '*',

    TOK_INT = 128,
    TOK_REAL,
    TOK_STRING,
    TOK_NAME,
    TOK_KEYWORD,

    TOK_RIGHT_ARROW,

    TOK_EQ,
    TOK_NEQ,
    TOK_LTEQ,
    TOK_GTEQ,

    TOK_EOF,
    TOK_LAST = TOK_EOF,
};

struct Source_Pos
{
    String_Ref name;
    u64 line;
    u64 index_in_line;
};

struct Token
{
    Token_Kind kind;

    Atom atom;

    Source_Pos start;

    union
    {
        u64 integer;
        Real_Value real;
    };
};

struct Lexer
{
    Zodiac_Context *context;

    const char *stream_start;
    const char *stream;
    const char *line_start;

    Token token;
};

#define ALL_ZODIAC_KEYWORDS \
    ZODIAC_KEYWORD(if)      \
    ZODIAC_KEYWORD(else)    \
    ZODIAC_KEYWORD(return)  \
    ZODIAC_KEYWORD(while)   \
    ZODIAC_KEYWORD(sizeof)  \
    ZODIAC_KEYWORD(struct)  \
    ZODIAC_KEYWORD(union)   \
    ZODIAC_KEYWORD(print)

// Emit variable declaration for all keywords
#define ZODIAC_KEYWORD(n) ZAPI extern Atom keyword_##n;
ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

ZAPI void zodiac_register_keywords(Atom_Table *at);
ZAPI bool is_keyword(const Atom &atom);


ZAPI void lexer_create(Zodiac_Context *context, Lexer *out_lexer);
ZAPI void lexer_init_stream(Lexer *lexer, const String_Ref stream, const String_Ref stream_name);
ZAPI void lexer_destroy(Lexer *lexer);

ZAPI bool next_token(Lexer *lexer);
ZAPI bool is_token(Lexer *lexer, Token_Kind kind);
ZAPI bool is_token(Lexer *lexer, char c);
ZAPI bool match_token(Lexer *lexer, Token_Kind kind);
ZAPI bool match_token(Lexer *lexer, char c);
ZAPI bool expect_token(Lexer *lexer, Token_Kind kind);
ZAPI bool expect_token(Lexer *lexer, char c);

ZAPI void print_token(Token token);

ZAPI String_Ref tmp_token_str(Token token);
ZAPI String_Ref tmp_token_kind_str(Token_Kind kind);

}
