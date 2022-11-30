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

    TOK_INT = 128,
    TOK_REAL,
    TOK_NAME,
    TOK_KEYWORD,

    TOK_EQ,
    TOK_NEQ,
    TOK_LTEQ,
    TOK_GTEQ,

    TOK_EOF,
};

struct Token
{
    Token_Kind kind;

    Atom atom;

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

    Token token;
};

#define ALL_ZODIAC_KEYWORDS \
    ZODIAC_KEYWORD(for)     \
    ZODIAC_KEYWORD(sizeof)  \
    ZODIAC_KEYWORD(struct)

// Emit variable for all keywords
#define ZODIAC_KEYWORD(n) extern Atom keyword_##n;
ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

ZAPI void zodiac_register_keywords(Atom_Table *at);
ZAPI bool is_keyword(const Atom &atom);


ZAPI void lexer_create(Zodiac_Context *context, Lexer *out_lexer);
ZAPI void lexer_init_stream(Lexer *lexer, const char *stream);
ZAPI void lexer_destroy(Lexer *lexer);

ZAPI void next_token(Lexer *lexer);
ZAPI bool is_token(Lexer *lexer, Token_Kind kind);
ZAPI void print_token(Token token);
ZAPI String_Ref tmp_token_string(Token token);
ZAPI const char *token_kind_str(Token_Kind kind);

}
