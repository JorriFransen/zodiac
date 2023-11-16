#pragma once

#include "atom.h"
#include "common.h"
#include "defines.h"
#include "source_pos.h"
#include "util/zstring.h"

namespace Zodiac
{

enum Token_Kind
{
    TOK_INVALID = 0,

    // Ascii range (mostly here so the character literals can be used in switch statements)
    TOK_HASH   = '#', // 35
    TOK_LPAREN = '(', // 40
    TOK_RPAREN = ')', // 41
    TOK_STAR   = '*', // 42
    TOK_DOT    = '.', // 46
    TOK_LBRACK = '[', // 91
    TOK_RBRACK = ']', // 93

    TOK_INT = 256,
    TOK_REAL,
    TOK_STRING,
    TOK_CHAR,
    TOK_NAME,
    TOK_KEYWORD,

    TOK_RIGHT_ARROW,
    TOK_DOT_DOT,

    TOK_EQ,
    TOK_NEQ,
    TOK_LTEQ,
    TOK_GTEQ,

    TOK_EOF,
    TOK_LAST = TOK_EOF,
};

struct Token
{
    Token_Kind kind;

    Atom atom;

    Source_Range sr;

    union
    {
        u64 integer;
        Real_Value real;
        char character;
    };
};

struct Zodiac_Context;

struct Lexer
{
    Zodiac_Context *context;

    String_Ref stream_name;

    const char *stream_start;
    const char *stream;
    const char *line_start;

    Token token;
};

#define ALL_ZODIAC_KEYWORDS  \
    ZODIAC_KEYWORD(if)       \
    ZODIAC_KEYWORD(else)     \
    ZODIAC_KEYWORD(return)   \
    ZODIAC_KEYWORD(while)    \
    ZODIAC_KEYWORD(for)      \
    ZODIAC_KEYWORD(switch)   \
    ZODIAC_KEYWORD(case)     \
    ZODIAC_KEYWORD(default)  \
    ZODIAC_KEYWORD(break)    \
    ZODIAC_KEYWORD(continue) \
    ZODIAC_KEYWORD(sizeof)   \
    ZODIAC_KEYWORD(struct)   \
    ZODIAC_KEYWORD(union)    \
    ZODIAC_KEYWORD(enum)     \
    ZODIAC_KEYWORD(true)     \
    ZODIAC_KEYWORD(false)    \
    ZODIAC_KEYWORD(defer)    \
    ZODIAC_KEYWORD(cast)     \
    ZODIAC_KEYWORD(null)

// Emit atom declarations for all keywords
#define ZODIAC_KEYWORD(n) ZAPI extern Atom keyword_##n;
ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

#define ALL_ZODIAC_DIRECTIVES    \
    ZODIAC_DIRECTIVE(run)        \
    ZODIAC_DIRECTIVE(foreign)    \
    ZODIAC_DIRECTIVE(import)     \
    ZODIAC_DIRECTIVE(falltrough) \
    ZODIAC_DIRECTIVE(type_info)  \
    ZODIAC_DIRECTIVE(type_of)    \
    ZODIAC_DIRECTIVE(type)       \

// Emit atom declarations for all directives
#define ZODIAC_DIRECTIVE(n) ZAPI extern Atom directive_##n;
ALL_ZODIAC_DIRECTIVES
#undef ZODIAC_DIRECTIVE

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
