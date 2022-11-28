#pragma once

#include <defines.h>

#include <token.h>
#include <zstring.h>

namespace Zodiac
{

struct Lexer
{
    const char *stream_start;
    const char *stream;

    Token token;
};

ZAPI void lexer_create(const char *stream, Lexer *out_lexer);
ZAPI void lexer_destroy(Lexer *lexer);

ZAPI void next_token(Lexer *lexer);
ZAPI bool is_token(Lexer *lexer, Token_Kind kind);
ZAPI void print_token(Token token);
ZAPI const char *token_kind_str(Token_Kind kind);

}
