#pragma once

#include <defines.h>

#include <token.h>
#include <zodiac_context.h>
#include <zstring.h>

namespace Zodiac
{

struct Lexer
{
    Zodiac_Context *context;

    const char *stream_start;
    const char *stream;

    Token token;
};

ZAPI void lexer_create(Zodiac_Context *context, Lexer *out_lexer);
ZAPI void lexer_init_stream(Lexer *lexer, const char *stream);
ZAPI void lexer_destroy(Lexer *lexer);

ZAPI void next_token(Lexer *lexer);
ZAPI bool is_token(Lexer *lexer, Token_Kind kind);
ZAPI void print_token(Token token);
ZAPI String_Ref tmp_token_string(Token token);
ZAPI const char *token_kind_str(Token_Kind kind);

}
