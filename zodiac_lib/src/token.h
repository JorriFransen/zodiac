#pragma once

#include <common.h>
#include <defines.h>
#include <zstring.h>

namespace Zodiac
{

enum Token_Kind
{
    TOK_INVALID = 0,

    // Ascii range

    TOK_NUMBER = 128,
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
        Integer_Value number;
    };
};

}
