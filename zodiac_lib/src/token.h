#pragma once

#include <common.h>
#include <defines.h>

namespace Zodiac
{

enum Token_Kind
{
    TOK_INVALID = 0,

    // Ascii range

    TOK_NUMBER = 128,
    TOK_NAME,
    TOK_KEYWORD,
    TOK_EOF,
};

struct Token
{
    Token_Kind kind;

    const char *start;
    const char *end;

    union
    {
        Integer_Value number;
    };
};

}
