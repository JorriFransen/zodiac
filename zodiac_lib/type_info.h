#pragma once

#include "defines.h"

namespace Zodiac
{

struct Type;
struct Zodiac_Context;

enum class Type_Info_Kind
{
    INVALID = 0,

    VOID    = 1,
    INTEGER = 2,
    REAL    = 3,
    BOOL    = 4,
};

struct Type_Info
{
    Type_Info_Kind kind;
    s64 byte_size;
};

struct Type_Info_Int
{
    Type_Info base;
    bool sign;
};

ZAPI void add_type_info(Zodiac_Context *ctx, Type *type);
ZAPI void init_type_info_base(Type_Info *ti, Type_Info_Kind kind, s64 bit_size);

}
