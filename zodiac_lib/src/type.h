#pragma once

#include "defines.h"

namespace Zodiac
{

enum class Type_Kind
{
    INTEGER,
    FLOAT,
};

struct Type
{
    Type_Kind kind;
    u64 bit_size;

    union {

        struct {
            bool sign;
        } integer;

    };
};

ZAPI void create_type(Type *type, Type_Kind kind, u64 bit_size);
ZAPI void create_integer_type(Type *type, u64 bit_size, bool sign);
ZAPI void create_float_type(Type *type, u64 bit_size);

}
