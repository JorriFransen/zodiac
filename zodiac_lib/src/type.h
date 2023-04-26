#pragma once

#include "defines.h"

namespace Zodiac
{

struct AST_Declaration;

enum class Type_Kind
{
    UNSIZED_INTEGER,
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

ZAPI extern Type UNSIZED_INTEGER_TYPE;

ZAPI void create_type(Type *type, Type_Kind kind, u64 bit_size);
ZAPI void create_integer_type(Type *type, u64 bit_size, bool sign);
ZAPI void create_float_type(Type *type, u64 bit_size);

ZAPI Type *decl_type(AST_Declaration *decl);

ZAPI bool valid_static_type_conversion(Type *from, Type *to);

}
