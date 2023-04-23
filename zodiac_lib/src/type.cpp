#include "type.h"

#include "asserts.h"

namespace Zodiac
{

void create_type(Type *type, Type_Kind kind, u64 bit_size)
{
    assert(type);
    assert(bit_size % 8 == 0);

    type->kind = kind;
    type->bit_size = bit_size;
}

void create_integer_type(Type *type, u64 bit_size, bool sign)
{
    assert(type);
    assert(bit_size % 8 == 0);

    create_type(type, Type_Kind::INTEGER, bit_size);
    type->integer.sign = sign;
}

void create_float_type(Type *type, u64 bit_size)
{
    assert(type);
    assert(bit_size % 8 == 0);

    create_type(type, Type_Kind::FLOAT, bit_size);
}

}
