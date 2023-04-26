#include "type.h"

#include "asserts.h"
#include "ast.h"

namespace Zodiac
{

Type UNSIZED_INTEGER_TYPE;

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

Type *decl_type(AST_Declaration *decl)
{
    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);
        case AST_Declaration_Kind::VARIABLE: assert(false);

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            assert(decl->variable.resolved_type);
            return decl->variable.resolved_type;
            break;
        }

        case AST_Declaration_Kind::FUNCTION: assert(false);
        case AST_Declaration_Kind::STRUCT: assert(false);
        case AST_Declaration_Kind::UNION: assert(false);
    }

    assert(false);
    return nullptr;
}

bool valid_static_type_conversion(Type *from, Type *to)
{
    assert(from);
    assert(to);

    if (from == to) {
        return true;
    }

    switch (from->kind) {

        case Type_Kind::UNSIZED_INTEGER: {
            if (to->kind == Type_Kind::INTEGER) return true;
            assert(false);
            break;
        }

        case Type_Kind::INTEGER: assert(false);
        case Type_Kind::FLOAT: assert(false);
    }

    assert(false);
    return false;
}

}
