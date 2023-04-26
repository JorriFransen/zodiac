#include "type.h"

#include "asserts.h"
#include "ast.h"

namespace Zodiac
{

Type UNSIZED_INTEGER_TYPE;
Dynamic_Array<Type *> function_types;

bool type_system_initialize()
{
    dynamic_array_create(&dynamic_allocator, &function_types);

    return true;
}

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

void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types)
{
    assert(type);

    create_type(type, Type_Kind::FUNCTION, 64);

    type->function.return_type = return_type;
    type->function.parameter_types = param_types;
}

Type *get_function_type(Type *return_type, Dynamic_Array<Type *> parameter_types, Allocator *allocator)
{
    assert(return_type);
    assert(allocator);

    for (u64 i = 0; i < function_types.count; i++) {

        auto ex_type = function_types[i];
        if (ex_type->function.return_type != return_type) continue;
        if (ex_type->function.parameter_types.count != parameter_types.count) continue;

        bool param_match = true;
        for (u64 j = 0; j < parameter_types.count; j++) {
            if (ex_type->function.parameter_types[j] != parameter_types[j]) {
                param_match = false;
                break;
            }
        }

        if (param_match) return ex_type;
    }

    auto params_copy = dynamic_array_copy(&parameter_types, allocator);

    Type *result = alloc<Type>(allocator);
    create_function_type(result, return_type, params_copy);

    return result;
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
        case Type_Kind::FUNCTION: assert(false);
    }

    assert(false);
    return false;
}

}
