#pragma once

#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct AST_Declaration;

enum class Type_Kind
{
    UNSIZED_INTEGER,
    INTEGER,
    FLOAT,

    FUNCTION,
};

struct Type
{
    Type_Kind kind;
    u64 bit_size;

    union {

        struct {
            bool sign;
        } integer;

        struct {
            Type *return_type;
            Dynamic_Array<Type*> parameter_types;
        } function;

    };
};

ZAPI extern Type UNSIZED_INTEGER_TYPE;
ZAPI extern Dynamic_Array<Type *> function_types;

ZAPI bool type_system_initialize();

ZAPI void create_type(Type *type, Type_Kind kind, u64 bit_size);
ZAPI void create_integer_type(Type *type, u64 bit_size, bool sign);
ZAPI void create_float_type(Type *type, u64 bit_size);
ZAPI void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types);

ZAPI Type *get_function_type(Type *return_type, Dynamic_Array<Type *> param_types, Allocator *allocator);

ZAPI Type *decl_type(AST_Declaration *decl);

ZAPI bool valid_static_type_conversion(Type *from, Type *to);

}
