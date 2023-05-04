#pragma once

#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct Allocator;
struct AST_Declaration;
struct String_Builder;

enum class Type_Kind
{
    INVALID,

    VOID,

    UNSIZED_INTEGER,
    INTEGER,
    FLOAT,
    BOOLEAN,

    POINTER,

    STRUCTURE,
    STATIC_ARRAY,
    FUNCTION,
};

typedef u32 Type_Flags;
enum Type_Flag : Type_Flags
{
    TYPE_FLAG_NONE      = 0x00,
    TYPE_FLAG_INT       = 0x01,
    TYPE_FLAG_AGGREGATE = 0x02,
    TYPE_FLAG_ARRAY     = 0x04,
};

struct Type
{
    Type_Kind kind;
    u64 bit_size;

    Type_Flags flags;

    union {

        struct {
            bool sign;
        } integer;

        struct {
            Type *return_type;
            Dynamic_Array<Type*> parameter_types;
            bool is_vararg;
        } function;

    };
};

ZAPI extern bool type_system_initialized;

ZAPI extern Type builtin_type_unsized_integer;
ZAPI extern Type builtin_type_void;
ZAPI extern Type builtin_type_boolean;
ZAPI extern Type builtin_type_s64;
ZAPI extern Type builtin_type_s32;

ZAPI extern Dynamic_Array<Type *> function_types;

ZAPI bool type_system_initialize();

ZAPI void create_type(Type *type, Type_Kind kind, u64 bit_size, Type_Flags flags = TYPE_FLAG_NONE);
ZAPI void create_integer_type(Type *type, u64 bit_size, bool sign);
ZAPI void create_float_type(Type *type, u64 bit_size);
ZAPI void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types);

ZAPI Type *get_function_type(Type *return_type, Array_Ref<Type *> param_types, Allocator *allocator);

ZAPI Type *decl_type(AST_Declaration *decl);

ZAPI bool valid_static_type_conversion(Type *from, Type *to);

ZAPI void type_to_string(Type *type, String_Builder *sb);
ZAPI String type_to_string(Allocator *allocator, Type *type);

}
