#pragma once

#include "util/zstring.h"
#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"

namespace Zodiac
{

struct Allocator;
struct AST_Declaration;
struct String_Builder;
struct Zodiac_Context;

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

    Type *pointer_to;

    union {

        struct {
            bool sign;
        } integer;

        struct {
            Type *base;
        } pointer;

        struct {
            Atom name;
            Dynamic_Array<Type *> member_types;
        } structure;

        struct {
            Type *element_type;
            u64 count;
        } static_array;

        struct {
            Type *return_type;
            Dynamic_Array<Type *> parameter_types;
            bool is_vararg;
        } function;

    };
};

ZAPI extern bool type_system_initialized;

ZAPI extern Type builtin_type_unsized_integer;
ZAPI extern Type builtin_type_void;
ZAPI extern Type builtin_type_boolean;

ZAPI extern Type builtin_type_u64;
ZAPI extern Type builtin_type_s64;
ZAPI extern Type builtin_type_u32;
ZAPI extern Type builtin_type_s32;
ZAPI extern Type builtin_type_u16;
ZAPI extern Type builtin_type_s16;
ZAPI extern Type builtin_type_u8;
ZAPI extern Type builtin_type_s8;

ZAPI extern Type builtin_type_r64;
ZAPI extern Type builtin_type_r32;

ZAPI extern Type builtin_type_String;

ZAPI extern Dynamic_Array<Type *> function_types;
ZAPI extern Dynamic_Array<Type *> static_array_types;

ZAPI bool type_system_initialize();

ZAPI void create_type(Type *type, Type_Kind kind, u64 bit_size, Type_Flags flags = TYPE_FLAG_NONE);
ZAPI void create_integer_type(Type *type, u64 bit_size, bool sign);
ZAPI void create_float_type(Type *type, u64 bit_size);
ZAPI void create_pointer_type(Type *type, Type *base_type);
ZAPI void create_struct_type(Type *type, Dynamic_Array<Type *> member_types, Atom name);
ZAPI void create_static_array_type(Type *type, Type *element_type, u64 count);
ZAPI void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types);

ZAPI Type *get_pointer_type(Type *base, Allocator *allocator);
ZAPI Type *get_struct_type(Zodiac_Context *zc, Array_Ref<Type *> member_types, const char *cstr_name, Allocator *allocator);
ZAPI Type *get_struct_type(Array_Ref<Type *> member_types, Atom name, Allocator *allocator);
ZAPI Type *get_static_array_type(Type *element_type, u64 count, Allocator *allocator);
ZAPI Type *get_function_type(Type *return_type, Array_Ref<Type *> param_types, Allocator *allocator);

ZAPI Type *decl_type(AST_Declaration *decl);

ZAPI bool valid_static_type_conversion(Type *from, Type *to);

ZAPI void type_to_string(Type *type, String_Builder *sb);
ZAPI String type_to_string(Allocator *allocator, Type *type);

}
