#pragma once

#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "util/zstring.h"

namespace Zodiac
{

struct Symbol;
struct Allocator;
struct String_Builder;
struct Zodiac_Context;

#define ZODIAC_BUILTIN_TYPES                      \
    ZODIAC_NUMERIC_TYPE_DEF(u, 64)                \
    ZODIAC_NUMERIC_TYPE_DEF(s, 64)                \
    ZODIAC_NUMERIC_TYPE_DEF(u, 32)                \
    ZODIAC_NUMERIC_TYPE_DEF(s, 32)                \
    ZODIAC_NUMERIC_TYPE_DEF(u, 16)                \
    ZODIAC_NUMERIC_TYPE_DEF(s, 16)                \
    ZODIAC_NUMERIC_TYPE_DEF(u, 8)                 \
    ZODIAC_NUMERIC_TYPE_DEF(s, 8)                 \
    ZODIAC_NUMERIC_TYPE_DEF(r, 32)                \
    ZODIAC_NUMERIC_TYPE_DEF(r, 64)                \
    ZODIAC_NAME_TYPE_DEF(void)                    \
    ZODIAC_NAME_TYPE_DEF(bool)                    \
    ZODIAC_NAME_TYPE_DEF(String)                  \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Kind)          \
    ZODIAC_NAME_TYPE_DEF(Type_Info)               \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Int)           \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Pointer)       \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Struct)        \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Struct_Member) \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Enum)          \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Enum_Member)   \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Static_Array)  \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Slice)         \
    ZODIAC_NAME_TYPE_DEF(Type_Info_Function)      \

// Builtin type atoms
#define ZODIAC_NUMERIC_TYPE_DEF(type, size) ZAPI extern Atom atom_##type##size;
#define ZODIAC_NAME_TYPE_DEF(name) ZAPI extern Atom atom_##name;
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_NAME_TYPE_DEF
#undef ZODIAC_NUMERIC_TYPE_DEF

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
    ENUM,
    STATIC_ARRAY,
    SLICE,
    FUNCTION,
};

typedef u32 Type_Flags;
enum Type_Flag : Type_Flags
{
    TYPE_FLAG_NONE                   = 0x00,
    TYPE_FLAG_INT                    = 0x01,
    TYPE_FLAG_AGGREGATE              = 0x02,
    TYPE_FLAG_SLICE_STRUCT           = 0x04,
    TYPE_FLAG_UNFINISHED_STRUCT_TYPE = 0x08,
};

struct Type_Enum_Member
{
    Atom name;
    s64 value;
};

struct Type
{
    Type_Kind kind;
    u64 bit_size;

    Type_Flags flags;

    Type *pointer_to;

    // Temporary
    Type *clean_type;

    s64 info_index;

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
            Dynamic_Array<String> member_names;
        } structure;

        struct {
            Atom name;
            Dynamic_Array<Type_Enum_Member> members;
            Type *integer_type;
        } enumeration;

        struct {
            Type *element_type;
            u64 count;
        } static_array;

        struct {
            Type *element_type;
            Type *struct_type;
        } slice;

        struct {
            Type *return_type;
            Dynamic_Array<Type *> parameter_types;
            bool is_vararg;
        } function;

    };

    Type() {}
};

#define TYPE_IS_SLICE_STRUCT(t) ((t)->kind == Type_Kind::STRUCTURE && (t)->flags & TYPE_FLAG_SLICE_STRUCT)

ZAPI extern bool type_system_initialized;

ZAPI extern Type builtin_type_unsized_integer;
ZAPI extern Type builtin_type_void;
ZAPI extern Type builtin_type_bool;

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

ZAPI extern s64 pointer_size;
ZAPI extern Dynamic_Array<Type *> function_types;
ZAPI extern Dynamic_Array<Type *> struct_types;
ZAPI extern Dynamic_Array<Type *> enum_types_types;
ZAPI extern Dynamic_Array<Type *> static_array_types;
ZAPI extern Dynamic_Array<Type *> slice_types;

ZAPI bool type_system_initialize(Zodiac_Context *ctx);

ZAPI void create_type(Type *type, Type_Kind kind, u64 bit_size, Type_Flags flags = TYPE_FLAG_NONE);
ZAPI void create_integer_type(Type *type, u64 bit_size, bool sign);
ZAPI void create_float_type(Type *type, u64 bit_size);
ZAPI void create_pointer_type(Type *type, Type *base_type);
ZAPI void create_struct_type(Type *type, Atom name, Dynamic_Array<Type *> member_types, Dynamic_Array<String> member_names);
ZAPI void create_static_array_type(Type *type, Type *element_type, u64 count);
ZAPI void create_slice_type(Type *type, Type *element_type, Type *struct_type);
ZAPI void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types, bool vararg = false);

ZAPI Type *get_pointer_type(Type *base, Allocator *allocator);

ZAPI Type *get_struct_type(Zodiac_Context *zc, const char *cstr_name, Array_Ref<Type *> member_types, Array_Ref<String> member_names, Allocator *allocator);
ZAPI Type *get_struct_type(Atom name, Array_Ref<Type *> member_types, Array_Ref<String> member_names, Allocator *allocator);
ZAPI Type *finalize_struct_type(Type *unfinished, Array_Ref<Type *> member_types, Array_Ref<String> member_names, Allocator *allocator);
ZAPI Type *get_enum_type(Atom name, Dynamic_Array<Type_Enum_Member> members, Type *integer_type, Allocator *allocator);

ZAPI Type *get_static_array_type(Type *element_type, u64 count, Allocator *allocator);
ZAPI Type *get_slice_type(Zodiac_Context *ctx, Type *element_type, Allocator *allocator);
ZAPI Type *get_function_type(Type *return_type, Array_Ref<Type *> param_types, Allocator *allocator, bool vararg = false);

ZAPI Type *get_struct_type_by_name(Zodiac_Context *ctx, Atom name);
ZAPI Type *get_string_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_kind_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_int_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_pointer_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_struct_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_struct_member_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_enum_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_enum_member_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_static_array_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_slice_type(Zodiac_Context *ctx);
ZAPI Type *get_type_info_function_type(Zodiac_Context *ctx);

ZAPI Type *sym_decl_type(Symbol *sym);

ZAPI Type *cleanup_slice_pointers(Zodiac_Context *ctx, Type *type);

ZAPI bool valid_static_type_conversion(Type *from, Type *to);

ZAPI void type_to_string(Type *type, String_Builder *sb);
ZAPI String type_to_string(Allocator *allocator, Type *type);
ZAPI String temp_type_string(Type *type);

}
