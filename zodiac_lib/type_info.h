#pragma once

#include "defines.h"
#include "util/zstring.h"

namespace Zodiac
{

struct Type;
struct Zodiac_Context;

enum class Type_Info_Kind
{
    INVALID      = 0,

    VOID         = 1,
    INTEGER      = 2,
    REAL         = 3,
    BOOL         = 4,

    POINTER      = 5,

    STRUCT       = 6,
    ENUM         = 7,
    STATIC_ARRAY = 8,
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

struct Type_Info_Pointer
{
    Type_Info base;
    Type_Info *pointer_to;
};

struct Type_Info_Struct_Member
{
    String name;
    Type_Info *type;
};

struct Type_Info_Struct
{
    Type_Info base;

    String name;

    Type_Info_Struct_Member *members;
    s64 member_count;
};

struct Type_Info_Enum_Member
{
    String name;
    s64 value;
};

struct Type_Info_Enum
{
    Type_Info base;

    String name;
    Type_Info *integer_type;

    Type_Info_Enum_Member *members;
    s64 member_count;
};

struct Type_Info_Static_Array
{
    Type_Info base;

    Type_Info *element_type;
    s64 length;
};

ZAPI Type_Info *add_type_info(Zodiac_Context *ctx, Type *type);
ZAPI void init_type_info_base(Type_Info *ti, Type_Info_Kind kind, s64 bit_size);

}
