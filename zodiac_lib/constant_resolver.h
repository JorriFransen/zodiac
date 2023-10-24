#pragma once

#include "common.h"
#include "defines.h"

namespace Zodiac {

struct AST_Expression;
struct Type;

enum class Constant_Resolve_Result_Kind {
    UNDEFINED,
    OK,
};

struct Constant_Resolve_Result
{
    Constant_Resolve_Result_Kind kind;
    Type *type;

    union {
        Integer_Value integer;
        Real_Value real;
        bool boolean;
        void *pointer;
    };
};

ZAPI Constant_Resolve_Result resolve_constant_integer_expr(AST_Expression *expr, Type *type = nullptr);
ZAPI Constant_Resolve_Result resolve_constant_integer_binary_expr(AST_Expression *expr, Type *type = nullptr);

ZAPI Constant_Resolve_Result resolve_constant_bool_expr(AST_Expression *expr);

ZAPI Constant_Resolve_Result resolve_constant_real_expr(AST_Expression *expr, Type *type = nullptr);

ZAPI Constant_Resolve_Result resolve_constant_pointer_expression(AST_Expression *expr);
}
