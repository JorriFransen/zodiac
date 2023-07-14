#pragma once

#include "common.h"
#include "defines.h"

namespace Zodiac {

struct AST_Expression;
struct Scope;
struct Type;

ZAPI Integer_Value resolve_constant_integer_expr(AST_Expression *expr, Type *type = nullptr);
ZAPI Integer_Value resolve_constant_integer_binary_expr(AST_Expression *expr, Type *type = nullptr);

}