#include "constant_resolver.h"

#include "ast.h"
#include "scope.h"
#include "type.h"

namespace Zodiac {

Integer_Value resolve_constant_integer_expr(AST_Expression *expr, Type *type/*=nullptr*/)
{
    assert(expr);
    assert(EXPR_IS_CONST(expr));
    assert(expr->resolved_type);

    if (type) {
        assert(expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER ||
               expr->resolved_type == type);
        assert(type->kind == Type_Kind::INTEGER);
    } else {
        assert(expr->resolved_type->kind == Type_Kind::INTEGER);
        type = expr->resolved_type;
    }


    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            return expr->integer_literal.value;
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto scope = expr->identifier.scope;
            assert(scope);
            auto sym = scope_get_symbol(scope, expr->identifier.name);
            auto decl = sym->decl;
            assert(decl);
            assert(decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);
            assert(decl->variable.resolved_type);

            Type *init_type = decl->variable.resolved_type;
            if (init_type->kind == Type_Kind::UNSIZED_INTEGER) {
                init_type = &builtin_type_s64;
            }
            assert(init_type->kind == Type_Kind::INTEGER);

            AST_Expression *init_expr = decl->variable.value;
            assert(init_expr);
            return resolve_constant_integer_expr(init_expr, init_type);
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;

        case AST_Expression_Kind::BINARY: {
            return resolve_constant_integer_binary_expr(expr, type);
        }
    }
}

Integer_Value resolve_constant_integer_binary_expr(AST_Expression *expr, Type *type/*=nullptr*/)
{
    assert(expr);
    assert(EXPR_IS_CONST(expr));
    assert(expr->resolved_type);

    if (type) {
        assert(expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER ||
               expr->resolved_type == type);
        assert(type->kind == Type_Kind::INTEGER);
    } else {
        assert(expr->resolved_type->kind == Type_Kind::INTEGER);
        type = expr->resolved_type;
    }

    assert(type->kind == Type_Kind::INTEGER);

    auto size = type->bit_size;

    Integer_Value lhs_val = resolve_constant_integer_expr(expr->binary.lhs, type);
    Integer_Value rhs_val = resolve_constant_integer_expr(expr->binary.rhs, type);

    Integer_Value result;

#define EXECUTE_SIZED_BINOP(size, op) { result.u##size = (lhs_val).u##size op (rhs_val).u##size; break; }

#define EXECUTE_BINOP(op) { switch (size) { \
        case 8:  EXECUTE_SIZED_BINOP(8, +)  \
        case 16: EXECUTE_SIZED_BINOP(16, +) \
        case 32: EXECUTE_SIZED_BINOP(32, +) \
        case 64: EXECUTE_SIZED_BINOP(64, +) \
    }   break;                              \
}

    switch (expr->binary.op) {
        case AST_Binary_Operator::INVALID: assert(false); break;

        case AST_Binary_Operator::ADD: EXECUTE_BINOP(+)
        case AST_Binary_Operator::SUB: EXECUTE_BINOP(-)
        case AST_Binary_Operator::MUL: EXECUTE_BINOP(*)
        case AST_Binary_Operator::DIV: EXECUTE_BINOP(/)

        case AST_Binary_Operator::EQ: assert(false); break;
        case AST_Binary_Operator::NEQ: assert(false); break;
        case AST_Binary_Operator::LT: assert(false); break;
        case AST_Binary_Operator::GT: assert(false); break;
        case AST_Binary_Operator::LTEQ: assert(false); break;
        case AST_Binary_Operator::GTEQ: assert(false); break;
    }

#undef EXECUTE_BINOP
#undef EXECUTE_SIZED_BINOP

    return result;
}

}