#include "constant_resolver.h"

#include "ast.h"
#include "scope.h"
#include "type.h"
#include "util/asserts.h"

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

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::CHAR_LITERAL: {
            return expr->integer_literal.value;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto scope = expr->identifier.scope;
            assert(scope);
            auto sym = scope_get_symbol(scope, expr->identifier);
            assert(sym);
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
            assert(init_expr->resolved_type);
            return resolve_constant_integer_expr(init_expr, init_type);
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;

        case AST_Expression_Kind::UNARY: {
            switch (expr->unary.op) {
                case AST_Unary_Operator::INVALID: assert(false); break;

                case AST_Unary_Operator::PLUS: {
                    return resolve_constant_integer_expr(expr->unary.operand);
                }

                case AST_Unary_Operator::MINUS: {
                    Integer_Value operand_val = resolve_constant_integer_expr(expr->unary.operand, type);
                    switch (type->bit_size) {
                        default: assert(false);
                        case 8: operand_val.s8 = -operand_val.s8;
                        case 16: operand_val.s16 = -operand_val.s16;
                        case 32: operand_val.s32 = -operand_val.s32;
                        case 64: operand_val.s64 = -operand_val.s64;
                    }

                    return operand_val;
                }

                case AST_Unary_Operator::ADDRESS_OF: assert(false); break;
                case AST_Unary_Operator::DEREF: assert(false); break;
            }
        }

        case AST_Expression_Kind::BINARY: {
            return resolve_constant_integer_binary_expr(expr, type);
        }

        case AST_Expression_Kind::CAST: {
            return resolve_constant_integer_expr(expr->cast.value, expr->resolved_type);
            break;
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            assert(expr->directive.generated_expression);
            return resolve_constant_integer_expr(expr->directive.generated_expression, type);
        }

        case AST_Expression_Kind::COMPOUND: assert(false); break;
    }

    assert(false);
    return {};
}

Integer_Value resolve_constant_integer_binary_expr(AST_Expression *expr, Type *type/*=nullptr*/)
{
    assert(expr);
    assert(expr->kind == AST_Expression_Kind::BINARY);
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
        case 8:  EXECUTE_SIZED_BINOP(8, op)  \
        case 16: EXECUTE_SIZED_BINOP(16, op) \
        case 32: EXECUTE_SIZED_BINOP(32, op) \
        case 64: EXECUTE_SIZED_BINOP(64, op) \
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

bool resolve_constant_bool_expr(AST_Expression *expr)
{
    debug_assert(expr);

    assert(EXPR_IS_CONST(expr));
    assert(EXPR_IS_TYPED(expr));
    assert(expr->resolved_type->kind == Type_Kind::BOOLEAN);

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::BOOL_LITERAL: {
            return expr->bool_literal;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            auto scope = expr->identifier.scope;
            assert(scope);

            auto sym = scope_get_symbol(scope, expr->identifier);
            assert(sym);
            auto decl = sym->decl;
            assert(decl);
            assert(decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);
            assert(decl->variable.resolved_type);

            Type *init_type = decl->variable.resolved_type;
            assert(init_type->kind == Type_Kind::BOOLEAN);

            AST_Expression *init_expr = decl->variable.value;
            assert(init_expr);
            assert(init_expr->resolved_type);
            assert(init_expr->resolved_type == init_type);
            return resolve_constant_bool_expr(init_expr);
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            assert(expr->directive.generated_expression);
            return resolve_constant_bool_expr(expr->directive.generated_expression);
        }

        case AST_Expression_Kind::COMPOUND: assert(false); break;

    }

    assert(false);
    return false;
}

Real_Value resolve_constant_real_expr(AST_Expression *expr)
{
    debug_assert(expr);

    assert(EXPR_IS_CONST(expr));
    assert(EXPR_IS_TYPED(expr));

    if (expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER) {
        Integer_Value iv = resolve_constant_integer_expr(expr, &builtin_type_s64);
        return { .r32 = (float)iv.s64, .r64 = (double)iv.s64 };
    }


    assert(expr->resolved_type->kind == Type_Kind::FLOAT);

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;

        case AST_Expression_Kind::REAL_LITERAL: {
            return expr->real_literal.value;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            auto scope = expr->identifier.scope;
            assert(scope);

            auto sym = scope_get_symbol(scope, expr->identifier);
            assert(sym);
            auto decl = sym->decl;
            assert(decl);
            assert(decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE);
            assert(decl->variable.resolved_type);

            Type *init_type = decl->variable.resolved_type;
            assert(init_type->kind == Type_Kind::FLOAT);

            AST_Expression *init_expr = decl->variable.value;
            assert(init_expr);
            assert(init_expr->resolved_type);
            assert(init_expr->resolved_type == init_type);
            return resolve_constant_real_expr(init_expr);
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            assert(expr->directive.generated_expression);
            return resolve_constant_real_expr(expr->directive.generated_expression);
        }

        case AST_Expression_Kind::COMPOUND: assert(false); break;
    }

    assert(false);
    return {};
}

void* constant_resolve_pointer_expr(AST_Expression *expr)
{
    assert(EXPR_IS_TYPED(expr));
    assert(expr->resolved_type->kind == Type_Kind::POINTER);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::IDENTIFIER: assert(false); break;
        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;

        case AST_Expression_Kind::CAST: {
            auto base_expr = expr->cast.value;

            assert(base_expr->resolved_type->kind == Type_Kind::INTEGER);
            auto int_val = resolve_constant_integer_expr(base_expr);
            return (void *)int_val.u64;
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;
    }
}

}
