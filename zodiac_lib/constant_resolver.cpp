#include "constant_resolver.h"

#include "ast.h"
#include "containers/dynamic_array.h"
#include "scope.h"
#include "type.h"
#include "util/asserts.h"

namespace Zodiac {

Constant_Resolve_Result resolve_constant_integer_expr(AST_Expression *expr, Type *type/*=nullptr*/)
{
    assert(expr);
    assert(EXPR_IS_CONST(expr));
    assert(expr->resolved_type);

    if (type) {
        assert(expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER ||
               expr->resolved_type == type);
        assert(type->kind == Type_Kind::INTEGER || type->kind == Type_Kind::ENUM);
    } else {
        assert(expr->resolved_type->kind == Type_Kind::INTEGER || expr->resolved_type->kind == Type_Kind::ENUM);
        type = expr->resolved_type;
    }


    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::CHAR_LITERAL: {

            return { Constant_Resolve_Result_Kind::OK, type, { expr->integer_literal.value } };
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

            Type *init_type = nullptr;
            AST_Expression *init_expr = nullptr;

            if (decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE) {
                assert(decl->variable.resolved_type);

                init_type = decl->variable.resolved_type;
                if (init_type->kind == Type_Kind::UNSIZED_INTEGER) {
                    init_type = &builtin_type_s64;
                }
                assert(init_type->kind == Type_Kind::INTEGER);

                init_expr = decl->variable.value;

            } else if (decl->kind == AST_Declaration_Kind::ENUM_MEMBER) {
                if (!decl->enum_member.value_expr)
                {
                    return { Constant_Resolve_Result_Kind::UNDEFINED };
                }
                assert(decl->enum_member.value_expr->resolved_type);
                init_type = decl->enum_member.value_expr->resolved_type;
                assert(init_type->kind == Type_Kind::INTEGER);

                init_expr = decl->enum_member.value_expr;

            } else {
                assert(false);
            }

            assert(init_type);
            assert(init_expr);
            return resolve_constant_integer_expr(init_expr, init_type);
        }

        case AST_Expression_Kind::MEMBER: {
            assert(expr->resolved_type->kind == Type_Kind::ENUM);
            auto enum_type = expr->resolved_type;
            assert(enum_type->enumeration.integer_type == &builtin_type_s64);

            auto value = enum_type->enumeration.members[expr->member.index_in_parent].value;

            return { Constant_Resolve_Result_Kind::OK, type, { { .s64 = value } } };
        }

        case AST_Expression_Kind::INDEX: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;

        case AST_Expression_Kind::UNARY: {
            switch (expr->unary.op) {
                case AST_Unary_Operator::INVALID: assert(false); break;

                case AST_Unary_Operator::PLUS: {
                    return resolve_constant_integer_expr(expr->unary.operand);
                }

                case AST_Unary_Operator::MINUS: {
                    auto operand_result = resolve_constant_integer_expr(expr->unary.operand, type);
                    assert(operand_result.kind == Constant_Resolve_Result_Kind::OK);
                    auto operand_val = operand_result.integer;
                    switch (type->bit_size) {
                        default: assert(false);
                        case 8: operand_val.s8 = -operand_val.s8;
                        case 16: operand_val.s16 = -operand_val.s16;
                        case 32: operand_val.s32 = -operand_val.s32;
                        case 64: operand_val.s64 = -operand_val.s64;
                    }

                    return { Constant_Resolve_Result_Kind::OK, type, { operand_val } };
                }

                case AST_Unary_Operator::ADDRESS_OF: assert(false); break;
                case AST_Unary_Operator::DEREF: assert(false); break;
                case AST_Unary_Operator::NOT: assert(false); break;
            }
        }

        case AST_Expression_Kind::BINARY: {
            return resolve_constant_integer_binary_expr(expr, type);
        }

        case AST_Expression_Kind::RANGE: assert(false); break;

        case AST_Expression_Kind::CAST: {

            auto from = expr->cast.value->resolved_type;
            auto to = expr->resolved_type;

            if (from->kind == Type_Kind::ENUM) {

                assert(to->kind == Type_Kind::INTEGER);
                auto enum_type = from;
                auto result = resolve_constant_integer_expr(expr->cast.value, enum_type);

                assert(result.kind == Constant_Resolve_Result_Kind::OK);
                assert(result.type == enum_type);
                assert(enum_type->enumeration.integer_type == &builtin_type_s64);

                return { Constant_Resolve_Result_Kind::OK, to, { { .s64 = result.integer.s64 } } };

            } else if (from->kind == Type_Kind::INTEGER) {
                auto result = resolve_constant_integer_expr(expr->cast.value, from);
                assert(result.kind == Constant_Resolve_Result_Kind::OK);
                assert(result.type == from);
                result.type = to;
                return result;
            } else {
                assert(false);
            }
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

Constant_Resolve_Result resolve_constant_integer_binary_expr(AST_Expression *expr, Type *type/*=nullptr*/)
{
    assert(expr);
    assert(expr->kind == AST_Expression_Kind::BINARY);
    assert(EXPR_IS_CONST(expr));
    assert(expr->resolved_type);

    if (type) {
        assert(expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER ||
               expr->resolved_type == type);
        assert(type->kind == Type_Kind::INTEGER ||
               type->kind == Type_Kind::BOOLEAN);
    } else {
        assert(expr->resolved_type->kind == Type_Kind::INTEGER);
        type = expr->resolved_type;
    }

    assert(type->kind == Type_Kind::INTEGER ||
            type->kind == Type_Kind::BOOLEAN);

    auto size = type->bit_size;

    auto lhs_type = type;
    auto rhs_type = type;
    if (is_binary_cmp_op(expr->binary.op)) {
        lhs_type = expr->binary.lhs->resolved_type;
        rhs_type = expr->binary.rhs->resolved_type;
        assert(lhs_type == rhs_type);
    }

    auto lhs_res = resolve_constant_integer_expr(expr->binary.lhs, lhs_type);
    assert(lhs_res.kind == Constant_Resolve_Result_Kind::OK);
    assert(lhs_res.type->kind == Type_Kind::INTEGER || lhs_res.type->kind == Type_Kind::ENUM);

    auto rhs_res = resolve_constant_integer_expr(expr->binary.rhs, rhs_type);
    assert(rhs_res.kind == Constant_Resolve_Result_Kind::OK);
    assert(rhs_res.type->kind == Type_Kind::INTEGER || rhs_res.type->kind == Type_Kind::ENUM);

    assert(lhs_res.type == rhs_res.type);

    Integer_Value lhs_val = lhs_res.integer;
    Integer_Value rhs_val = rhs_res.integer;

    Integer_Value result;

#define EXECUTE_SIZED_BINOP(size, op) case (size): { result.u##size = lhs_val.u##size op rhs_val.u##size; break; }

#define EXECUTE_BINOP(op) { switch (size) { \
        default: assert(false); \
        EXECUTE_SIZED_BINOP(8, op)  \
        EXECUTE_SIZED_BINOP(16, op) \
        EXECUTE_SIZED_BINOP(32, op) \
        EXECUTE_SIZED_BINOP(64, op) \
    }   break;                              \
}

#define EXECUTE_SIZED_CMP_BINOP(size, op) case (size): { result.u64 = lhs_val.u##size op rhs_val.u##size; break; }

#define EXECUTE_CMP_BINOP(op) { switch (size) { \
        default: assert(false); \
        EXECUTE_SIZED_CMP_BINOP(8, op) \
        EXECUTE_SIZED_CMP_BINOP(16, op) \
        EXECUTE_SIZED_CMP_BINOP(32, op) \
        EXECUTE_SIZED_CMP_BINOP(64, op) \
    }   break; \
}

    switch (expr->binary.op) {
        case AST_Binary_Operator::INVALID: assert(false); break;

        case AST_Binary_Operator::ADD: EXECUTE_BINOP(+)
        case AST_Binary_Operator::SUB: EXECUTE_BINOP(-)
        case AST_Binary_Operator::MUL: EXECUTE_BINOP(*)
        case AST_Binary_Operator::DIV: EXECUTE_BINOP(/)
        case AST_Binary_Operator::MOD: EXECUTE_BINOP(%)

        case AST_Binary_Operator::EQ: EXECUTE_CMP_BINOP(==);
        case AST_Binary_Operator::NEQ: assert(false); break;
        case AST_Binary_Operator::LT: assert(false); break;
        case AST_Binary_Operator::GT: assert(false); break;
        case AST_Binary_Operator::LTEQ: assert(false); break;
        case AST_Binary_Operator::GTEQ: assert(false); break;
    }

#undef EXECUTE_BINOP
#undef EXECUTE_SIZED_BINOP

    return { Constant_Resolve_Result_Kind::OK, type, { result } };
}

Constant_Resolve_Result resolve_constant_bool_expr(AST_Expression *expr)
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
            return { Constant_Resolve_Result_Kind::OK, expr->resolved_type, { .boolean = expr->bool_literal } };
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
        case AST_Expression_Kind::RANGE: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            assert(expr->directive.generated_expression);
            return resolve_constant_bool_expr(expr->directive.generated_expression);
        }

        case AST_Expression_Kind::COMPOUND: assert(false); break;

    }

    assert(false);
    return { Constant_Resolve_Result_Kind::UNDEFINED };
}

Constant_Resolve_Result resolve_constant_real_expr(AST_Expression *expr)
{
    debug_assert(expr);

    assert(EXPR_IS_CONST(expr));
    assert(EXPR_IS_TYPED(expr));

    if (expr->resolved_type->kind == Type_Kind::UNSIZED_INTEGER) {
        auto int_res = resolve_constant_integer_expr(expr, &builtin_type_s64);
        assert(int_res.kind == Constant_Resolve_Result_Kind::OK);
        auto iv = int_res.integer;
        return { Constant_Resolve_Result_Kind::OK, expr->resolved_type, { .real = { .r32 = (float)iv.s64, .r64 = (double)iv.s64 } } };
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
            return { Constant_Resolve_Result_Kind::OK, expr->resolved_type, { .real = expr->real_literal.value } };
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
        case AST_Expression_Kind::RANGE: assert(false); break;
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

Constant_Resolve_Result resolve_constant_pointer_expression(AST_Expression *expr)
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
        case AST_Expression_Kind::RANGE: assert(false); break;

        case AST_Expression_Kind::CAST: {
            auto base_expr = expr->cast.value;

            assert(base_expr->resolved_type->kind == Type_Kind::INTEGER);
            auto int_res = resolve_constant_integer_expr(base_expr);
            assert(int_res.kind == Constant_Resolve_Result_Kind::OK);
            auto int_val = int_res.integer;
            return { Constant_Resolve_Result_Kind::OK, expr->resolved_type, { .pointer = (void *)int_val.u64 } };
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;
    }
}

}
