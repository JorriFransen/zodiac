#include "parser.h"

#include <containers/dynamic_array.h>
#include <logger.h>

namespace Zodiac
{

// Builtin type atoms
#define ZODIAC_TYPE_DEF(sign, size) Atom atom_##sign##size;
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_TYPE_DEF

file_local bool builtin_types_initialized = false;

template <typename T>
struct Temp_Array
{
    Temporary_Allocator_Mark mark;
    Dynamic_Array<T> array;
};

file_local void initialize_builtin_types(Zodiac_Context *ctx)
{
    assert(!builtin_types_initialized);

    auto at = &ctx->atoms;

#define ZODIAC_TYPE_DEF(sign, size) atom_##sign##size = atom_get(at, #sign#size);
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_TYPE_DEF


    builtin_types_initialized = true;
}

template <typename T>
file_local Temp_Array<T> temp_array_create(Parser *parser)
{
    assert(parser);

    Temp_Array<T> result;

    result.mark = temporary_allocator_get_mark(&parser->context->temp_allocator_state);
    dynamic_array_create(&parser->context->temp_allocator, &result.array, 0);
    return result;
}

template <typename T>
file_local Dynamic_Array<T> temp_array_finalize(Parser *parser, Temp_Array<T> ta)
{
    auto result = dynamic_array_copy(&ta.array, parser->context->ast_allocator);
    temporary_allocator_reset(&parser->context->temp_allocator_state, ta.mark);
    return result;
}

void parser_create(Zodiac_Context *ctx, Lexer *lxr, Parser *out_parser)
{
    assert(ctx && lxr && out_parser);
    out_parser->context = ctx;
    out_parser->lxr = lxr;

    if (!builtin_types_initialized) initialize_builtin_types(ctx);
}

AST_Expression *parse_expr_operand(Parser *parser)
{
    assert(parser);

    if (is_token(parser, TOK_INT)) {
        u64 value = cur_tok(parser).integer;
        next_token(parser);

        return ast_integer_literal_expr_new(parser->context, { .u64 = value });
    } else if (is_token(parser, TOK_NAME)) {
        Atom name_atom = cur_tok(parser).atom;
        next_token(parser);

        return ast_identifier_expr_new(parser->context, name_atom);
    } else {

        if (match_token(parser, '(')) {
            auto result = parse_expression(parser);
            expect_token(parser, ')');
            return result;
        }
    }

    ZFATAL("Expected INT, NAME or '(' when parsing expression, got: '%s'", tmp_token_string(cur_tok(parser)).data);
    return nullptr;
}

AST_Expression *parse_expr_base(Parser *parser)
{
    AST_Expression *expr = parse_expr_operand(parser);

    while (is_token(parser, '(') || is_token(parser, '[') || is_token(parser, '.')) {

        if (match_token(parser, '(')) {

            auto temp_args = temp_array_create<AST_Expression *>(parser);

            while (!match_token(parser, ')')) {

                if (temp_args.array.count != 0) {
                    expect_token(parser, ',');
                }

                AST_Expression *arg = parse_expression(parser);
                dynamic_array_append(&temp_args.array, arg);
            }

            auto args = temp_array_finalize(parser, temp_args);
            expr = ast_call_expr_new(parser->context, expr, args);

        } else if (match_token(parser, '[')) {

            AST_Expression *index = parse_expression(parser);
            expect_token(parser, ']');
            expr = ast_index_expr_new(parser->context, expr, index);


        } else if (match_token(parser, '.')) {

            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);
            expr = ast_member_expr_new(parser->context, expr, name_tok.atom);

        } else {
            auto tmp_tok_str = tmp_token_string(cur_tok(parser));
            ZFATAL("Unexpected token: '%.*s'", (int)tmp_tok_str.length, tmp_tok_str.data);
        }
    }

    return expr;
}

AST_Expression *parse_expr_unary(Parser *parser)
{
    if (is_token(parser, '+')) {
        next_token(parser);
        return parse_expr_unary(parser);
    } else if (is_token(parser, '-')) {
        next_token(parser);
        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, AST_Unary_Operator::MINUS, operand);
    } else {
        return parse_expr_base(parser);
    }
}

file_local AST_Binary_Operator token_kind_to_ast_binop[256] = {

    ['+'] = AST_Binary_Operator::ADD,
    ['-'] = AST_Binary_Operator::SUB,
    ['*'] = AST_Binary_Operator::MUL,
    ['/'] = AST_Binary_Operator::DIV,

    [TOK_EQ] = AST_Binary_Operator::EQ,
    [TOK_NEQ] = AST_Binary_Operator::NEQ,
    ['<'] = AST_Binary_Operator::LT,
    ['>'] = AST_Binary_Operator::GT,
    [TOK_LTEQ] = AST_Binary_Operator::LTEQ,
    [TOK_GTEQ] = AST_Binary_Operator::GTEQ,


};

AST_Expression *parse_expr_mul(Parser *parser)
{
    AST_Expression *lhs = parse_expr_unary(parser);

    while (is_token(parser, '*') || is_token(parser, '/')) {
        char op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_unary(parser);

        auto ast_op = token_kind_to_ast_binop[(int)op];
        assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expr_add(Parser *parser)
{
    AST_Expression *lhs = parse_expr_mul(parser);

    while (is_token(parser, '+') || is_token(parser, '-')) {
        char op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_mul(parser);

        auto ast_op = token_kind_to_ast_binop[(int)op];
        assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expr_cmp(Parser *parser)
{
    AST_Expression *lhs = parse_expr_add(parser);

    while (is_token(parser, '<') || is_token(parser, '>') || is_token(parser, TOK_EQ) || is_token(parser, TOK_NEQ) || is_token(parser, TOK_LTEQ) || is_token(parser, TOK_GTEQ)) {
        auto op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_add(parser);

        auto cmp_op = token_kind_to_ast_binop[(int)op];
        assert(cmp_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, cmp_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expression(Parser *parser)
{
    return parse_expr_cmp(parser);
}

AST_Statement *parse_statement(Parser *parser)
{
    assert(parser);

    if (match_keyword(parser, keyword_if)) {
        AST_Expression *cond = parse_expression(parser);
        AST_Statement *then_stmt = parse_statement(parser);
        AST_Statement *else_stmt = nullptr;

        auto temp_else_ifs = temp_array_create<AST_Else_If>(parser);

        while (match_keyword(parser, keyword_else)) {
            if (match_keyword(parser, keyword_if)) {
                AST_Expression *else_if_cond = parse_expression(parser);
                AST_Statement *else_if_then = parse_statement(parser);
                AST_Else_If else_if = { else_if_cond, else_if_then };
                dynamic_array_append(&temp_else_ifs.array, else_if);
            } else {
                else_stmt = parse_statement(parser);
                break;
            }
        }

        auto else_ifs = temp_array_finalize(parser, temp_else_ifs);

        return ast_if_stmt_new(parser->context, cond, then_stmt, else_ifs, else_stmt);

    } else if (match_keyword(parser, keyword_while)) {

        AST_Expression *cond = parse_expression(parser);
        AST_Statement *do_stmt = parse_statement(parser);

        return ast_while_stmt_new(parser->context, cond, do_stmt);

    } else if (match_keyword(parser, keyword_return)) {

        AST_Expression *value = nullptr;
        if (!is_token(parser, ';')) {
            value = parse_expression(parser);
        }

        expect_token(parser, ';');

        return ast_return_stmt_new(parser->context, value);
    }

    if (match_token(parser, '{')) {
        auto temp_statements = temp_array_create<AST_Statement *>(parser);

        while (!is_token(parser, '}')) {

            AST_Statement *stmt = parse_statement(parser);
            dynamic_array_append(&temp_statements.array, stmt);
        }
        expect_token(parser, '}');

        auto statements = temp_array_finalize(parser, temp_statements);
        return ast_block_stmt_new(parser->context, statements);
    }

    // All remaining options start with an expression
    AST_Statement *result = nullptr;
    AST_Expression *expr = parse_expression(parser);
    if (expr->kind == AST_Expression_Kind::CALL) {
        result = ast_call_stmt_new(parser->context, expr);

    } else if (expr->kind == AST_Expression_Kind::IDENTIFIER && match_token(parser, ':')) {

        AST_Type_Spec *type_spec = nullptr;
        if (!is_token(parser, '=')) {
            type_spec = parse_type_spec(parser);
        }

        AST_Expression *value = nullptr;
        if (match_token(parser, '=')) {
            value = parse_expression(parser);
        }

        expect_token(parser, ';');

        AST_Declaration *decl = ast_variable_decl_new(parser->context, expr, type_spec, value);
        return ast_declaration_stmt_new(parser->context, decl);

    } else {
        expect_token(parser, '=');
        AST_Expression *value = parse_expression(parser);
        assert(value);
        result = ast_assign_stmt_new(parser->context, expr, value);
    }

    assert(result);

    expect_token(parser, ';');

    return result;
}

AST_Type_Spec *parse_type_spec(Parser *parser)
{
    auto name_tok = cur_tok(parser);
    expect_token(parser, TOK_NAME);

    if (name_tok.atom == atom_u8) {
        return ast_integer_ts_new(parser->context, false, 8);
    } else if (name_tok.atom == atom_u16) {
        return ast_integer_ts_new(parser->context, false, 16);
    } else if (name_tok.atom == atom_u32) {
        return ast_integer_ts_new(parser->context, false, 32);
    } else if (name_tok.atom == atom_u64) {
        return ast_integer_ts_new(parser->context, false, 64);
    } else if (name_tok.atom == atom_s8) {
        return ast_integer_ts_new(parser->context, true, 8);
    } else if (name_tok.atom == atom_s16) {
        return ast_integer_ts_new(parser->context, true, 16);
    } else if (name_tok.atom == atom_s32) {
        return ast_integer_ts_new(parser->context, true, 32);
    } else if (name_tok.atom == atom_s64) {
        return ast_integer_ts_new(parser->context, true, 64);
    } else {
        assert(false);
    }

    assert(false);
    return nullptr;
}

bool match_keyword(Parser *parser, Atom keyword)
{
    assert(is_keyword(keyword));

    if (is_token(parser, TOK_KEYWORD) && cur_tok(parser).atom == keyword) {
        next_token(parser);
        return true;
    }

    return false;
}

bool is_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);
    return is_token(parser->lxr, kind);
}

bool is_token(Parser *parser, char c)
{
    assert(parser && parser->lxr);
    return is_token(parser->lxr, c);
}

bool next_token(Parser *parser)
{
    assert(parser && parser->lxr);
    return next_token(parser->lxr);
}

bool match_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);
    return match_token(parser->lxr, kind);
}

bool match_token(Parser *parser, char c)
{
    assert(parser && parser->lxr);
    return match_token(parser->lxr, c);
}

bool expect_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);
    return expect_token(parser->lxr, kind);
}

bool expect_token(Parser *parser, char c)
{
    assert(parser && parser->lxr);
    return expect_token(parser->lxr, c);
}

Token cur_tok(Parser *parser)
{
    assert(parser && parser->lxr);
    return parser->lxr->token;
}

}
