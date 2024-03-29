#include "parser.h"

#include <stdio.h>

#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "source_pos.h"
#include "util/asserts.h"
#include "zodiac_context.h"

namespace Zodiac
{

#define FIRST_VARARG(N, ...) N

#define report_parse_error(parser, range, fmt, ...) { \
    (parser)->error = true; \
    (parser)->context->fatal_resolve_error = true; \
    auto handle = zodiac_report_error((parser)->context, ZODIAC_PARSE_ERROR, range, (fmt), ##__VA_ARGS__); \
    (parser)->context->errors[handle].fatal = true; \
}

#define expect_token(p, c) { if (!_expect_token((p), (c))) { return {}; } }

#define parse_expression(p) _parse_expression(p); if ((p)->error) { return {}; }
#define parse_expr_unary(p) _parse_expr_unary(p); if ((p)->error) { return {}; }
#define parse_statement(...) _parse_statement(__VA_ARGS__); if ((FIRST_VARARG(__VA_ARGS__))->error) { return {}; }
#define parse_type_spec(p) _parse_type_spec(p); if ((p)->error) { return {}; }

#define return_if_null(n) { if (!(n)) { debug_assert(parser->error); return nullptr; }}

void parser_create(Zodiac_Context *ctx, Lexer *lxr, Parser *out_parser)
{
    debug_assert(ctx && lxr && out_parser);
    out_parser->context = ctx;
    out_parser->lxr = lxr;
    queue_create(&dynamic_allocator, &out_parser->peeked_tokens);
    out_parser->error = false;
}

void parser_destroy(Parser *parser)
{
    debug_assert(parser);

    queue_destroy(&parser->peeked_tokens);
}

AST_Identifier parse_identifier(Parser *parser)
{
    Token ident_tok = cur_tok(parser);

    if (!_expect_token(parser, TOK_NAME)) return {};

    AST_Identifier result;
    ast_identifier_create(ident_tok.atom, ident_tok.sr, &result);
    return result;
}

AST_Expression *parse_expr_compound(Parser *parser)
{
    debug_assert(parser);

    Source_Pos start_pos = cur_tok(parser).sr.start;

    expect_token(parser, '{');

    auto temp_exprs = temp_array_create<AST_Expression *>(temp_allocator_allocator());

    Source_Pos end_pos = cur_tok(parser).sr.end;

    while (!match_token(parser, '}')) {

        if (temp_exprs.array.count != 0) {
            expect_token(parser, ',');
        }

        AST_Expression *expr = parse_expression(parser);

        dynamic_array_append(&temp_exprs.array, expr);

        end_pos = cur_tok(parser).sr.end;
    }

    auto exprs = temp_array_finalize(&parser->context->ast_allocator, &temp_exprs);
    return ast_compound_expr_new(parser->context, { start_pos, end_pos }, exprs);
}

AST_Expression *parse_expr_operand(Parser *parser)
{
    debug_assert(parser);

    Source_Pos start_pos = cur_tok(parser).sr.start;

    auto ct = cur_tok(parser);

    Source_Range range = { start_pos, ct.sr.end };

    if (is_token(parser, TOK_INT)) {
        next_token(parser);
        return ast_integer_literal_expr_new(parser->context, range,  { .u64 = ct.integer });
    } else if (is_token(parser, TOK_REAL)) {
        next_token(parser);
        return ast_real_literal_expr_new(parser->context, range, ct.real);
    } else if (is_token(parser, TOK_NAME)) {
        next_token(parser);
        return ast_identifier_expr_new(parser->context, range, ct.atom);

    } else if (is_token(parser, TOK_STRING)) {
        Atom token_atom = ct.atom;
        next_token(parser);

        Atom content_atom = atom_get(&parser->context->atoms, token_atom.data + 1, token_atom.length - 2);
        return ast_string_literal_expr_new(parser->context, range, content_atom);

    } else if (is_token(parser, TOK_CHAR)) {

        char c = ct.character;
        next_token(parser);

        return ast_character_literal_expr_new(parser->context, range, c);

    } else if (is_token(parser, TOK_KEYWORD)) {

        if (match_keyword(parser, keyword_null)) {
            return ast_null_literal_expr_new(parser->context, range);
        } else if (match_keyword(parser, keyword_true)) {
            return ast_bool_literal_expr_new(parser->context, range, true);
        } else if (match_keyword(parser, keyword_false)) {
            return ast_bool_literal_expr_new(parser->context, range, false);
        } else if (match_keyword(parser, keyword_cast)) {

            expect_token(parser, '(');
            AST_Type_Spec *ts = parse_type_spec(parser);
            expect_token(parser, ',');
            AST_Expression *expr = parse_expression(parser);

            auto end = cur_tok(parser).sr.end;
            expect_token(parser, ')');

            return ast_cast_expr_new(parser->context, {range.start, end}, ts, expr);
        }

    } else if (is_token(parser, '{')) {

        return parse_expr_compound(parser);

    } else {

        if (match_token(parser, '(')) {
            auto result = parse_expression(parser);
            expect_token(parser, ')');
            return result;
        }
    }

    ct = cur_tok(parser);
    report_parse_error(parser, ct.sr, "Expected INT, NAME, '(' or 'null' when parsing expression, got: '%s', (%s)", ct.atom.data, tmp_token_kind_str(ct.kind).data);
    return nullptr;
}

AST_Expression *parse_expr_base(Parser *parser)
{
    debug_assert(parser);

    AST_Expression *expr = parse_expr_operand(parser);
    return_if_null(expr);

    auto start_pos = expr->sr.start;

    while (is_token(parser, '(') || is_token(parser, '[') || is_token(parser, '.')) {

        if (match_token(parser, '(')) {

            auto temp_args = temp_array_create<AST_Expression *>(temp_allocator_allocator());

            auto end_pos = cur_tok(parser).sr.end;

            while (!match_token(parser, ')')) {

                if (temp_args.array.count != 0) {
                    expect_token(parser, ',');
                }

                AST_Expression *arg = parse_expression(parser);
                dynamic_array_append(&temp_args.array, arg);

                end_pos = cur_tok(parser).sr.end;
            }

            auto args = temp_array_finalize(&parser->context->ast_allocator, &temp_args);
            expr = ast_call_expr_new(parser->context, {start_pos, end_pos}, expr, args);

        } else if (match_token(parser, '[')) {

            AST_Expression *index = parse_expression(parser);
            auto end_pos = cur_tok(parser).sr.end;
            expect_token(parser, ']');
            expr = ast_index_expr_new(parser->context, {start_pos, end_pos}, expr, index);


        } else {

            expect_token(parser, '.');

            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);
            expr = ast_member_expr_new(parser->context, {start_pos, name_tok.sr.end}, expr, name_tok.atom);

        }
    }

    return expr;
}

AST_Expression *_parse_expr_unary(Parser *parser)
{
    debug_assert(parser);
    Source_Pos start_pos = cur_tok(parser).sr.start;

    if (match_token(parser, '+')) {

        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, {start_pos, operand->sr.end} , AST_Unary_Operator::PLUS, operand);

    } else if (match_token(parser, '-')) {

        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, {start_pos, operand->sr.end}, AST_Unary_Operator::MINUS, operand);

    } else if (match_token(parser, '*')) {

        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, {start_pos, operand->sr.end}, AST_Unary_Operator::ADDRESS_OF, operand);

    } else if (match_token(parser, '<')) {

        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, {start_pos, operand->sr.end}, AST_Unary_Operator::DEREF, operand);

    } else if (match_token(parser, '!')) {

        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, {start_pos, operand->sr.end}, AST_Unary_Operator::NOT, operand);

    } else if (is_token(parser, '#')) {

        Parsed_Directive pd = parse_directive(parser, false);
        return_if_null(pd.data);

        auto directive = pd.data;

        if (directive->kind == AST_Directive_Kind::RUN) {
            return ast_run_directive_expr_new(parser->context, directive->sr, directive);
        } else if (directive->kind == AST_Directive_Kind::TYPE_INFO) {
            return ast_type_info_expr_new(parser->context, directive->sr, directive);
        } else {
            assert_msg(false, "Unhandled directive expression");
        }

    } else if (match_token(parser, TOK_DOT_DOT)) {

        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, {start_pos, operand->sr.end}, AST_Unary_Operator::SPREAD, operand);

    } else {
        return parse_expr_base(parser);
    }
}

file_local AST_Binary_Operator token_kind_to_ast_binop[TOK_LAST] = {

    ['+'] = AST_Binary_Operator::ADD,
    ['-'] = AST_Binary_Operator::SUB,
    ['*'] = AST_Binary_Operator::MUL,
    ['/'] = AST_Binary_Operator::DIV,

    ['%'] = AST_Binary_Operator::MOD,

    [TOK_EQ] = AST_Binary_Operator::EQ,
    [TOK_NEQ] = AST_Binary_Operator::NEQ,
    ['<'] = AST_Binary_Operator::LT,
    ['>'] = AST_Binary_Operator::GT,
    [TOK_LTEQ] = AST_Binary_Operator::LTEQ,
    [TOK_GTEQ] = AST_Binary_Operator::GTEQ,


};

AST_Expression *parse_expr_mul(Parser *parser)
{
    debug_assert(parser);

    AST_Expression *lhs = parse_expr_unary(parser);
    return_if_null(lhs);

    while (is_token(parser, '*') || is_token(parser, '/') || is_token(parser, '%')) {
        char op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_unary(parser);

        auto ast_op = token_kind_to_ast_binop[(int)op];
        debug_assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, {lhs->sr.start, rhs->sr.end}, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expr_add(Parser *parser)
{
    debug_assert(parser);

    AST_Expression *lhs = parse_expr_mul(parser);

    while (is_token(parser, '+') || is_token(parser, '-')) {
        char op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_mul(parser);
        return_if_null(rhs);

        auto ast_op = token_kind_to_ast_binop[(int)op];
        debug_assert(ast_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, {lhs->sr.start, rhs->sr.end}, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expr_cmp(Parser *parser)
{
    debug_assert(parser);

    AST_Expression *lhs = parse_expr_add(parser);

    while (is_token(parser, '<') || is_token(parser, '>') || is_token(parser, TOK_EQ) || is_token(parser, TOK_NEQ) || is_token(parser, TOK_LTEQ) || is_token(parser, TOK_GTEQ)) {
        auto op = cur_tok(parser).kind;
        next_token(parser);

        AST_Expression *rhs = parse_expr_add(parser);
        return_if_null(rhs);

        auto cmp_op = token_kind_to_ast_binop[(int)op];
        debug_assert(cmp_op != AST_Binary_Operator::INVALID);
        lhs = ast_binary_expr_new(parser->context, {lhs->sr.start, rhs->sr.end}, cmp_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *_parse_expression(Parser *parser)
{
    return parse_expr_cmp(parser);
}

AST_Statement *parse_keyword_statement(Parser *parser, bool optional_semi/*=false*/)
{
    debug_assert(parser);

    Source_Pos start_pos = cur_tok(parser).sr.start;

    if (match_keyword(parser, keyword_if)) {
        AST_Expression *cond = parse_expression(parser);
        AST_Statement *then_stmt = parse_statement(parser);
        AST_Statement *else_stmt = nullptr;

        auto end_pos = then_stmt->sr.end;

        auto temp_else_ifs = temp_array_create<AST_If_Block>(temp_allocator_allocator());

        dynamic_array_append(&temp_else_ifs.array, { cond, then_stmt, nullptr });

        while (match_keyword(parser, keyword_else)) {
            if (match_keyword(parser, keyword_if)) {
                AST_Expression *else_if_cond = parse_expression(parser);
                AST_Statement *else_if_then = parse_statement(parser);
                AST_If_Block else_if = { else_if_cond, else_if_then, nullptr };
                dynamic_array_append(&temp_else_ifs.array, else_if);
                end_pos = else_if_then->sr.end;
            } else {
                else_stmt = parse_statement(parser);
                end_pos = else_stmt->sr.end;
                break;
            }
        }

        auto else_ifs = temp_array_finalize(&parser->context->ast_allocator, &temp_else_ifs);

        return ast_if_stmt_new(parser->context, {start_pos, end_pos}, else_ifs, else_stmt);

    } else if (match_keyword(parser, keyword_while)) {

        AST_Expression *cond = parse_expression(parser);
        AST_Statement *do_stmt = parse_statement(parser);

        return ast_while_stmt_new(parser->context, { start_pos, do_stmt->sr.end }, cond, do_stmt);

    } else if (match_keyword(parser, keyword_for)) {

        bool expect_rparen = false;
        if (match_token(parser, '(')) {
            expect_rparen = true;
        }

        AST_Declaration *init_decl = parse_declaration(parser, {});
        assert(init_decl);

        AST_Expression *cond_expr = parse_expression(parser);
        expect_token(parser, ';');

        AST_Statement *inc_stmt = parse_statement(parser, true);

        auto end_pos = inc_stmt->sr.end;

        if (expect_rparen) {
            end_pos = cur_tok(parser).sr.end;
            expect_token(parser, ')');
        }

        AST_Statement *body_stmt = parse_statement(parser);

        return ast_for_stmt_new(parser->context, { start_pos, end_pos }, init_decl, cond_expr, inc_stmt, body_stmt);

    } else if (match_keyword(parser, keyword_switch)) {

        AST_Expression *switch_value = parse_expression(parser);

        expect_token(parser, '{');

        Dynamic_Array<AST_Statement *> cases;
        dynamic_array_create(&parser->context->ast_allocator, &cases);

        while (!is_token(parser, '}')) {

            auto case_start = cur_tok(parser).sr.start;
            if (match_keyword(parser, keyword_case)) {

                Dynamic_Array<AST_Expression *> case_values;
                dynamic_array_create(&parser->context->ast_allocator, &case_values, 1);

                do {

                    AST_Expression *case_value = parse_expression(parser);

                    if (match_token(parser, TOK_DOT_DOT)) {
                        AST_Expression *max = parse_expression(parser);
                        case_value = ast_range_expr_new(parser->context, {case_value->sr.start, max->sr.end}, case_value, max);
                    }

                    dynamic_array_append(&case_values, case_value);

                } while(match_token(parser, ','));

                expect_token(parser, ':');

                assert(case_values.count);

                AST_Statement *case_body = parse_switch_case_body(parser);
                return_if_null(case_body);

                assert(case_body);
                AST_Statement *case_stmt = ast_switch_case_stmt_new(parser->context, {case_start, case_body->sr.end}, case_values, case_body);

                dynamic_array_append(&cases, case_stmt);

            } else if (match_keyword(parser, keyword_default)) {

                expect_token(parser, ':');
                AST_Statement *default_stmt = parse_statement(parser);

                AST_Statement *case_stmt = ast_switch_case_stmt_new(parser->context, {case_start, default_stmt->sr.end}, {}, default_stmt);

                dynamic_array_append(&cases, case_stmt);

            } else {
                auto ct = cur_tok(parser);
                report_parse_error(parser,  ct.sr.start, "Expected 'case' or 'default', got: '%s'", ct.atom.data);
                return nullptr;
            }

        }

        if (!cases.count) {
            report_parse_error(parser,  start_pos, "Switch without cases");
            return nullptr;
        }

        auto end_pos = cur_tok(parser).sr.start;
        expect_token(parser, '}');

        assert(switch_value);

        return ast_switch_stmt_new(parser->context, {start_pos, end_pos}, switch_value, cases);

    } else if (match_keyword(parser, keyword_break)) {

        auto end_pos = cur_tok(parser).sr.end;
        expect_token(parser, ';');

        return ast_break_stmt_new(parser->context, {start_pos, end_pos});

    } else if (match_keyword(parser, keyword_continue)) {

        auto end_pos = cur_tok(parser).sr.end;
        expect_token(parser, ';');

        return ast_continue_stmt_new(parser->context, {start_pos, end_pos});

    } else if (match_keyword(parser, keyword_return)) {

        AST_Expression *value = nullptr;
        if (!is_token(parser, ';')) {
            value = parse_expression(parser);
        }

        auto end_pos = cur_tok(parser).sr.end;
        expect_token(parser, ';');

        return ast_return_stmt_new(parser->context, { start_pos, end_pos }, value);

    } else if (match_keyword(parser, keyword_defer)) {

        AST_Statement *stmt = parse_statement(parser);
        return ast_defer_stmt_new(parser->context, {start_pos, stmt->sr.end}, stmt);
    }

    Token t = cur_tok(parser);
    report_parse_error(parser, t.sr, "Unexpected keyword token when parsing statement: '%s'", tmp_token_str(t).data);
    return nullptr;
}

AST_Statement *_parse_statement(Parser *parser, bool optional_semi/*=false*/)
{
    debug_assert(parser);

    if (is_token(parser, TOK_KEYWORD)) {
        return parse_keyword_statement(parser, optional_semi);
    }

    Source_Pos start_pos = cur_tok(parser).sr.start;

    if (match_token(parser, '{')) {
        auto temp_statements = temp_array_create<AST_Statement *>(temp_allocator_allocator());

        while (!is_token(parser, '}')) {

            AST_Statement *stmt = parse_statement(parser);
            dynamic_array_append(&temp_statements.array, stmt);
        }
        auto end_pos = cur_tok(parser).sr.end;
        expect_token(parser, '}');

        auto statements = temp_array_finalize(&parser->context->ast_allocator, &temp_statements);
        return ast_block_stmt_new(parser->context, {start_pos, end_pos}, statements);
    }

    if (is_token(parser, '#') && peek_token(parser).atom == directive_falltrough) {
        auto pd = parse_directive(parser);
        assert(pd.kind == Parsed_Directive_Kind::DATA);
        assert(pd.data);
        assert(pd.data->kind == AST_Directive_Kind::FALLTROUGH);
        return ast_falltrough_stmt_new(parser->context, pd.data->sr, pd.data);
    }

    AST_Statement *result = nullptr;

    // All remaining options start with a unary expression
    AST_Expression *expr = parse_expr_unary(parser);
    return_if_null(expr);

    if (expr->kind == AST_Expression_Kind::CALL) {
        result = ast_call_stmt_new(parser->context, expr->sr, expr);

    } else if (expr->kind == AST_Expression_Kind::IDENTIFIER && match_token(parser, ':')) {

        AST_Type_Spec *type_spec = nullptr;
        if ((!is_token(parser, '=')) && (!is_token(parser, ':'))) {
            type_spec = parse_type_spec(parser);
        }

        AST_Expression *value = nullptr;

        auto ct = cur_tok(parser);
        if (!(is_token(parser, '=') || is_token(parser, ':') || is_token(parser, ';'))) {
            report_parse_error(parser, ct.sr, "Expected '=', ':' or ';' after typespec in variable or constant declaration, got '%s'", ct.atom.data);
            return nullptr;
        }

        bool is_constant = false;
        if (match_token(parser, '=')) {
            value = parse_expression(parser);
        } else if (match_token(parser, ':')) {
            is_constant = true;
            value = parse_expression(parser);
        }

        auto end_pos = cur_tok(parser).sr.end;

        Source_Range range = {start_pos, end_pos};
        if (is_constant) {
            auto decl = ast_constant_variable_decl_new(parser->context, range, expr->identifier, type_spec, value);
            result = ast_declaration_stmt_new(parser->context, range, decl);
        } else {
            auto decl = ast_variable_decl_new(parser->context, range, expr->identifier, type_spec, value);
            result = ast_declaration_stmt_new(parser->context, range, decl);
        }

    } else if (expr->kind == AST_Expression_Kind::RUN_DIRECTIVE) {
        report_parse_error(parser, expr->sr, "Result value of #run in local scope is not used");
        return nullptr;
    } else if (is_token(parser, '+') || is_token(parser, '-') || is_token(parser, '*') || is_token(parser, '/')) {

        auto op_token = cur_tok(parser);
        next_token(parser);
        expect_token(parser, '=');

        auto rhs = parse_expression(parser);
        auto op = token_kind_to_ast_binop[op_token.kind];
        auto new_value = ast_binary_expr_new(parser->context, {start_pos, rhs->sr.end}, op, expr, rhs);

        result = ast_assign_stmt_new(parser->context, {start_pos, new_value->sr.end}, expr, new_value);
        result->flags |= AST_STMT_FLAG_COMPOUND_ASSIGNMENT;
    } else {
        expect_token(parser, '=');
        AST_Expression *value = parse_expression(parser);
        result = ast_assign_stmt_new(parser->context, {start_pos, value->sr.end}, expr, value);
    }

    debug_assert(result);

    if (optional_semi) {
        match_token(parser, ';');
    } else {
        expect_token(parser, ';');
    }

    return result;
}

AST_Statement *parse_switch_case_body(Parser *parser)
{
    AST_Statement *first_stmt = parse_statement(parser);

#define IS_CASE_END(p) (is_keyword(p, keyword_case) || is_keyword(p, keyword_default) || is_token(p, '}'))

    if (IS_CASE_END(parser)) {
        return first_stmt;
    }

    if (first_stmt->kind == AST_Statement_Kind::BLOCK) {
        report_parse_error(parser, first_stmt->sr.end, "Expected 'case' or 'range' after case block");
        return nullptr;
    }

    auto stmts = temp_array_create<AST_Statement *>(temp_allocator_allocator(), 4);
    dynamic_array_append(&stmts, first_stmt);

    while (!IS_CASE_END(parser)) {
        auto stmt = parse_statement(parser);
        dynamic_array_append(&stmts, stmt);
    }

    Source_Range range = { first_stmt->sr.start, stmts[stmts.array.count-1]->sr.end };
    return ast_block_stmt_new(parser->context, range, temp_array_finalize(&parser->context->ast_allocator, &stmts));


#undef CASE_END
}

AST_Declaration *parse_function_declaration(Parser *parser, AST_Identifier ident, Parsed_Directive pd)
{
    debug_assert(parser);

    bool foreign = false;

    if (pd.kind != Parsed_Directive_Kind::NONE) {
        if (pd.kind == Parsed_Directive_Kind::FOREIGN) foreign = true;
        else assert(false);
    }

    expect_token(parser, '(');

    Dynamic_Array<AST_Declaration *> params = {};

    bool is_vararg = false;

    if (is_token(parser, TOK_NAME)) {


        auto temp_params = temp_array_create<AST_Declaration *>(temp_allocator_allocator());
        do {

            assert(!is_vararg); // Can only have 1 vararg param

            Token name_tok = cur_tok(parser);
            Source_Pos start = name_tok.sr.start;
            expect_token(parser, TOK_NAME);
            expect_token(parser, ':');

            AST_Type_Spec *ts = nullptr;
            Source_Pos end;

            AST_Declaration_Flags field_flags = AST_DECL_FLAG_NONE;

            if (is_token(parser, TOK_DOT_DOT)) {
                auto range = cur_tok(parser).sr;
                end = range.end;
                next_token(parser);

                ts = ast_vararg_type_spec_new(parser->context, range);
                is_vararg = true;
                field_flags |= AST_DECL_FLAG_VARARG;

            } else {
                ts = parse_type_spec(parser);
                end = ts->sr.end;
            }

            AST_Identifier param_ident;
            ast_identifier_create(name_tok.atom, name_tok.sr, &param_ident);

            AST_Declaration *param_decl = ast_parameter_decl_new(parser->context, {start, end}, param_ident, ts);
            param_decl->flags |= field_flags;

            dynamic_array_append(&temp_params.array, param_decl);
        } while (match_token(parser, ','));

        params = temp_array_finalize(&parser->context->ast_allocator, &temp_params);

    }

    expect_token(parser, ')');

    AST_Type_Spec *return_ts = nullptr;

    Dynamic_Array<AST_Statement *> statements = {};

    if (match_token(parser, TOK_RIGHT_ARROW)) {
        return_ts = parse_type_spec(parser);
    } else if (foreign) {
        report_parse_error(parser, ident.sr, "Foreign function must specify return type");
    }

    AST_Declaration_Flags flags = AST_DECL_FLAG_NONE;

    if (!foreign) {

        expect_token(parser, '{');

        auto temp_stmts = temp_array_create<AST_Statement *>(temp_allocator_allocator());

        while (!match_token(parser, '}')) {
            AST_Statement *stmt = parse_statement(parser);
            return_if_null(stmt);

            dynamic_array_append(&temp_stmts.array, stmt);
        }

        statements = temp_array_finalize(&parser->context->ast_allocator, &temp_stmts);
    } else {
        flags |= AST_DECL_FLAG_FOREIGN;
        expect_token(parser, ';');
    }

    if (is_vararg) {
        if (foreign) {
            flags |= AST_DECL_C_FLAG_VARARG;
        } else {
            flags |= AST_DECL_FLAG_VARARG;
        }
    }

    return ast_function_decl_new(parser->context, ident.sr, ident, params, return_ts, statements, flags);
}

AST_Declaration *parse_aggregate_declaration(Parser *parser, AST_Identifier ident)
{
    debug_assert(parser);

    AST_Declaration_Kind kind = AST_Declaration_Kind::INVALID;

    if (match_keyword(parser, keyword_struct)) {
        kind = AST_Declaration_Kind::STRUCT;
    } else {
        expect_keyword(parser, keyword_union);
        kind = AST_Declaration_Kind::UNION;
    }

    debug_assert(kind == AST_Declaration_Kind::STRUCT || kind == AST_Declaration_Kind::UNION);

    expect_token(parser, '{');

    auto temp_fields = temp_array_create<AST_Declaration *>(temp_allocator_allocator());

    while (!match_token(parser, '}')) {

        auto temp_idents = temp_array_create<AST_Identifier>(temp_allocator_allocator());

        // At least one name
        Token first_name_tok = cur_tok(parser);
        expect_token(parser, TOK_NAME);

        AST_Identifier first_ident;
        ast_identifier_create(first_name_tok.atom, first_name_tok.sr, &first_ident);
        dynamic_array_append(&temp_idents.array, first_ident);

        while (match_token(parser, ',')) {
            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);

            AST_Identifier ident;
            ast_identifier_create(name_tok.atom, name_tok.sr, &ident);
            dynamic_array_append(&temp_idents.array, ident);
        }

        expect_token(parser, ':');

        AST_Type_Spec *ts = parse_type_spec(parser);
        expect_token(parser, ';');

        for (u64 i = 0; i < temp_idents.array.count; i++) {

            Source_Range field_range = { temp_idents.array[i].sr.start, ts->sr.end };

            AST_Declaration *field_decl = ast_field_decl_new(parser->context, field_range, temp_idents.array[i], ts);
            dynamic_array_append(&temp_fields.array, field_decl);
        }
    }

    auto fields = temp_array_finalize(&parser->context->ast_allocator, &temp_fields);
    return ast_aggregate_decl_new(parser->context, ident.sr, ident, kind, fields);
}

AST_Declaration *parse_enum_declaration(Parser *parser, AST_Identifier ident)
{
    if (!expect_keyword(parser, keyword_enum)) {
        return nullptr;
    }

    expect_token(parser, '{');

    Dynamic_Array<AST_Declaration *> members;
    dynamic_array_create(&parser->context->ast_allocator, &members);

    while (!is_token(parser, '}')) {

        AST_Identifier member_ident = parse_identifier(parser);
        AST_Expression *member_value = nullptr;

        if (match_token(parser, ':')) {
            expect_token(parser, ':');

            member_value = parse_expression(parser);
        }

        if (!(is_token(parser, ',') || is_token(parser, ';'))) {
            auto pos = member_ident.sr;
            if (member_value) pos = member_value->sr;
            report_parse_error(parser, pos, "Expected enum member to be terminated with ';' or ','");
            return nullptr;
        }

        auto member_end = cur_tok(parser).sr.end;
        next_token(parser);

        AST_Declaration *member_decl = ast_enum_member_decl_new(parser->context, {ident.sr.start, member_end}, member_ident, member_value);
        dynamic_array_append(&members, member_decl);
    }

    auto end_pos = cur_tok(parser).sr.end;
    expect_token(parser, '}');

    return ast_enum_decl_new(parser->context, {ident.sr.start, end_pos}, ident, members);
}

AST_Declaration *parse_declaration(Parser *parser, Parsed_Directive  pd)
{
    AST_Identifier ident = parse_identifier(parser);
    return_if_null(ident.name.data);

    expect_token(parser, ':');

    AST_Type_Spec *ts = nullptr;

    if (!is_token(parser, ':') &&  !is_token(parser, '=')) {
        ts = parse_type_spec(parser);

        auto ct = cur_tok(parser);
        if (!(is_token(parser, '=') || is_token(parser, ':') || is_token(parser, ';'))) {
            report_parse_error(parser, ct.sr, "Expected '=', ':' or ';' after typespec in variable or constant declaration, got '%s'", ct.atom.data);
            return nullptr;
        }
    }

    if (match_token(parser, ':')) {

        if (is_keyword(parser, keyword_struct) || is_keyword(parser, keyword_union)) {
            return parse_aggregate_declaration(parser, ident);
        }

        if (is_keyword(parser, keyword_enum)) {
            return parse_enum_declaration(parser, ident);
        }

        // Constant
        if (is_token(parser, '(')) {
            if ((peek_token(parser).kind == Token_Kind::TOK_NAME && peek_token(parser, 2).kind == ':') ||
                 peek_token(parser).kind == ')') {

                debug_assert(!ts);
                return parse_function_declaration(parser, ident, pd);
            }
        }

        AST_Expression *const_value = nullptr;

        auto peek_1 = peek_token(parser);
        if (is_token(parser, '#') && peek_1.kind == Token_Kind::TOK_NAME && peek_1.atom == directive_type) {
            Parsed_Directive pd = parse_directive(parser, false);
            assert(pd.kind == Parsed_Directive_Kind::DATA);
            assert(pd.data->kind == AST_Directive_Kind::TYPE);

            AST_Type_Spec *ts = parse_type_spec(parser);
            pd.data->type.type_spec = ts;
            const_value = ast_type_expr_new(parser->context, { pd.data->sr.start, ts->sr.end }, pd.data);

        } else {

            const_value = parse_expression(parser);
        }

        assert(const_value);

        expect_token(parser, ';');

        return ast_constant_variable_decl_new(parser->context, ident.sr, ident, ts, const_value);

    } else if (is_token(parser, '=') || is_token(parser, ';')) {

        // Variable
        AST_Expression *value = nullptr;

        bool expect_semicolon = true;

        if (match_token(parser, '=')) {
            value = parse_expression(parser);
            expect_semicolon = value->kind != AST_Expression_Kind::RUN_DIRECTIVE;

        }

        auto end = cur_tok(parser).sr.end;

        if (expect_semicolon) {
            expect_token(parser, ';');
        } else {
            match_token(parser, ';');
        }

        return ast_variable_decl_new(parser->context, Source_Range { ident.sr.start, end }, ident, ts, value);
    }

    // Should be unreachable, if not one of the cases in the 'if' above, an error should have been reported
    return nullptr;
}

AST_Type_Spec *_parse_type_spec(Parser *parser)
{
    Token t = cur_tok(parser);
    switch (t.kind) {

        case '*': {
            next_token(parser);
            AST_Type_Spec *base = parse_type_spec(parser);
            return ast_pointer_ts_new(parser->context, { t.sr.start, base->sr.end }, base);
        }

        case '[': {
            next_token(parser);

            AST_Expression *length_expr = nullptr;
            if (!is_token(parser, ']')) {
                length_expr = parse_expression(parser);
            }

            if (!match_token(parser, ']')) return nullptr;

            AST_Type_Spec *base_ts = parse_type_spec(parser);

            Source_Range range = { t.sr.start, base_ts->sr.end };
            if (length_expr) {
                return ast_static_array_ts_new(parser->context, range, length_expr, base_ts);
            } else {
                return ast_slice_ts_new(parser->context, range, base_ts);
            }
        }

        case '(': {
            next_token(parser);

            auto params = temp_array_create<AST_Type_Spec *>(temp_allocator_allocator());

            bool vararg = false;

            while (!match_token(parser, ')')) {

                assert(!vararg);

                if (params.array.count) {
                    expect_token(parser, ',');
                }

                AST_Type_Spec *param_ts = parse_type_spec(parser);
                dynamic_array_append(&params, param_ts);

                if (param_ts->kind == AST_Type_Spec_Kind::VARARG) {
                    vararg = true;
                }
            }

            expect_token(parser, TOK_RIGHT_ARROW);

            AST_Type_Spec *return_ts = parse_type_spec(parser);

            auto pts = temp_array_finalize(&parser->context->ast_allocator, &params);

            return ast_function_ts_new(parser->context, {t.sr.start, return_ts->sr.end}, pts, return_ts, vararg);
        }

        case '#': {

            Parsed_Directive pd = parse_directive(parser);
            assert(pd.kind == Parsed_Directive_Kind::DATA);
            assert(pd.data);

            assert(pd.data->kind == AST_Directive_Kind::TYPE_OF);

            return ast_type_of_ts_new(parser->context, {t.sr.start, pd.data->sr.end}, pd.data);
        }

        case TOK_NAME: {
            next_token(parser);

            AST_Identifier ident;
            ast_identifier_create(t.atom, t.sr, &ident);

            return ast_name_ts_new(parser->context, t.sr, ident);
        }

        default: {
            report_parse_error(parser, t.sr, "Unexpected token when parsing typespec: '%s'", t.atom.data);
            return nullptr;
        }
    }

    assert(false);
    return nullptr;
}

Parsed_Directive parse_directive(Parser *parser, bool eat_semicolon/*=true*/)
{
    debug_assert(parser);

    Source_Pos start_pos = cur_tok(parser).sr.start;

    expect_token(parser, '#');

    auto directive_name_tok = cur_tok(parser);

    expect_token(parser, TOK_NAME);

    Parsed_Directive result = {};

    // TODO: Use hashes here
    if (directive_name_tok.atom == directive_run) {

        result.kind = Parsed_Directive_Kind::DATA;

        if (is_token(parser, '{')) {

            AST_Statement *stmt = parse_statement(parser, true);

            if (stmt->kind == AST_Statement_Kind::BLOCK) {

                for (s64 i = 0; i < stmt->block.statements.count; i++) {
                    auto member_stmt = stmt->block.statements[i];

                    if (member_stmt->kind != AST_Statement_Kind::CALL) {
                        report_parse_error(parser, member_stmt->sr, "Only call statements are allowed in run blocks");
                        return {};
                    }
                }

            }

            result.data = ast_run_directive_new(parser->context, { start_pos, stmt->sr.end }, stmt);
        } else {

            AST_Expression *expr = parse_expression(parser);
            result.data = ast_run_directive_new(parser->context, { start_pos, expr->sr.end }, expr);
        }

        if (eat_semicolon) match_token(parser, ';');
        return result;

    } else if (directive_name_tok.atom == directive_import) {

        auto str_lit_tok = cur_tok(parser);
        expect_token(parser, TOK_STRING);

        assert(str_lit_tok.atom.data[0] == '"' && str_lit_tok.atom.data[str_lit_tok.atom.length-1] == '"');
        auto path = atom_get(&parser->context->atoms, str_lit_tok.atom.data + 1, str_lit_tok.atom.length - 2);

        result.kind = Parsed_Directive_Kind::DATA;
        result.data = ast_import_directive_new(parser->context, { start_pos, str_lit_tok.sr.end }, path);

        if (eat_semicolon) match_token(parser, ';');
        return result;


    } else if (directive_name_tok.atom == directive_foreign) {
        result.kind = Parsed_Directive_Kind::FOREIGN;
        if (eat_semicolon) match_token(parser, ';');
        return result;
    } else if (directive_name_tok.atom == directive_falltrough) {
        result.kind = Parsed_Directive_Kind::DATA;
        result.data = ast_falltrough_directive_new(parser->context, { start_pos, directive_name_tok.sr.end});
        if (eat_semicolon) match_token(parser, ';');
        return result;

    } else if (directive_name_tok.atom == directive_type_info) {
        result.kind = Parsed_Directive_Kind::DATA;

        expect_token(parser, '(');
        AST_Type_Spec *ts = parse_type_spec(parser);

        auto end_pos = cur_tok(parser).sr.end;
        expect_token(parser, ')');

        result.data = ast_type_info_directive_new(parser->context, { start_pos, end_pos }, ts);
        if (eat_semicolon) match_token(parser, ';');
        return result;

    } else if (directive_name_tok.atom == directive_type_of) {
        result.kind = Parsed_Directive_Kind::DATA;

        expect_token(parser, '(');
        AST_Expression *expr = parse_expression(parser);

        auto end_pos = cur_tok(parser).sr.end;
        expect_token(parser, ')');

        result.data = ast_type_of_directive_new(parser->context, { start_pos, end_pos }, expr);

        if (eat_semicolon) match_token(parser, ';');
        return result;

    } else if (directive_name_tok.atom == directive_type) {
        result.kind = Parsed_Directive_Kind::DATA;

        auto end_pos = directive_name_tok.sr.end;

        result.data = ast_type_directive_new(parser->context, { start_pos, end_pos });
        return result;
    } else {
        assert_msg(false, "Unknown directive");
    }

    assert(directive_name_tok.kind);
    assert(false);

    return {};
}

AST_File *parse_file(Parser *parser)
{
    Dynamic_Array<AST_Declaration *> decls;
    dynamic_array_create(&parser->context->ast_allocator, &decls);

    while (!is_token(parser, TOK_EOF)) {

        Parsed_Directive pd = { .kind = Parsed_Directive_Kind::NONE };

        // Top level directive
        if (is_token(parser, '#')) {
            pd = parse_directive(parser);
            if (pd.kind == Parsed_Directive_Kind::INVALID) return nullptr;

            if (pd.kind == Parsed_Directive_Kind::DATA) { assert(pd.data); }
            else { assert(!pd.data); }

        }

        AST_Declaration *decl = nullptr;


        if (pd.kind == Parsed_Directive_Kind::DATA) {
            auto directive = pd.data;
            if (directive->kind == AST_Directive_Kind::RUN) {
                decl = ast_run_directive_decl_new(parser->context, directive->sr, directive);
            } else {
                assert(directive->kind == AST_Directive_Kind::IMPORT);
                decl = ast_import_directive_decl_new(parser->context, directive->sr, directive);
            }
        } else {
            decl = parse_declaration(parser, pd);
        }

        if (!decl) {
            assert(parser->error);
            return nullptr;
        }

        dynamic_array_append(&decls, decl);
    }

    assert(parser->lxr->stream_name.length);
    Atom stream_name = atom_get(&parser->context->atoms, parser->lxr->stream_name);
    return ast_file_new(parser->context, stream_name, decls);
}

bool is_keyword(Parser *parser, Atom keyword)
{
    debug_assert(parser && is_keyword(keyword));

    return is_token(parser, TOK_KEYWORD) && cur_tok(parser).atom == keyword;
}

bool match_keyword(Parser *parser, Atom keyword)
{
    debug_assert(parser && is_keyword(keyword));

    if (is_keyword(parser, keyword)) {
        next_token(parser);
        return true;
    }

    return false;
}

bool expect_keyword(Parser *parser, Atom keyword)
{
    debug_assert(parser && is_keyword(keyword));

    if (is_keyword(parser, keyword)) {
        next_token(parser);
        return true;
    }

    return false;
}

bool is_token(Parser *parser, Token_Kind kind)
{
    debug_assert(parser);
    return cur_tok(parser).kind == kind;
}

bool is_token(Parser *parser, char c)
{
    debug_assert(parser);
    return (char)cur_tok(parser).kind == c;
}

bool next_token(Parser *parser)
{
    debug_assert(parser && parser->lxr);

    if (!queue_empty(&parser->peeked_tokens)) {
        queue_dequeue(&parser->peeked_tokens);
        return true;
    } else {
        return next_token(parser->lxr);
    }
}

bool match_token(Parser *parser, Token_Kind kind)
{
    debug_assert(parser && parser->lxr);

    if (is_token(parser, kind)) {
        next_token(parser);
        return true;
    }

    return false;
}

bool match_token(Parser *parser, char c)
{
    return match_token(parser, (Token_Kind)c);
}

bool _expect_token(Parser *parser, Token_Kind kind)
{
    debug_assert(parser && parser->lxr);

    if (is_token(parser, kind)) {
        next_token(parser);
        return true;
    }

    String_Ref kind_str = tmp_token_kind_str(kind);
    Token t = cur_tok(parser);
    report_parse_error(parser, t.sr, "Expected token: '%s', got: '%s'", kind_str.data, t.atom.data);
    return false;
}

bool _expect_token(Parser *parser, char c)
{
    return _expect_token(parser, (Token_Kind)c);
}

Token cur_tok(Parser *parser)
{
    debug_assert(parser && parser->lxr);

    if (queue_empty(&parser->peeked_tokens)) {
        return parser->lxr->token;
    } else {
        return queue_peek(&parser->peeked_tokens, 0);
    }
}

Token peek_token(Parser *parser, u64 offset/*=1*/)
{
    debug_assert(parser && parser->lxr);
    debug_assert(offset >= 1);

    auto queued_token_count = queue_used(&parser->peeked_tokens);

    if (queued_token_count < offset + 1) {

        if (queue_empty(&parser->peeked_tokens)) {
            for (s64 i = 0; i < offset + 1; i++) {
                queue_enqueue(&parser->peeked_tokens, parser->lxr->token);
                next_token(parser->lxr);
            }
        } else {
            s64 peeked_count = queue_used(&parser->peeked_tokens);

            s64 add_count = (offset + 1) - peeked_count;

            for (s64 i = 0; i < add_count; i++) {
                queue_enqueue(&parser->peeked_tokens, parser->lxr->token);
                next_token(parser->lxr);
            }
        }
    }

    return queue_peek(&parser->peeked_tokens, offset);
}

void syntax_error(Parser *parser, const String_Ref fmt, ...)
{
    debug_assert(parser);
    debug_assert(fmt.length && fmt.data);
    debug_assert(fmt.data[fmt.length] == '\0');

    va_list args;
    va_start(args, fmt);

    syntax_error(parser, fmt, args);

    va_end(args);
}

void syntax_error(Parser *parser, const String_Ref fmt, va_list args)
{
    char err_msg[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    auto out_length = string_format(err_msg, fmt, args);

    if (out_length + 1 > ZSTRING_FORMAT_STACK_BUFFER_SIZE) {
        assert_msg(false, "Formatted syntax error does not fit in stack buffer");
        return;
    }

    Token t = cur_tok(parser);
    Source_Pos pos = t.sr.start;

    out_length = string_format(err_msg, "%s:%i:%i: error: %s", pos.name.data, pos.line, pos.index_in_line, err_msg);

    if (out_length + 1 > ZSTRING_FORMAT_STACK_BUFFER_SIZE) {
        assert_msg(false, "Formatted syntax error does not fit in stack buffer");
        return;
    }

    parser->error = true;
    printf("%s\n", err_msg);
}

#undef parse_error

}
