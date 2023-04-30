#include "parser.h"

#include <stdio.h>

#include "containers/dynamic_array.h"
#include "defines.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "platform/platform.h"
#include "source_pos.h"
#include "util/asserts.h"
#include "zodiac_context.h"

namespace Zodiac
{

// Builtin type atoms
#define ZODIAC_NUMERIC_TYPE_DEF(type, size) Atom atom_##type##size;
#define ZODIAC_NAME_TYPE_DEF(name) Atom atom_##name;
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_NAME_TYPE_DEF
#undef ZODIAC_NUMERIC_TYPE_DEF

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

#define ZODIAC_NUMERIC_TYPE_DEF(sign, size) atom_##sign##size = atom_get(at, #sign#size);
#define ZODIAC_NAME_TYPE_DEF(name) atom_##name = atom_get(at, #name);
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_NAME_TYPE_DEF
#undef ZODIAC_NUMERIC_TYPE_DEF


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
file_local void temp_array_destroy(Parser *parser, Temp_Array<T> ta)
{
    temporary_allocator_reset(&parser->context->temp_allocator_state, ta.mark);
}

template <typename T>
file_local Dynamic_Array<T> temp_array_finalize(Parser *parser, Temp_Array<T> ta)
{
    auto result = dynamic_array_copy(&ta.array, &parser->context->ast_allocator);
    temp_array_destroy(parser, ta);
    return result;
}

void parser_create(Zodiac_Context *ctx, Lexer *lxr, Parser *out_parser)
{
    assert(ctx && lxr && out_parser);
    out_parser->context = ctx;
    out_parser->lxr = lxr;
    out_parser->error = false;
    queue_create(&dynamic_allocator, &out_parser->peeked_tokens);

    if (!builtin_types_initialized) initialize_builtin_types(ctx);
}

AST_Identifier parse_identifier(Parser *parser)
{
    Token ident_tok = cur_tok(parser);
    expect_token(parser, TOK_NAME);

    AST_Identifier result;
    ast_identifier_create(ident_tok.atom, ident_tok.start, &result);
    return result;
}

AST_Expression *parse_expr_operand(Parser *parser)
{
    assert(parser);

    Source_Pos start_pos = cur_tok(parser).start;

    if (is_token(parser, TOK_INT)) {
        u64 value = cur_tok(parser).integer;
        next_token(parser);

        return ast_integer_literal_expr_new(parser->context, start_pos,  { .u64 = value });

    } else if (is_token(parser, TOK_NAME)) {
        Atom name_atom = cur_tok(parser).atom;
        next_token(parser);

        return ast_identifier_expr_new(parser->context, start_pos, name_atom);

    } else if (is_token(parser, TOK_STRING)) {
        Atom token_atom = cur_tok(parser).atom;
        next_token(parser);

        Atom content_atom = atom_get(&parser->context->atoms, token_atom.data + 1, token_atom.length - 2);
        return ast_string_literal_expr_new(parser->context, start_pos, content_atom);

    } else if (is_token(parser, TOK_KEYWORD)) {

        if (match_keyword(parser, keyword_null)) {
            return ast_null_literal_expr_new(parser->context, start_pos);
        }

    } else {

        if (match_token(parser, '(')) {
            auto result = parse_expression(parser);
            expect_token(parser, ')');
            return result;
        }
    }

    fatal_syntax_error(parser, "Expected INT, NAME ,'(' or 'null' when parsing expression, got: '%s'", cur_tok(parser).atom.data);
    return nullptr;
}

AST_Expression *parse_expr_base(Parser *parser)
{
    AST_Expression *expr = parse_expr_operand(parser);

    Source_Pos start_pos = cur_tok(parser).start;

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
            expr = ast_call_expr_new(parser->context, start_pos, expr, args);

        } else if (match_token(parser, '[')) {

            AST_Expression *index = parse_expression(parser);
            expect_token(parser, ']');
            expr = ast_index_expr_new(parser->context, start_pos, expr, index);


        } else if (match_token(parser, '.')) {

            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);
            expr = ast_member_expr_new(parser->context, start_pos, expr, name_tok.atom);

        } else {
            Token t = cur_tok(parser);
            fatal_syntax_error(parser, "Unexpected token: '%s'", tmp_token_str(t).data);
        }
    }

    return expr;
}

AST_Expression *parse_expr_unary(Parser *parser)
{
    Source_Pos start_pos = cur_tok(parser).start;
    if (is_token(parser, '+')) {

        next_token(parser);
        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, start_pos, AST_Unary_Operator::PLUS, operand);
    } else if (is_token(parser, '-')) {
        next_token(parser);
        AST_Expression *operand = parse_expr_unary(parser);
        return ast_unary_expr_new(parser->context, start_pos, AST_Unary_Operator::MINUS, operand);
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
        lhs = ast_binary_expr_new(parser->context, lhs->pos, ast_op, lhs, rhs);
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
        lhs = ast_binary_expr_new(parser->context, lhs->pos, ast_op, lhs, rhs);
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
        lhs = ast_binary_expr_new(parser->context, lhs->pos, cmp_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expression(Parser *parser)
{
    return parse_expr_cmp(parser);
}

AST_Statement *parse_keyword_statement(Parser *parser)
{
    assert(parser);

    Source_Pos start_pos = cur_tok(parser).start;

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

        return ast_if_stmt_new(parser->context, start_pos, cond, then_stmt, else_ifs, else_stmt);

    } else if (match_keyword(parser, keyword_while)) {

        AST_Expression *cond = parse_expression(parser);
        AST_Statement *do_stmt = parse_statement(parser);

        return ast_while_stmt_new(parser->context, start_pos, cond, do_stmt);

    } else if (match_keyword(parser, keyword_return)) {

        AST_Expression *value = nullptr;
        if (!is_token(parser, ';')) {
            value = parse_expression(parser);
        }

        expect_token(parser, ';');

        return ast_return_stmt_new(parser->context, start_pos, value);

    } else if (match_keyword(parser, keyword_print)) {

        expect_token(parser, '(');
        AST_Expression *print_expr = parse_expression(parser);
        expect_token(parser, ')');
        expect_token(parser, ';');

        return ast_print_statement_new(parser->context, start_pos, print_expr);
    }

    Token t = cur_tok(parser);
    fatal_syntax_error(parser, "Unexpected keyword token when parsing statment: '%s'", tmp_token_str(t).data);
    return nullptr;
}

AST_Statement *parse_statement(Parser *parser)
{
    assert(parser);

    if (is_token(parser, TOK_KEYWORD)) {
        return parse_keyword_statement(parser);
    }

    Source_Pos start_pos = cur_tok(parser).start;

    if (match_token(parser, '{')) {
        auto temp_statements = temp_array_create<AST_Statement *>(parser);

        while (!is_token(parser, '}')) {

            AST_Statement *stmt = parse_statement(parser);
            dynamic_array_append(&temp_statements.array, stmt);
        }
        expect_token(parser, '}');

        auto statements = temp_array_finalize(parser, temp_statements);
        return ast_block_stmt_new(parser->context, start_pos, statements);
    }

    // All remaining options start with an expression
    AST_Statement *result = nullptr;
    AST_Expression *expr = parse_expression(parser);
    if (expr->kind == AST_Expression_Kind::CALL) {
        result = ast_call_stmt_new(parser->context, start_pos, expr);

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

        AST_Declaration *decl = ast_variable_decl_new(parser->context, start_pos, expr->identifier, type_spec, value);
        return ast_declaration_stmt_new(parser->context, start_pos, decl);

    } else {
        expect_token(parser, '=');
        AST_Expression *value = parse_expression(parser);
        assert(value);
        result = ast_assign_stmt_new(parser->context, start_pos, expr, value);
    }

    assert(result);

    expect_token(parser, ';');

    return result;
}

AST_Declaration *parse_function_declaration(Parser *parser, AST_Identifier ident)
{
    assert(parser);

    expect_token(parser, '(');

    Dynamic_Array<AST_Field_Declaration> params = {};

    if (is_token(parser, TOK_NAME)) {

        auto temp_params = temp_array_create<AST_Field_Declaration>(parser);
        do {

            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);
            expect_token(parser, ':');
            AST_Type_Spec *ts = parse_type_spec(parser);

            AST_Identifier param_ident;
            ast_identifier_create(name_tok.atom, name_tok.start, &param_ident);

            dynamic_array_append(&temp_params.array, { param_ident, ts });
        } while (match_token(parser, ','));

        params = temp_array_finalize(parser, temp_params);

    }

    expect_token(parser, ')');

    AST_Type_Spec *return_ts = nullptr;

    if (match_token(parser, TOK_RIGHT_ARROW)) {
        return_ts = parse_type_spec(parser);
    }

    expect_token(parser, '{');

    auto temp_stmts = temp_array_create<AST_Statement *>(parser);

    while (!match_token(parser, '}')) {
        AST_Statement *stmt = parse_statement(parser);
        assert(stmt);
        dynamic_array_append(&temp_stmts.array, stmt);
    }

    auto statements = temp_array_finalize(parser, temp_stmts);

    return ast_function_decl_new(parser->context, ident.pos, ident, params, return_ts, statements);
}

AST_Declaration *parse_aggregate_declaration(Parser *parser, AST_Identifier ident)
{
    assert(parser);

    AST_Declaration_Kind kind = AST_Declaration_Kind::INVALID;

    if (match_keyword(parser, keyword_struct)) {
        kind = AST_Declaration_Kind::STRUCT;
    } else {
        expect_keyword(parser, keyword_union);
        kind = AST_Declaration_Kind::UNION;
    }

    assert(kind == AST_Declaration_Kind::STRUCT || kind == AST_Declaration_Kind::UNION);

    expect_token(parser, '{');

    auto temp_fields = temp_array_create<AST_Field_Declaration>(parser);

    while (!match_token(parser, '}')) {

        auto temp_idents = temp_array_create<AST_Identifier>(parser);

        // At least one name
        Token first_name_tok = cur_tok(parser);
        expect_token(parser, TOK_NAME);

        AST_Identifier first_ident;
        ast_identifier_create(first_name_tok.atom, first_name_tok.start, &first_ident);
        dynamic_array_append(&temp_idents.array, first_ident);

        while (match_token(parser, ',')) {
            Token name_tok = cur_tok(parser);
            expect_token(parser, TOK_NAME);

            AST_Identifier ident;
            ast_identifier_create(name_tok.atom, name_tok.start, &ident);
            dynamic_array_append(&temp_idents.array, ident);
        }

        expect_token(parser, ':');

        AST_Type_Spec *ts = parse_type_spec(parser);
        expect_token(parser, ';');

        for (u64 i = 0; i < temp_idents.array.count; i++) {
            dynamic_array_append(&temp_fields.array, { temp_idents.array[i], ts });
        }
    }

    auto fields = temp_array_finalize(parser, temp_fields);
    return ast_aggregate_decl_new(parser->context, ident.pos, ident, kind, fields);
}

AST_Declaration *parse_declaration(Parser *parser)
{
    AST_Identifier ident = parse_identifier(parser);
    assert(ident.name.data)

    expect_token(parser, ':');

    AST_Type_Spec *ts = nullptr;

    if (!is_token(parser, ':') &&  !is_token(parser, '=')) {
        ts = parse_type_spec(parser);

        if (!(is_token(parser, '=') || is_token(parser, ':') || is_token(parser, ';'))) {
            fatal_syntax_error(parser, "Expected '=', ':' or ';' after typespec in variable on constant declaration, got '%s'", cur_tok(parser).atom.data);
            return nullptr;
        }
    }

    if (match_token(parser, ':')) {

        if (is_keyword(parser, keyword_struct) || is_keyword(parser, keyword_union)) {
            return parse_aggregate_declaration(parser, ident);
        }

        // Constant
        if (is_token(parser, '(')) {
            if ((peek_token(parser).kind == Token_Kind::TOK_NAME && peek_token(parser, 2).kind == ':') ||
                 peek_token(parser).kind == ')') {

                assert(!ts);
                return parse_function_declaration(parser, ident);
            }
        }

        AST_Expression *const_value = parse_expression(parser);
        expect_token(parser, ';');

        return ast_constant_variable_decl_new(parser->context, ident.pos, ident, ts, const_value);

    } else if (is_token(parser, '=') || is_token(parser, ';')) {
        // Variable
        AST_Expression *value = nullptr;
        if (match_token(parser, '=')) {
            value = parse_expression(parser);
        }
        expect_token(parser, ';');

        return ast_variable_decl_new(parser->context, ident.pos, ident, ts, value);
    }

    fatal_syntax_error(parser, "Unexpected token: '%s' when parsing declaration", cur_tok(parser).atom.data);
    return nullptr;
}

AST_Type_Spec *parse_type_spec(Parser *parser)
{
    Token t = cur_tok(parser);
    switch (t.kind) {

        case TOK_STAR: {
            next_token(parser);
            AST_Type_Spec *base = parse_type_spec(parser);
            return ast_pointer_ts_new(parser->context, t.start, base);
        }

        case TOK_NAME: {
            next_token(parser);

            AST_Identifier ident;
            ast_identifier_create(t.atom, t.start, &ident);

            return ast_name_ts_new(parser->context, t.start, ident);
        }

        default: {
            fatal_syntax_error(parser, "Unexpected token when parsing typespec: '%s'", t.atom.data);
            return nullptr;
        }
    }

    assert(false);
    return nullptr;
}

AST_File *parse_file(Parser *parser)
{
    Dynamic_Array<AST_Declaration *> decls;
    dynamic_array_create(&parser->context->ast_allocator, &decls);

    while (!is_token(parser, TOK_EOF)) {
        AST_Declaration *decl = parse_declaration(parser);
        if (parser->error) break;

        dynamic_array_append(&decls, decl);
    }

    return ast_file_new(parser->context, decls);
}

bool is_keyword(Parser *parser, Atom keyword)
{
    assert(parser && is_keyword(keyword));

    return is_token(parser, TOK_KEYWORD) && cur_tok(parser).atom == keyword;
}

bool match_keyword(Parser *parser, Atom keyword)
{
    assert(parser && is_keyword(keyword));

    if (is_keyword(parser, keyword)) {
        next_token(parser);
        return true;
    }

    return false;
}

bool expect_keyword(Parser *parser, Atom keyword)
{
    assert(parser && is_keyword(keyword));

    if (is_keyword(parser, keyword)) {
        next_token(parser);
        return true;
    }

    return false;
}

bool is_token(Parser *parser, Token_Kind kind)
{
    assert(parser);
    return cur_tok(parser).kind == kind;
}

bool is_token(Parser *parser, char c)
{
    assert(parser);
    return (char)cur_tok(parser).kind == c;
}

bool next_token(Parser *parser)
{
    assert(parser && parser->lxr);

    if (!queue_empty(&parser->peeked_tokens)) {
        queue_dequeue(&parser->peeked_tokens);
        return true;
    } else {
        return next_token(parser->lxr);
    }
}

bool match_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);

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

bool expect_token(Parser *parser, Token_Kind kind)
{
    assert(parser && parser->lxr);

    if (is_token(parser, kind)) {
        next_token(parser);
        return true;
    }

    String_Ref kind_str = tmp_token_kind_str(kind);
    Token t = cur_tok(parser);
    fatal_syntax_error(parser, "Expected token: '%s', got: '%s'", kind_str.data, t.atom.data);
    return false;
}

bool expect_token(Parser *parser, char c)
{
    return expect_token(parser, (Token_Kind)c);
}

Token cur_tok(Parser *parser)
{
    assert(parser && parser->lxr);

    if (queue_empty(&parser->peeked_tokens)) {
        return parser->lxr->token;
    } else {
        return queue_peek(&parser->peeked_tokens, 0);
    }
}

Token peek_token(Parser *parser, u64 offset/*=1*/)
{
    assert(parser && parser->lxr);
    assert(offset >= 1);

    if (queue_empty(&parser->peeked_tokens)) {
        for (u64 i = 0; i < offset + 1; i++) {
            queue_enqueue(&parser->peeked_tokens, parser->lxr->token);
            next_token(parser->lxr);
        }
    } else {
        u64 peeked_count = queue_used(&parser->peeked_tokens);

        for (u64 i = 0; i < (offset + 1) - peeked_count; i++) {
            queue_enqueue(&parser->peeked_tokens, parser->lxr->token);
            next_token(parser->lxr);
        }
    }

    return queue_peek(&parser->peeked_tokens, offset);
}

void syntax_error(Parser *parser, const String_Ref fmt, ...)
{
    assert(parser);
    assert(fmt.length && fmt.data);
    assert(fmt.data[fmt.length] == '\0');

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
        assert(false && "Formatted syntax error does not fit in stack buffer");
        return;
    }

    Token t = cur_tok(parser);
    Source_Pos pos = t.start;

    out_length = string_format(err_msg, "%s:%i:%i: error: %s", pos.name.data, pos.line, pos.index_in_line, err_msg);

    if (out_length + 1 > ZSTRING_FORMAT_STACK_BUFFER_SIZE) {
        assert(false && "Formatted syntax error does not fit in stack buffer");
        return;
    }

    parser->error = true;
    printf("%s\n", err_msg);
}

void fatal_syntax_error(Parser *parser, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    syntax_error(parser, fmt, args);

    va_end(args);
    platform_exit(1);
}

}
