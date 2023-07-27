#include "ast.h"

#include <stdio.h>

#include "memory/allocator.h"
#include "util/asserts.h"
#include "util/string_builder.h"
#include "util/zstring.h"
#include "zodiac_context.h"

namespace Zodiac
{

void ast_identifier_create(Atom name, Source_Range range, AST_Identifier *out_ident)
{
    assert(out_ident);

    out_ident->name = name;
    out_ident->range = range;
    out_ident->scope = nullptr;
}

void ast_integer_literal_expr_create(Integer_Value value, AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::INTEGER_LITERAL, AST_EXPR_FLAG_CONST, out_expr);

    out_expr->integer_literal.value = value;
}

void ast_string_literal_expr_create(Atom atom, AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::STRING_LITERAL, AST_EXPR_FLAG_CONST, out_expr);

    out_expr->string_literal.atom = atom;
}

void ast_null_literal_expr_create(AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::NULL_LITERAL, AST_EXPR_FLAG_CONST, out_expr);
}

void ast_bool_literal_expr_create(AST_Expression *out_expr, bool value)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::BOOL_LITERAL, AST_EXPR_FLAG_CONST, out_expr);
    out_expr->bool_literal = value;
}

void ast_identifier_expr_create(AST_Identifier ident, AST_Expression *out_expr)
{
    assert(&out_expr);

    ast_expression_create(AST_Expression_Kind::IDENTIFIER, AST_EXPR_FLAG_NONE, out_expr);

    out_expr->identifier = ident;
}

void ast_member_expr_create(AST_Expression *base, Atom atom, AST_Expression *out_expr)
{
    assert(base && out_expr);

    ast_expression_create(AST_Expression_Kind::MEMBER, AST_EXPR_FLAG_NONE, out_expr);

    out_expr->member.base = base;
    out_expr->member.member_name = atom;
    out_expr->member.type_decl = nullptr;
}

void ast_index_expr_create(AST_Expression *base, AST_Expression *index, AST_Expression *out_expr)
{
    assert(base && index && out_expr);

    ast_expression_create(AST_Expression_Kind::INDEX, AST_EXPR_FLAG_NONE, out_expr);

    out_expr->index.base = base;
    out_expr->index.index = index;
}

void ast_call_expr_create(AST_Expression *base, Dynamic_Array<AST_Expression *> args, AST_Expression *out_expr)
{
    assert(base && out_expr);

    ast_expression_create(AST_Expression_Kind::CALL, AST_EXPR_FLAG_NONE, out_expr);

    out_expr->call.base = base;
    out_expr->call.args = args;
}

void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Expression *out_expr)
{
    assert(operand && out_expr);
    assert(op != AST_Unary_Operator::INVALID);

    ast_expression_create(AST_Expression_Kind::UNARY, AST_EXPR_FLAG_NONE, out_expr);

    out_expr->unary.op = op;
    out_expr->unary.operand = operand;
}

void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Expression *out_expr)
{
    assert(lhs && rhs && out_expr);
    assert(op != AST_Binary_Operator::INVALID);

    AST_Expression_Flags flags = AST_EXPR_FLAG_NONE;

    if (lhs->flags & AST_EXPR_FLAG_CONST && rhs->flags & AST_EXPR_FLAG_CONST) {
        flags |= AST_EXPR_FLAG_CONST;
    }

    ast_expression_create(AST_Expression_Kind::BINARY, flags, out_expr);

    out_expr->binary.op = op;
    out_expr->binary.lhs = lhs;
    out_expr->binary.rhs = rhs;
}

void ast_expression_create(AST_Expression_Kind kind, AST_Expression_Flags flags, AST_Expression *out_expr)
{
    assert(out_expr);

    out_expr->kind = kind;
    out_expr->flags = flags;
    out_expr->resolved_type = nullptr;
    out_expr->infer_type_from = nullptr;
}

void ast_block_stmt_create(Dynamic_Array<AST_Statement *> statements, AST_Statement *out_stmt)
{
    assert(out_stmt);

    ast_statement_create(AST_Statement_Kind::BLOCK, out_stmt);

    out_stmt->block.statements = statements;
    out_stmt->block.scope = nullptr;
}

void ast_declaration_stmt_create(AST_Declaration *decl, AST_Statement *out_stmt)
{
    assert(decl && out_stmt);

    ast_statement_create(AST_Statement_Kind::DECLARATION, out_stmt);

    out_stmt->decl.decl = decl;
}

void ast_assign_stmt_create(AST_Expression *dest, AST_Expression *value, AST_Statement *out_stmt)
{
    assert(dest && value && out_stmt);

    ast_statement_create(AST_Statement_Kind::ASSIGN, out_stmt);

    out_stmt->assign.dest = dest;
    out_stmt->assign.value = value;
}

void ast_call_stmt_create(AST_Expression *call, AST_Statement *out_stmt)
{
    assert(call && out_stmt);

    ast_statement_create(AST_Statement_Kind::CALL, out_stmt);

    out_stmt->call.call = call;
}

void ast_if_stmt_create(Dynamic_Array<AST_If_Block> blocks, AST_Statement *else_stmt, AST_Statement *out_stmt)
{
    assert(blocks.count && out_stmt);

    ast_statement_create(AST_Statement_Kind::IF, out_stmt);

    out_stmt->if_stmt.blocks = blocks;
    out_stmt->if_stmt.else_stmt = else_stmt;
    out_stmt->if_stmt.else_scope = nullptr;
}

void ast_while_stmt_create(AST_Expression *cond, AST_Statement *do_stmt, AST_Statement *out_stmt)
{
    assert(cond && do_stmt && out_stmt);

    ast_statement_create(AST_Statement_Kind::WHILE, out_stmt);

    out_stmt->while_stmt.cond = cond;
    out_stmt->while_stmt.do_stmt = do_stmt;
    out_stmt->while_stmt.scope = nullptr;
}

void ast_return_stmt_create(AST_Expression *value, AST_Statement *out_stmt)
{
    assert(out_stmt);

    ast_statement_create(AST_Statement_Kind::RETURN, out_stmt);

    out_stmt->return_stmt.value = value;
    out_stmt->return_stmt.scope = nullptr;
}

void ast_print_stmt_create(AST_Expression *print_expr, AST_Statement *out_stmt)
{
    assert(print_expr && out_stmt);

    ast_statement_create(AST_Statement_Kind::PRINT, out_stmt);

    out_stmt->print_expr = print_expr;
}

void ast_statement_create(AST_Statement_Kind kind, AST_Statement *out_stmt)
{
    assert(out_stmt);

    out_stmt->kind = kind;
}

void ast_variable_decl_create(AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value, AST_Declaration *out_decl)
{
    assert(out_decl);

    ast_declaration_create(AST_Declaration_Kind::VARIABLE, out_decl);

    out_decl->identifier = ident;
    out_decl->variable.type_spec = ts;
    out_decl->variable.value = value;
    out_decl->variable.resolved_type = nullptr;
}

void ast_constant_variable_decl_create(AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value, AST_Declaration *out_decl)
{
    assert(out_decl);
    assert_msg(value, "Constant variable declaration must have a value");

    ast_declaration_create(AST_Declaration_Kind::CONSTANT_VARIABLE, out_decl);

    out_decl->identifier = ident;
    out_decl->variable.type_spec = ts;
    out_decl->variable.value = value;
    out_decl->variable.resolved_type = nullptr;
}

void ast_parameter_decl_create(AST_Identifier ident, AST_Type_Spec *ts, Source_Range range, AST_Declaration *out_decl)
{
    assert(out_decl);

    ast_declaration_create(AST_Declaration_Kind::PARAMETER, out_decl);

    out_decl->identifier = ident;
    out_decl->parameter.type_spec = ts;
    out_decl->parameter.value = nullptr;
    out_decl->parameter.resolved_type = nullptr;
}

void ast_field_decl_create(AST_Identifier ident, AST_Type_Spec *ts, Source_Range range, AST_Declaration *out_decl)
{
    assert(out_decl);

    ast_declaration_create(AST_Declaration_Kind::FIELD, out_decl);

    out_decl->identifier = ident;
    out_decl->field.type_spec = ts;
    out_decl->field.value = nullptr;
    out_decl->field.resolved_type = nullptr;
}

void ast_function_decl_create(Allocator *allocator, AST_Identifier ident, Dynamic_Array<AST_Declaration *> args, AST_Type_Spec *return_ts, Dynamic_Array<AST_Statement *> body, AST_Declaration *out_decl)
{
    assert(out_decl);

    ast_declaration_create(AST_Declaration_Kind::FUNCTION, out_decl);

    out_decl->identifier = ident;
    out_decl->function.params = args;
    dynamic_array_create(allocator, &out_decl->function.variables, 0);
    out_decl->function.return_ts = return_ts;
    out_decl->function.body = body;
    out_decl->function.type = nullptr;
    out_decl->function.inferred_return_type = nullptr;
    out_decl->function.parameter_scope = nullptr;
    out_decl->function.local_scope = nullptr;
}

void ast_aggregate_decl_create(AST_Identifier ident, AST_Declaration_Kind kind, Dynamic_Array<AST_Declaration *> fields, AST_Declaration *out_decl)
{
    assert(out_decl);
    assert(kind == AST_Declaration_Kind::STRUCT || kind == AST_Declaration_Kind::UNION);

    ast_declaration_create(kind, out_decl);

    out_decl->identifier = ident;
    out_decl->aggregate.fields = fields;
    out_decl->aggregate.resolved_type = nullptr;
}

void ast_declaration_create(AST_Declaration_Kind kind, AST_Declaration *out_decl)
{
    assert(out_decl);

    out_decl->kind = kind;
}

void ast_name_ts_create(AST_Identifier ident, AST_Type_Spec *out_ts)
{
    assert(out_ts)


    ast_type_spec_create(AST_Type_Spec_Kind::NAME, out_ts);

    out_ts->identifier = ident;
}

void ast_pointer_ts_create(AST_Type_Spec *base, AST_Type_Spec *out_ts)
{
    assert(base && out_ts);

    ast_type_spec_create(AST_Type_Spec_Kind::POINTER, out_ts);

    out_ts->base = base;
}

void ast_type_spec_create(AST_Type_Spec_Kind kind, AST_Type_Spec *out_ts)
{
    assert(out_ts);

    out_ts->kind = kind;
}

void ast_file_create(Dynamic_Array<AST_Declaration *> decls, AST_File *out_file)
{
    assert(out_file);

    out_file->declarations = decls;
}

AST_Expression *ast_integer_literal_expr_new(Zodiac_Context *ctx, Source_Range range, Integer_Value value)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx, range);
    ast_integer_literal_expr_create(value, expr);
    return expr;
}

AST_Expression *ast_string_literal_expr_new(Zodiac_Context *ctx, Source_Range range, Atom atom)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx, range);
    ast_string_literal_expr_create(atom, expr);
    return expr;
}

AST_Expression *ast_null_literal_expr_new(Zodiac_Context *ctx, Source_Range range)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx, range);
    ast_null_literal_expr_create(expr);
    return expr;
}

AST_Expression *ast_bool_literal_expr_new(Zodiac_Context *ctx, Source_Range range, bool value)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx, range);
    ast_bool_literal_expr_create(expr, value);

    return expr;
}

AST_Expression *ast_identifier_expr_new(Zodiac_Context *ctx, Source_Range range, Atom atom)
{
    assert(ctx);

    AST_Identifier ident;
    ast_identifier_create(atom, range, &ident);

    auto expr = ast_expression_new(ctx, range);
    ast_identifier_expr_create(ident, expr);
    return expr;
}

AST_Expression *ast_member_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *base, Atom atom)
{
    assert(ctx && base);

    auto expr = ast_expression_new(ctx, range);
    ast_member_expr_create(base, atom, expr);
    return expr;
}

AST_Expression *ast_index_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *base, AST_Expression *index)
{
    assert(ctx && base && index);

    auto expr = ast_expression_new(ctx, range);
    ast_index_expr_create(base, index, expr);
    return expr;
}

AST_Expression *ast_call_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *base, Dynamic_Array<AST_Expression *> args)
{
    assert(ctx && base);

    auto expr = ast_expression_new(ctx, range);
    ast_call_expr_create(base, args, expr);
    return expr;

}

AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Unary_Operator op, AST_Expression *operand)
{
    assert(ctx && operand);

    auto expr = ast_expression_new(ctx, range);
    ast_unary_expr_create(op, operand, expr);
    return expr;
}

AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, Source_Range range, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs)
{
    assert(ctx && lhs && rhs);

    auto expr = ast_expression_new(ctx, range);
    ast_binary_expr_create(op, lhs, rhs, expr);
    return expr;
}

AST_Expression *ast_expression_new(Zodiac_Context *ctx, Source_Range range)
{
    AST_Expression *result = alloc<AST_Expression>(&ctx->ast_allocator);
    result->range = range;
    return result;
}

AST_Statement *ast_block_stmt_new(Zodiac_Context *ctx, Source_Range range, Dynamic_Array<AST_Statement *> statements)
{
    assert(ctx);

    auto stmt = ast_statement_new(ctx, range);
    ast_block_stmt_create(statements, stmt);
    return stmt;
}

AST_Statement *ast_declaration_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Declaration *decl)
{
    assert(ctx && decl);

    auto stmt = ast_statement_new(ctx, range);
    ast_declaration_stmt_create(decl, stmt);
    return stmt;
}

AST_Statement *ast_assign_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *dest, AST_Expression *value)
{
    assert(ctx && dest && value);

    auto stmt = ast_statement_new(ctx, range);
    ast_assign_stmt_create(dest, value, stmt);
    return stmt;
}

AST_Statement *ast_call_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *call)
{
    assert(ctx && call);

    auto stmt = ast_statement_new(ctx, range);
    ast_call_stmt_create(call, stmt);
    return stmt;
}

AST_Statement *ast_if_stmt_new(Zodiac_Context *ctx, Source_Range range, Dynamic_Array<AST_If_Block> blocks, AST_Statement *else_stmt)
{
    assert(ctx && blocks.count);

    auto stmt = ast_statement_new(ctx, range);
    ast_if_stmt_create(blocks, else_stmt, stmt);
    return stmt;
}

AST_Statement *ast_while_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *cond, AST_Statement *do_stmt)
{
    assert(ctx && cond && do_stmt);

    auto stmt = ast_statement_new(ctx, range);
    ast_while_stmt_create(cond, do_stmt, stmt);
    return stmt;
}

AST_Statement *ast_return_stmt_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *value)
{
    assert(ctx);

    auto stmt = ast_statement_new(ctx, range);
    ast_return_stmt_create(value, stmt);
    return stmt;
}

AST_Statement *ast_print_statement_new(Zodiac_Context *ctx, Source_Range range, AST_Expression *print_expr)
{
    assert(ctx);

    auto stmt = ast_statement_new(ctx, range);
    ast_print_stmt_create(print_expr, stmt);
    return stmt;
}

AST_Statement *ast_statement_new(Zodiac_Context *ctx, Source_Range range)
{
    assert(ctx);

    AST_Statement *result = alloc<AST_Statement>(&ctx->ast_allocator);
    result->range = range;
    return result;
}

AST_Declaration *ast_variable_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value)
{
    assert(ctx);

    auto decl = ast_declaration_new(ctx, range);
    ast_variable_decl_create(ident, ts, value, decl);
    return decl;
}

AST_Declaration *ast_constant_variable_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts, AST_Expression *value)
{
    assert(ctx && value);

    auto decl = ast_declaration_new(ctx, range);
    ast_constant_variable_decl_create(ident, ts, value, decl);
    return decl;
}

AST_Declaration *ast_parameter_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts)
{
    assert(ctx);

    auto decl = ast_declaration_new(ctx, range);
    ast_parameter_decl_create(ident, ts, range, decl);

    return decl;
}

AST_Declaration *ast_field_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Type_Spec *ts)
{
    assert(ctx);

    auto decl = ast_declaration_new(ctx, range);
    ast_field_decl_create(ident, ts, range, decl);

    return decl;
}

AST_Declaration *ast_function_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, Dynamic_Array<AST_Declaration *> args, AST_Type_Spec *return_ts, Dynamic_Array<AST_Statement *> body)
{
    assert(ctx);

    auto decl = ast_declaration_new(ctx, range);
    ast_function_decl_create(&ctx->ast_allocator, ident, args, return_ts, body, decl);
    return decl;
}

AST_Declaration *ast_aggregate_decl_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident, AST_Declaration_Kind kind, Dynamic_Array<AST_Declaration *> fields)
{
    assert(ctx);
    assert(kind == AST_Declaration_Kind::STRUCT || kind == AST_Declaration_Kind::UNION);

    auto decl = ast_declaration_new(ctx, range);
    ast_aggregate_decl_create(ident, kind, fields, decl);
    return decl;
}

AST_Declaration *ast_declaration_new(Zodiac_Context *ctx, Source_Range range)
{
    assert(ctx);

    AST_Declaration *result = alloc<AST_Declaration>(&ctx->ast_allocator);
    result->range = range;
    return result;
}

file_local const char *ast_binop_to_string[(int)AST_Binary_Operator::LAST_BINOP + 1] = {

    [(int)AST_Binary_Operator::ADD] = "+",
    [(int)AST_Binary_Operator::SUB] = "-",
    [(int)AST_Binary_Operator::MUL] = "*",
    [(int)AST_Binary_Operator::DIV] = "/",

    [(int)AST_Binary_Operator::EQ] = "==",
    [(int)AST_Binary_Operator::NEQ] = "!=",
    [(int)AST_Binary_Operator::LT] = "<",
    [(int)AST_Binary_Operator::GT] = ">",
    [(int)AST_Binary_Operator::LTEQ] = "<=",
    [(int)AST_Binary_Operator::GTEQ] = ">=",
};

AST_Type_Spec *ast_name_ts_new(Zodiac_Context *ctx, Source_Range range, AST_Identifier ident)
{
    assert(ctx);

    auto ts = ast_type_spec_new(ctx, range);
    ast_name_ts_create(ident, ts);
    return ts;
}

AST_Type_Spec *ast_pointer_ts_new(Zodiac_Context *ctx, Source_Range range, AST_Type_Spec *base)
{
    assert(ctx && base);

    auto ts = ast_type_spec_new(ctx, range);
    ast_pointer_ts_create(base, ts);
    return ts;
}

AST_Type_Spec *ast_type_spec_new(Zodiac_Context *ctx, Source_Range range)
{
    assert(ctx);

    AST_Type_Spec *result = alloc<AST_Type_Spec>(&ctx->ast_allocator);
    result->range = range;
    return result;
}

AST_File *ast_file_new(Zodiac_Context *ctx, Dynamic_Array<AST_Declaration *> decls)
{
    assert(ctx);

    auto file = alloc<AST_File>(&ctx->ast_allocator);
    ast_file_create(decls, file);
    return file;
}

void ast_print_expression(String_Builder *sb, AST_Expression *expr)
{
    assert(sb && expr);

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false);

        case AST_Expression_Kind::INTEGER_LITERAL: {
            string_builder_append(sb, "%i", expr->integer_literal.value.u64);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            auto atom = expr->string_literal.atom;
            string_builder_append(sb, "\"%.*s\"", (int)atom.length, atom.data);
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            string_builder_append(sb, "null");
            break;
        }

        case AST_Expression_Kind::BOOL_LITERAL: {
            string_builder_append(sb, "%s", expr->bool_literal ? "true" : "false");
            break;
        }

        case AST_Expression_Kind::IDENTIFIER: {
            string_builder_append(sb, "%s", expr->identifier.name.data);
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            ast_print_expression(sb, expr->member.base);
            string_builder_append(sb, ".%s", expr->member.member_name.data);
            break;
        }

        case AST_Expression_Kind::INDEX: {
            ast_print_expression(sb, expr->index.base);
            string_builder_append(sb, "[");
            ast_print_expression(sb, expr->index.index);
            string_builder_append(sb, "]");
            break;
        }

        case AST_Expression_Kind::CALL: {
            ast_print_expression(sb, expr->index.base);
            string_builder_append(sb, "(");
            for (u64 i = 0; i < expr->call.args.count; i++) {
                if (i != 0) string_builder_append(sb, ", ");
                ast_print_expression(sb, expr->call.args[i]);
            }
            string_builder_append(sb, ")");
            break;
        }

        case AST_Expression_Kind::UNARY: {
            string_builder_append(sb, "%c", (char)expr->unary.op);
            ast_print_expression(sb, expr->unary.operand);
            break;
        }

        case AST_Expression_Kind::BINARY: {
            string_builder_append(sb, "(");
            ast_print_expression(sb, expr->binary.lhs);
            string_builder_append(sb, " %s ", ast_binop_to_string[(int)expr->binary.op]);
            ast_print_expression(sb, expr->binary.rhs);
            string_builder_append(sb, ")");
            break;
        }
    }
}

void ast_print_indent(String_Builder *sb, int indent) {
    for (int i = 0; i < indent; i++) {
        string_builder_append(sb, "  ");
    }
}

file_local void ast__print_statement_internal(String_Builder *sb, AST_Statement *stmt, int indent, bool newline = true)
{
    assert(stmt);

    if (stmt->kind == AST_Statement_Kind::BLOCK) {
        string_builder_append(sb, " {\n");
        for (u64 i = 0; i < stmt->block.statements.count; i++) {
            ast_print_statement(sb, stmt->block.statements[i], indent + 1);
        }
        string_builder_append(sb, "\n");
        ast_print_indent(sb, indent);
        string_builder_append(sb, "}");

    } else {
        string_builder_append(sb, "\n");
        ast_print_statement(sb, stmt, indent + 1);
        if (newline) string_builder_append(sb, "\n");
    }
}

void ast_print_statement(String_Builder *sb, AST_Statement *stmt, int indent/*=0*/)
{
    assert(sb && stmt);

    bool semicolon = true;

    ast_print_indent(sb, indent);

    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false);

        case AST_Statement_Kind::BLOCK: {
            semicolon = false;
            string_builder_append(sb, "{ \n");
            for (u64 i = 0; i < stmt->block.statements.count; i++) {
                ast_print_statement(sb, stmt->block.statements[i], indent + 1);
                string_builder_append(sb, "\n");
            }
            ast_print_indent(sb, indent);
            string_builder_append(sb, "}");
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            semicolon = false;
            ast_print_declaration(sb, stmt->decl.decl, indent);
            break;
        }

        case AST_Statement_Kind::ASSIGN: {
            ast_print_expression(sb, stmt->assign.dest);
            string_builder_append(sb, " = ");
            ast_print_expression(sb, stmt->assign.value);
            break;
        }

        case AST_Statement_Kind::CALL: {
            ast_print_expression(sb, stmt->call.call);
            break;
        }

        case AST_Statement_Kind::IF: {
            semicolon = false;

            assert(stmt->if_stmt.blocks.count);
            bool indent_else_if = stmt->if_stmt.blocks[0].then->kind != AST_Statement_Kind::BLOCK;

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = &stmt->if_stmt.blocks[i];
                if (i == 0) {
                    string_builder_append(sb, "if ");
                } else {
                    string_builder_append(sb, " else if ");
                }
                ast_print_expression(sb, if_block->cond);
                ast__print_statement_internal(sb, if_block->then, indent);

                indent_else_if = if_block->then->kind != AST_Statement_Kind::BLOCK;
            }

            bool indent_else = indent_else_if;

            if (stmt->if_stmt.else_stmt) {
                if (indent_else) ast_print_indent(sb, indent);
                else string_builder_append(sb, " ");
                string_builder_append(sb, "else");
                ast__print_statement_internal(sb, stmt->if_stmt.else_stmt, indent, !indent_else);
            }

            break;
        }

        case AST_Statement_Kind::WHILE: {
            semicolon = false;
            string_builder_append(sb, "while ");
            ast_print_expression(sb, stmt->while_stmt.cond);
            ast__print_statement_internal(sb, stmt->while_stmt.do_stmt, indent, false);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            string_builder_append(sb, "return");

            if (stmt->return_stmt.value) {
                string_builder_append(sb, " ");
                ast_print_expression(sb, stmt->return_stmt.value);
            }
            break;
        }

        case AST_Statement_Kind::PRINT: {
            string_builder_append(sb, "print(");
            ast_print_expression(sb, stmt->print_expr);
            string_builder_append(sb, ")");
            break;
        }
    }

    if (semicolon) string_builder_append(sb, ";");
}

void ast_print_declaration(String_Builder *sb, AST_Declaration *decl, int indent/*=0*/)
{
    assert(sb && decl);

    bool semicolon = false;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE: {
            semicolon = true;
            string_builder_append(sb, "%s :", decl->identifier.name.data);
            if (decl->variable.type_spec) {
                string_builder_append(sb, " ");
                ast_print_type_spec(sb, decl->variable.type_spec);
            }
            if (decl->variable.value) {
                if (decl->variable.type_spec) {
                    string_builder_append(sb, " ");
                }
                string_builder_append(sb, "= ");
                ast_print_expression(sb, decl->variable.value);
            }
            break;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            semicolon = true;
            string_builder_append(sb, "%s :", decl->identifier.name.data);
            if (decl->variable.type_spec) {
                string_builder_append(sb, " ");
                ast_print_type_spec(sb, decl->variable.type_spec);
            }
            if (decl->variable.value) {
                if (decl->variable.type_spec) {
                    string_builder_append(sb, " ");
                }
                string_builder_append(sb, ": ");
                ast_print_expression(sb, decl->variable.value);
            }
            break;
        }

        case AST_Declaration_Kind::PARAMETER: assert(false); break;
        case AST_Declaration_Kind::FIELD: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {
            string_builder_append(sb, "%s :: (", decl->identifier.name.data);
            for (u64 i = 0; i < decl->function.params.count; i++) {
                if (i > 0) string_builder_append(sb, ", ");
                string_builder_append(sb, "%s: ", decl->function.params[i]->identifier.name.data);
                ast_print_type_spec(sb, decl->function.params[i]->parameter.type_spec);
            }
            string_builder_append(sb, ") -> ");
            if (decl->function.return_ts) {
                ast_print_type_spec(sb, decl->function.return_ts);
            } else {
                string_builder_append(sb, "void");
            }
            string_builder_append(sb, " {\n");
            for (u64 i = 0; i < decl->function.body.count; i++) {
                ast_print_statement(sb, decl->function.body[i], 1);
                string_builder_append(sb, "\n");
            }
            string_builder_append(sb, "}");
            break;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            semicolon = false;
            string_builder_append(sb, "%s :: ", decl->identifier.name.data);
            if (decl->kind == AST_Declaration_Kind::STRUCT) {
                string_builder_append(sb, "struct {\n");
            } else {
                string_builder_append(sb, "union {\n");
            }
            for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "%s: ", decl->aggregate.fields[i]->identifier.name.data);
                ast_print_type_spec(sb, decl->aggregate.fields[i]->field.type_spec);
                string_builder_append(sb, ";\n");
            }
            string_builder_append(sb, "}");
            break;
        }
    }

    if (semicolon) string_builder_append(sb, ";");
}

void ast_print_declaration(AST_Declaration *decl)
{
    assert(decl);

    String_Builder sb;
    string_builder_create(&sb);

    ast_print_declaration(&sb, decl, 0);

    String ast_str = string_builder_to_string(&sb);
    printf("%.*s", (int)ast_str.length, ast_str.data);
    free(sb.allocator, ast_str.data);

    string_builder_destroy(&sb);
}

file_local void ast__print_type_spec_internal(String_Builder *sb, AST_Type_Spec *ts, int indent)
{
    assert(sb && ts);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::NAME: {
            string_builder_append(sb, "%s", ts->identifier.name.data);
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            string_builder_append(sb, "*");
            ast__print_type_spec_internal(sb, ts->base, indent);
            break;
        }
    }
}

void ast_print_type_spec(String_Builder *sb, AST_Type_Spec *ts, int indent/*=0*/)
{
    string_builder_append(sb, "type_spec(");
    ast__print_type_spec_internal(sb, ts, indent);
    string_builder_append(sb, ")");
}

void ast_print_file(String_Builder *sb, AST_File *file)
{
    assert(sb && file);

    for (u64 i = 0; i < file->declarations.count; i++) {
        ast_print_declaration(sb, file->declarations[i]);
        string_builder_append(sb, "\n\n");
    }
}

void ast_print_file(AST_File *file)
{
    String_Builder sb;
    string_builder_create(&sb);

    ast_print_file(&sb, file);

    String ast_str = string_builder_to_string(&sb);
    printf("%.*s", (int)ast_str.length, ast_str.data);
    free(sb.allocator, ast_str.data);

    string_builder_destroy(&sb);
}

bool is_binary_arithmetic_op(AST_Binary_Operator op)
{
    return op >= AST_Binary_Operator::FIRST_ARITHMETIC_OP && op <= AST_Binary_Operator::LAST_ARITHMETIC_OP;
}

bool is_binary_cmp_op(AST_Binary_Operator op)
{
    return op >= AST_Binary_Operator::FIRST_CMP_OP && op <= AST_Binary_Operator::LAST_CMP_OP;
}

}
