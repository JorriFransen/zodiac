#include "ast.h"

#include <asserts.h>
#include <string_builder.h>

namespace Zodiac
{

void ast_integer_literal_expr_create(Integer_Value value, AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::INTEGER_LITERAL, out_expr);

    out_expr->integer_literal.value = value;
}

void ast_identifier_expr_create(Atom atom, AST_Expression *out_expr)
{
    assert(out_expr);

    ast_expression_create(AST_Expression_Kind::IDENTIFIER, out_expr);

    out_expr->identifier.atom = atom;
}

void ast_member_expr_create(AST_Expression *base, Atom atom, AST_Expression *out_expr)
{
    assert(base && out_expr);

    ast_expression_create(AST_Expression_Kind::MEMBER, out_expr);

    out_expr->member.base = base;
    out_expr->member.member_name = atom;
}

void ast_index_expr_create(AST_Expression *base, AST_Expression *index, AST_Expression *out_expr)
{
    assert(base && index && out_expr);

    ast_expression_create(AST_Expression_Kind::INDEX, out_expr);

    out_expr->index.base = base;
    out_expr->index.index = index;
}

void ast_call_expr_create(AST_Expression *base, Dynamic_Array<AST_Expression *> args, AST_Expression *out_expr)
{
    assert(base && out_expr);

    ast_expression_create(AST_Expression_Kind::CALL, out_expr);

    out_expr->call.base = base;
    out_expr->call.args = args;
}

void ast_unary_expr_create(AST_Unary_Operator op, AST_Expression *operand, AST_Expression *out_expr)
{
    assert(operand && out_expr);
    assert(op != AST_Unary_Operator::INVALID);

    ast_expression_create(AST_Expression_Kind::UNARY, out_expr);

    out_expr->unary.op = op;
    out_expr->unary.operand = operand;
}

void ast_binary_expr_create(AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs, AST_Expression *out_expr)
{
    assert(lhs && rhs && out_expr);
    assert(op != AST_Binary_Operator::INVALID);

    ast_expression_create(AST_Expression_Kind::BINARY, out_expr);

    out_expr->binary.op = op;
    out_expr->binary.lhs = lhs;
    out_expr->binary.rhs = rhs;
}

void ast_expression_create(AST_Expression_Kind kind, AST_Expression *out_expr)
{
    assert(out_expr);

    out_expr->kind = kind;
}

void ast_block_stmt_create(Dynamic_Array<AST_Statement *> statements, AST_Statement *out_stmt)
{
    assert(out_stmt);

    ast_statement_create(AST_Statement_Kind::BLOCK, out_stmt);

    out_stmt->block.statements = statements;
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

void ast_if_stmt_create(AST_Expression *cond, AST_Statement *then_stmt, Dynamic_Array<AST_Else_If> else_ifs, AST_Statement *else_stmt, AST_Statement *out_stmt)
{
    assert(cond && then_stmt && out_stmt);

    ast_statement_create(AST_Statement_Kind::IF, out_stmt);

    out_stmt->if_stmt.cond = cond;
    out_stmt->if_stmt.then_stmt = then_stmt;
    out_stmt->if_stmt.else_ifs = else_ifs;
    out_stmt->if_stmt.else_stmt = else_stmt;
}

void ast_while_stmt_create(AST_Expression *cond, AST_Statement *do_stmt, AST_Statement *out_stmt)
{
    assert(cond && do_stmt && out_stmt);

    ast_statement_create(AST_Statement_Kind::WHILE, out_stmt);

    out_stmt->while_stmt.cond = cond;
    out_stmt->while_stmt.do_stmt = do_stmt;
}

void ast_return_stmt_create(AST_Expression *value, AST_Statement *out_stmt)
{
    assert(out_stmt);

    ast_statement_create(AST_Statement_Kind::RETURN, out_stmt);

    out_stmt->return_stmt.value = value;
}

void ast_statement_create(AST_Statement_Kind kind, AST_Statement *out_stmt)
{
    assert(out_stmt);

    out_stmt->kind = kind;
}

void ast_variable_decl_create(AST_Expression *identifier, AST_Type_Spec *ts, AST_Expression *value, AST_Declaration *out_decl)
{
    assert(identifier && out_decl);
    assert(identifier->kind == AST_Expression_Kind::IDENTIFIER);

    ast_declaration_create(AST_Declaration_Kind::VARIABLE, out_decl);

    out_decl->identifier = identifier;
    out_decl->variable.type_spec = ts;
    out_decl->variable.value = value;
}

void ast_constant_variable_decl_create(AST_Expression *identifier, AST_Type_Spec *ts, AST_Expression *value, AST_Declaration *out_decl)
{
    assert(identifier && out_decl);
    assert(identifier->kind == AST_Expression_Kind::IDENTIFIER);

    ast_declaration_create(AST_Declaration_Kind::CONSTANT_VARIABLE, out_decl);

    out_decl->identifier = identifier;
    out_decl->constant_variable.type_spec = ts;
    out_decl->constant_variable.value = value;
}

void ast_function_decl_create(AST_Expression *identifier, Dynamic_Array<AST_Field_Declaration> args, AST_Type_Spec *return_ts, Dynamic_Array<AST_Statement *> body, AST_Declaration *out_decl)
{
    assert(out_decl);
    assert(identifier->kind == AST_Expression_Kind::IDENTIFIER);

    ast_declaration_create(AST_Declaration_Kind::FUNCTION, out_decl);

    out_decl->identifier = identifier;
    out_decl->function.args = args;
    out_decl->function.return_ts = return_ts;
    out_decl->function.body = body;
}

void ast_declaration_create(AST_Declaration_Kind kind, AST_Declaration *out_decl)
{
    assert(out_decl);

    out_decl->kind = kind;
}

void ast_integer_ts_create(bool sign, u64 bit_size, AST_Type_Spec *out_ts)
{
    assert(bit_size && out_ts);

    ast_type_spec_create(AST_Type_Spec_Kind::INTEGER, out_ts);

    out_ts->bit_size = bit_size;
    out_ts->integer.sign = sign;
}

void ast_type_spec_create(AST_Type_Spec_Kind kind, AST_Type_Spec *out_ts)
{
    assert(out_ts);

    out_ts->kind = kind;
}

AST_Expression *ast_integer_literal_expr_new(Zodiac_Context *ctx, Integer_Value value)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx);
    ast_integer_literal_expr_create(value, expr);
    return expr;
}

AST_Expression *ast_identifier_expr_new(Zodiac_Context *ctx, Atom atom)
{
    assert(ctx);

    auto expr = ast_expression_new(ctx);
    ast_identifier_expr_create(atom, expr);
    return expr;
}

AST_Expression *ast_member_expr_new(Zodiac_Context *ctx, AST_Expression *base, Atom atom)
{
    assert(ctx && base);

    auto expr = ast_expression_new(ctx);
    ast_member_expr_create(base, atom, expr);
    return expr;
}

AST_Expression *ast_index_expr_new(Zodiac_Context *ctx, AST_Expression *base, AST_Expression *index)
{
    assert(ctx && base && index);

    auto expr = ast_expression_new(ctx);
    ast_index_expr_create(base, index, expr);
    return expr;
}

AST_Expression *ast_call_expr_new(Zodiac_Context *ctx, AST_Expression *base, Dynamic_Array<AST_Expression *> args)
{
    assert(ctx && base);

    auto expr = ast_expression_new(ctx);
    ast_call_expr_create(base, args, expr);
    return expr;

}

AST_Expression *ast_unary_expr_new(Zodiac_Context *ctx, AST_Unary_Operator op, AST_Expression *operand)
{
    assert(ctx && operand);

    auto expr = ast_expression_new(ctx);
    ast_unary_expr_create(op, operand, expr);
    return expr;
}

AST_Expression *ast_binary_expr_new(Zodiac_Context *ctx, AST_Binary_Operator op, AST_Expression *lhs, AST_Expression *rhs)
{
    assert(ctx && lhs && rhs);

    auto expr = ast_expression_new(ctx);
    ast_binary_expr_create(op, lhs, rhs, expr);
    return expr;
}

AST_Expression *ast_expression_new(Zodiac_Context *ctx)
{
    return alloc<AST_Expression>(ctx->ast_allocator);
}

AST_Statement *ast_block_stmt_new(Zodiac_Context *ctx, Dynamic_Array<AST_Statement *> statements)
{
    assert(ctx);

    auto stmt = ast_statement_new(ctx);
    ast_block_stmt_create(statements, stmt);
    return stmt;
}

AST_Statement *ast_declaration_stmt_new(Zodiac_Context *ctx, AST_Declaration *decl)
{
    assert(ctx && decl);

    auto stmt = ast_statement_new(ctx);
    ast_declaration_stmt_create(decl, stmt);
    return stmt;
}

AST_Statement *ast_assign_stmt_new(Zodiac_Context *ctx, AST_Expression *dest, AST_Expression *value)
{
    assert(ctx && dest && value);

    auto stmt = ast_statement_new(ctx);
    ast_assign_stmt_create(dest, value, stmt);
    return stmt;
}

AST_Statement *ast_call_stmt_new(Zodiac_Context *ctx, AST_Expression *call)
{
    assert(ctx && call);

    auto stmt = ast_statement_new(ctx);
    ast_call_stmt_create(call, stmt);
    return stmt;
}

AST_Statement *ast_if_stmt_new(Zodiac_Context *ctx, AST_Expression *cond, AST_Statement *then_stmt, Dynamic_Array<AST_Else_If> else_ifs, AST_Statement *else_stmt)
{
    assert(ctx && cond && then_stmt);

    auto stmt = ast_statement_new(ctx);
    ast_if_stmt_create(cond, then_stmt, else_ifs, else_stmt, stmt);
    return stmt;
}

AST_Statement *ast_while_stmt_new(Zodiac_Context *ctx, AST_Expression *cond, AST_Statement *do_stmt)
{
    assert(ctx && cond && do_stmt);

    auto stmt = ast_statement_new(ctx);
    ast_while_stmt_create(cond, do_stmt, stmt);
    return stmt;
}

AST_Statement *ast_return_stmt_new(Zodiac_Context *ctx, AST_Expression *value)
{
    assert(ctx);

    auto stmt = ast_statement_new(ctx);
    ast_return_stmt_create(value, stmt);
    return stmt;
}

AST_Statement *ast_statement_new(Zodiac_Context *ctx)
{
    assert(ctx);

    return alloc<AST_Statement>(ctx->ast_allocator);
}

AST_Declaration *ast_variable_decl_new(Zodiac_Context *ctx, AST_Expression *identifier, AST_Type_Spec *ts, AST_Expression *value)
{
    assert(ctx && identifier);
    assert(identifier->kind == AST_Expression_Kind::IDENTIFIER);

    auto decl = ast_declaration_new(ctx);
    ast_variable_decl_create(identifier, ts, value, decl);
    return decl;
}

AST_Declaration *ast_constant_variable_decl_new(Zodiac_Context *ctx, AST_Expression *identifier, AST_Type_Spec *ts, AST_Expression *value)
{
    assert(ctx && identifier);
    assert(identifier->kind == AST_Expression_Kind::IDENTIFIER);

    auto decl = ast_declaration_new(ctx);
    ast_constant_variable_decl_create(identifier, ts, value, decl);
    return decl;
}

AST_Declaration *ast_function_decl_new(Zodiac_Context *ctx, AST_Expression *identifier, Dynamic_Array<AST_Field_Declaration> args, AST_Type_Spec *return_ts, Dynamic_Array<AST_Statement *> body)
{
    assert(ctx && identifier);
    assert(identifier->kind == AST_Expression_Kind::IDENTIFIER);

    auto decl = ast_declaration_new(ctx);
    ast_function_decl_create(identifier, args, return_ts, body, decl);
    return decl;
}

AST_Declaration *ast_declaration_new(Zodiac_Context *ctx)
{
    assert(ctx);

    return alloc<AST_Declaration>(ctx->ast_allocator);
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

AST_Type_Spec *ast_integer_ts_new(Zodiac_Context *ctx, bool sign, u64 bit_size)
{
    assert(ctx && bit_size);

    auto ts = ast_type_spec_new(ctx);
    ast_integer_ts_create(sign, bit_size, ts);
    return ts;
}

AST_Type_Spec *ast_type_spec_new(Zodiac_Context *ctx)
{
    assert(ctx);

    return alloc<AST_Type_Spec>(ctx->ast_allocator);
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

        case AST_Expression_Kind::IDENTIFIER: {
            string_builder_append(sb, "%s", expr->identifier.atom.data);
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
            string_builder_append(sb, "if ");
            ast_print_expression(sb, stmt->if_stmt.cond);
            ast__print_statement_internal(sb, stmt->if_stmt.then_stmt, indent);

            bool indent_else_if = stmt->if_stmt.then_stmt->kind != AST_Statement_Kind::BLOCK;

            for (u64 i = 0; i < stmt->if_stmt.else_ifs.count; i++) {
                auto else_if = stmt->if_stmt.else_ifs[i];
                if (indent_else_if) ast_print_indent(sb, indent);
                else string_builder_append(sb, " ");
                string_builder_append(sb, "else if ");
                ast_print_expression(sb, else_if.cond);
                ast__print_statement_internal(sb, else_if.then, indent);

                indent_else_if = else_if.then->kind != AST_Statement_Kind::BLOCK;
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
            ast_print_expression(sb, decl->identifier);
            string_builder_append(sb, " :");
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
            ast_print_expression(sb, decl->identifier);
            string_builder_append(sb, " :");
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

        case AST_Declaration_Kind::FUNCTION: {
            ast_print_expression(sb, decl->identifier);
            string_builder_append(sb, " :: (");
            for (u64 i = 0; i < decl->function.args.count; i++) {
                if (i > 0) string_builder_append(sb, ", ");
                string_builder_append(sb, "%s: ", decl->function.args[i].name.data);
                ast_print_type_spec(sb, decl->function.args[i].type_spec);
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
    }

    if (semicolon) string_builder_append(sb, ";");
}

void ast_print_type_spec(String_Builder *sb, AST_Type_Spec *ts, int indent/*=0*/)
{
    assert(sb && ts);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false);

        case AST_Type_Spec_Kind::INTEGER: {
            string_builder_append(sb, "type_spec(");
            if (ts->integer.sign) string_builder_append(sb, "s");
            else string_builder_append(sb, "u");
            string_builder_append(sb, "%i", ts->bit_size);
            string_builder_append(sb, ")");
            break;
        }
    }
}

}
