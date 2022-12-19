#include "resolve.h"

#include "ast.h"
#include "scope.h"
#include "zodiac_context.h"

namespace Zodiac
{

Scope *global_scope;
Scope *current_scope;

Dynamic_Array<Statement_Scope> statement_scopes;

u64 name_resolved_count = 0;

Dynamic_Array<Resolve_Error> resolve_errors;
bool fatal_resolve_error;

Zodiac_Context *ctx;

Scope *get_statement_scope(AST_Statement *stmt)
{
    assert(stmt);
    for (u64 i = 0; i < statement_scopes.count; i++) {

        if (statement_scopes[i].stmt == stmt) {
            assert(statement_scopes[i].scope);
            return statement_scopes[i].scope;
        }
    }

    return nullptr;
}

void add_statement_scope(AST_Statement *stmt, Scope *scope)
{
    assert(stmt && scope);
    assert(scope->kind == Scope_Kind::FUNCTION_LOCAL);

    assert(!get_statement_scope(stmt));

    dynamic_array_append(&statement_scopes, { stmt, scope });
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args)
{
    Resolve_Error err;

    err.message = string_format(&ctx->resolve_error_allocator, fmt, args);
    err.pos = pos;
    err.fatal = fatal;

    dynamic_array_append(&resolve_errors, err);
}

void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(decl->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(stmt->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(expr->pos, fatal, fmt, args);
    va_end(args);
}

void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    resolve_error_(ts->pos, fatal, fmt, args);
    va_end(args);
}
}
