// #include "resolve.h"

// #include <stdio.h>

// #include "ast.h"
// #include "memory/temporary_allocator.h"
// #include "memory/zmemory.h"
// #include "parser.h" // for atom_s64 etc.
// #include "zodiac_context.h"

// namespace Zodiac
// {

// Scope *global_scope;
// Scope *current_scope;

// Dynamic_Array<Statement_Scope> statement_scopes;

// u64 name_resolved_count = 0;

// Dynamic_Array<Resolve_Error> resolve_errors;
// bool fatal_resolve_error;

// Zodiac_Context *ctx;

// Scope *get_statement_scope(AST_Statement *stmt)
// {
//     assert(stmt);
//     for (u64 i = 0; i < statement_scopes.count; i++) {

//         if (statement_scopes[i].stmt == stmt) {
//             assert(statement_scopes[i].scope);
//             return statement_scopes[i].scope;
//         }
//     }

//     return nullptr;
// }

// void add_statement_scope(AST_Statement *stmt, Scope *scope)
// {
//     assert(stmt && scope);
//     assert(scope->kind == Scope_Kind::FUNCTION_LOCAL);

//     assert(!get_statement_scope(stmt));

//     dynamic_array_append(&statement_scopes, { stmt, scope });
// }

//  #define add_builtin_symbol(kind, atom) {                                                              \
//     add_resolved_symbol(global_scope, (kind), (SYM_FLAG_GLOBAL | SYM_FLAG_BUILTIN), (atom), nullptr); \
// }

// void resolve_test(Zodiac_Context *ctx, AST_File *file)
// {
//     assert(ctx);

//     global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);
//     dynamic_array_create(&dynamic_allocator, &statement_scopes);
//     dynamic_array_create(&dynamic_allocator, &resolve_errors);

//     add_builtin_symbol(Symbol_Kind::TYPE, atom_s64);
//     add_builtin_symbol(Symbol_Kind::TYPE, atom_r32);
//     add_builtin_symbol(Symbol_Kind::TYPE, atom_String);

//     auto decls_to_resolve = dynamic_array_copy(&file->declarations, &dynamic_allocator);

//     // Add global symbols
//     for (u64 i = 0; i < decls_to_resolve.count; i++) {
//         auto decl = decls_to_resolve[i];
//         add_unresolved_decl_symbol(global_scope, decl, true);
//     }

//     bool progress = true;
//     bool done = false;

//     while (progress && !done && !fatal_resolve_error) {

//         auto last_name_resolved_count = name_resolved_count;

//         done = true;
//         for (u64 i = 0; i < decls_to_resolve.count; i++) {

//             AST_Declaration *decl = decls_to_resolve[i];

//             if (decl) {
//                 current_scope = global_scope;
//                 if (name_resolve_decl_(decl, true)) {
//                     // Set the decl to null instead of removing it, don't want to do unordered removal!
//                     decls_to_resolve[i] = nullptr;

//                 } else {
//                     done = false;
//                 }
//             }
//         }

//         progress = last_name_resolved_count < name_resolved_count;

//         if (progress && !fatal_resolve_error) {
//             resolve_errors.count = 0;
//             temporary_allocator_reset(&ctx->resolve_error_allocator_state);
//         }
//     }

//     dynamic_array_free(&decls_to_resolve);

//     for (u64 i = 0; i < resolve_errors.count; i++) {
//         auto err = resolve_errors[i];

//         if (!fatal_resolve_error) assert(err.fatal == false);

//         bool print = fatal_resolve_error == err.fatal;

//         if (print) {
//             printf("%s:%llu:%llu: error: %s\n", err.pos.name.data, err.pos.line, err.pos.index_in_line, err.message.data);
//         }
//     }

//     if (resolve_errors.count == 0) {
//         for (u64 i = 0; i < global_scope->symbols.count; i++) {

//             auto sym = global_scope->symbols[i];
//             if (sym.state == Symbol_State::RESOLVED && sym.decl && (sym.flags & SYM_FLAG_GLOBAL)) {
//                 ast_print_declaration(sym.decl);
//                 printf("\n\n");
//             }
//         }
//     }
// }
// bool name_resolve_decl_(AST_Declaration *decl, bool global)
// {
//     assert(decl);

//     assert(current_scope);
//     assert(current_scope->kind == Scope_Kind::GLOBAL || current_scope->kind == Scope_Kind::FUNCTION_LOCAL);

//     auto decl_sym = scope_get_symbol(current_scope, decl->identifier.name);
//     if (decl_sym && decl_sym->decl != decl) {
//         report_redecl(decl_sym, decl_sym->name, decl->identifier.pos);
//         return false;
//     }

//     if (global) {
//         assert_msg(decl_sym, "Global symbol should have been registered already");
//     } else if (!decl_sym) {
//         // First time local symbol is encountered
//         if (!add_unresolved_decl_symbol(current_scope, decl, global)) {
//             return false;
//         }
//         decl_sym = scope_get_symbol(current_scope, decl->identifier);
//     }
//     assert(decl_sym && decl_sym->decl == decl);

//     switch (decl_sym->state) {
//         case Symbol_State::UNRESOLVED: decl_sym->state = Symbol_State::RESOLVING; break;
//         case Symbol_State::RESOLVING: assert(false); // circ dep
//         case Symbol_State::RESOLVED: return true;
//     }

//     Scope *aggregate_scope = nullptr;
//     Scope *parameter_scope = nullptr;
//     Scope *local_scope = nullptr;

//     switch (decl_sym->kind) {

//         default: break;

//         case Symbol_Kind::FUNC: {
//             assert(decl_sym->func.parameter_scope && decl_sym->func.local_scope);
//             parameter_scope = decl_sym->func.parameter_scope;
//             local_scope = decl_sym->func.local_scope;
//             break;
//         }

//         case Symbol_Kind::TYPE: {
//             assert(decl_sym->aggregate.scope);
//             aggregate_scope = decl_sym->aggregate.scope;
//             break;
//         }

//     }

//     bool result = true;

//     switch (decl->kind) {
//         case AST_Declaration_Kind::INVALID: assert(false);

//         case AST_Declaration_Kind::VARIABLE:
//         case AST_Declaration_Kind::CONSTANT_VARIABLE: {
//             auto ts = decl->variable.type_spec;
//             auto expr = decl->variable.value;

//             if (ts) name_resolve_ts(ts);
//             if (expr) name_resolve_expr(expr);

//             if (decl->kind == AST_Declaration_Kind::CONSTANT_VARIABLE) {

//                 if (!expr) {
//                     fatal_resolve_error(decl, "Expected value for constant variable declaration");
//                     return false;
//                 }

//                 if (!is_const_expr(expr)) {
//                     fatal_resolve_error(expr, "Value for constant variable declaration is not constant");
//                     return false;
//                 }

//             } else if (decl->kind == AST_Declaration_Kind::VARIABLE) {

//                 if (global && !is_const_expr(expr)) {
//                     fatal_resolve_error(expr, "Value for global variable declaration must be constant");
//                     return false;
//                 }
//             }

//             break;
//         }

//         case AST_Declaration_Kind::FUNCTION: {
//             assert(parameter_scope);
//             assert(local_scope);

//             for (u64 i = 0; i < decl->function.params.count; i++) {
//                 auto param = decl->function.params[i];
//                 auto param_sym = scope_get_symbol(parameter_scope, param.identifier);

//                 if (!param_sym) {
//                     resolve_error(param.identifier.pos, "Undeclared symbol: '%s'", param.identifier.name.data);
//                     result = false;
//                     break;
//                 }

//                 assert(param_sym->kind == Symbol_Kind::PARAM);

//                 switch (param_sym->state) {
//                     case Symbol_State::UNRESOLVED: param_sym->state = Symbol_State::RESOLVING; break;
//                     case Symbol_State::RESOLVING: assert(false); // Circular dependency
//                     case Symbol_State::RESOLVED: continue;
//                 }

//                 name_resolve_ts(param.type_spec);

//                 param_sym = scope_get_symbol(parameter_scope, param.identifier);
//                 param_sym->state = Symbol_State::RESOLVED;
//             }

//             auto return_ts = decl->function.return_ts;
//             if (return_ts) name_resolve_ts(return_ts);

//             Scope *old_scope = current_scope;
//             current_scope = local_scope;

//             for (u64 i = 0; i < decl->function.body.count; i++) {
//                 auto stmt = decl->function.body[i];

//                 name_resolve_stmt(stmt);
//             }

//             current_scope = old_scope;

//             break;
//         }

//         case AST_Declaration_Kind::STRUCT:
//         case AST_Declaration_Kind::UNION: {
//             assert(aggregate_scope);

//             for (u64 i = 0 ; i < decl->aggregate.fields.count; i++) {

//                 auto field = decl->aggregate.fields[i];
//                 auto field_sym = scope_get_symbol(aggregate_scope, field.identifier);

//                 if (!field_sym) {
//                     resolve_error(field.identifier.pos, "Undeclared symbol: '%s'", field.identifier.name.data);
//                     result = false;
//                     break;
//                 }

//                 assert(field_sym->kind == Symbol_Kind::MEMBER);

//                 switch (field_sym->state) {
//                     case Symbol_State::UNRESOLVED: field_sym->state = Symbol_State::RESOLVING; break;
//                     case Symbol_State::RESOLVING: assert(false); // Circular dependency
//                     case Symbol_State::RESOLVED: continue;
//                 }

//                 name_resolve_ts(field.type_spec);

//                 field_sym = scope_get_symbol(aggregate_scope, field.identifier);
//                 field_sym->state = Symbol_State::RESOLVED;
//             }
//             break;
//         }
//     }



// exit:
//     if (decl->kind == AST_Declaration_Kind::STRUCT || decl->kind == AST_Declaration_Kind::UNION) {
//         assert(aggregate_scope)
//         for (u64 i = 0; i < decl->aggregate.fields.count; i++) {
//             auto field_sym = scope_get_symbol(aggregate_scope, decl->aggregate.fields[i].identifier);
//             assert(field_sym);
//             if (field_sym->state == Symbol_State::RESOLVING) field_sym->state = Symbol_State::UNRESOLVED;
//         }
//     } else if (decl->kind == AST_Declaration_Kind::FUNCTION) {
//         assert(parameter_scope);
//         for (u64 i = 0; i < decl->function.params.count; i++) {
//             auto param_sym = scope_get_symbol(parameter_scope, decl->function.params[i].identifier);
//             assert(param_sym);
//             if (param_sym->state == Symbol_State::RESOLVING) param_sym->state = Symbol_State::UNRESOLVED;
//         }
//     }

//     decl_sym = scope_get_symbol(current_scope, decl->identifier.name);
//     assert(decl_sym->state == Symbol_State::RESOLVING);

//     if (result) {
//         decl_sym->state = Symbol_State::RESOLVED;
//         name_resolved_count += 1;
//     } else {
//         decl_sym->state = Symbol_State::UNRESOLVED;
//     }

//     return result;
// }

// bool name_resolve_stmt_(AST_Statement *stmt)
// {
//     assert(stmt);

//     bool result = true;

//     switch (stmt->kind) {
//         case AST_Statement_Kind::INVALID: assert(false);

//         case AST_Statement_Kind::BLOCK: {

//             Scope *block_scope = get_statement_scope(stmt);
//             if (!block_scope) {
//                 block_scope = scope_new(&dynamic_allocator, Scope_Kind::FUNCTION_LOCAL, current_scope);
//                 add_statement_scope(stmt, block_scope);
//             }

//             Scope *old_scope = current_scope;
//             current_scope = block_scope;

//             for (u64 i = 0; i < stmt->block.statements.count; i++) {
//                 name_resolve_stmt(stmt->block.statements[i]);
//             }

//             current_scope = old_scope;
//             break;
//         }

//         case AST_Statement_Kind::DECLARATION: {
//             name_resolve_decl(stmt->decl.decl, false);
//             break;
//         }

//         case AST_Statement_Kind::ASSIGN: {
//             name_resolve_expr(stmt->assign.dest);

//             if (!is_lvalue_expr(stmt->assign.dest)) {
//                 fatal_resolve_error(stmt->assign.dest, "Left side of assignment must be an lvalue.");
//                 return false;
//             }

//             name_resolve_expr(stmt->assign.value);
//             break;
//         }

//         case AST_Statement_Kind::CALL: {
//             name_resolve_expr(stmt->call.call);
//             break;
//         }

//         case AST_Statement_Kind::IF: assert(false);
//         case AST_Statement_Kind::WHILE: assert(false);

//         case AST_Statement_Kind::RETURN: {
//             auto value = stmt->return_stmt.value;
//             if (value) name_resolve_expr(value);
//             break;
//         }

//         case AST_Statement_Kind::PRINT: {
//             name_resolve_expr(stmt->print_expr);
//             break;
//         }
//     }

// exit:
//     return result;
// }

// bool name_resolve_expr_(AST_Expression *expr)
// {
//     assert(expr);

//     bool result = true;

//     switch (expr->kind) {
//         case AST_Expression_Kind::INVALID: assert(false);

//         case AST_Expression_Kind::INTEGER_LITERAL:
//         case AST_Expression_Kind::STRING_LITERAL:
//         case AST_Expression_Kind::NULL_LITERAL: return true;

//         case AST_Expression_Kind::IDENTIFIER: {
//             Symbol *sym = scope_get_symbol(current_scope, expr->identifier.name);

//             if (!sym) {
//                 resolve_error(expr, "Undefined symbol: '%s'", expr->identifier.name.data);
//                 return false;
//             }

//             if (sym->state == Symbol_State::RESOLVING) {

//                 fatal_resolve_error(expr, "Circular dependency detected");
//                 result = false;
//                 break;

//             } else if (sym->state == Symbol_State::UNRESOLVED) {

//                 bool global = sym->flags & SYM_FLAG_GLOBAL;
//                 switch (sym->kind) {
//                     case Symbol_Kind::INVALID: assert(false);

//                     case Symbol_Kind::FUNC:
//                     case Symbol_Kind::TYPE:
//                     case Symbol_Kind::MEMBER: {
//                         assert(global);
//                         resolve_error(expr, "Unresolved symbol: '%s'", expr->identifier.name.data);
//                         return false;
//                     }

//                     case Symbol_Kind::VAR:
//                     case Symbol_Kind::CONST:
//                     case Symbol_Kind::PARAM: {
//                         assert(global);
//                         assert(sym->decl);
//                         name_resolve_decl(sym->decl, global);
//                         break;
//                     }
//                 }

//             } else {
//                 assert(sym->state == Symbol_State::RESOLVED);
//             }

//             break;
//         }

//         case AST_Expression_Kind::MEMBER: assert(false);
//         case AST_Expression_Kind::INDEX: assert(false);

//         case AST_Expression_Kind::CALL: {

//             auto base = expr->call.base;
//             name_resolve_expr(base);

//             // TODO: Support more complex base expressions
//             assert(base->kind == AST_Expression_Kind::IDENTIFIER);
//             auto base_sym = scope_get_symbol(current_scope, base->identifier);
//             assert(base_sym);

//             if (base_sym->kind != Symbol_Kind::FUNC) {
//                 assert(base->kind == AST_Expression_Kind::IDENTIFIER);
//                 fatal_resolve_error(base, "Call expression base is not a function: '%s'", base->identifier.name.data);
//                 return false;
//             }

//             for (u64 i = 0; i < expr->call.args.count; i++) {
//                 auto arg = expr->call.args[i];

//                 name_resolve_expr(arg);
//             }

//             break;
//         }

//         case AST_Expression_Kind::UNARY: assert(false);

//         case AST_Expression_Kind::BINARY: {
//             name_resolve_expr(expr->binary.lhs);
//             name_resolve_expr(expr->binary.rhs);
//             break;
//         }
//     }

// exit:
//     return result;
// }

// bool name_resolve_ts_(AST_Type_Spec *ts)
// {
//     assert(ts);

//     bool result = true;

//     switch (ts->kind) {
//         case AST_Type_Spec_Kind::INVALID: assert(false);

//         case AST_Type_Spec_Kind::NAME: {
//             auto sym = scope_get_symbol(current_scope, ts->identifier);
//             if (!sym) {
//                 resolve_error(ts, "Undeclared symbol: '%s'", ts->identifier.name.data);
//                 return false;
//             }

//             if (sym->kind != Symbol_Kind::TYPE) {
//                 fatal_resolve_error(ts, "Not a type: '%s'", ts->identifier.name.data);
//                 return false;
//             }

//             switch (sym->state) {
//                 case Symbol_State::UNRESOLVED: {
//                     resolve_error(ts, "Unresolved symbol: '%s'", ts->identifier.name.data);
//                     return false;
//                 }
//                 case Symbol_State::RESOLVING: assert(false);
//                 case Symbol_State::RESOLVED: return true;
//             }
//             break;
//         }

//         case AST_Type_Spec_Kind::POINTER: {
//             name_resolve_ts(ts->base);
//             break;
//         }
//     }

// exit:
//     return result;
// }

// bool is_lvalue_expr(AST_Expression *expr)
// {
//     assert(expr);

//     switch(expr->kind) {
//         case AST_Expression_Kind::INVALID: assert(false);

//         case AST_Expression_Kind::INTEGER_LITERAL:
//         case AST_Expression_Kind::STRING_LITERAL:
//         case AST_Expression_Kind::NULL_LITERAL: return false;

//         case AST_Expression_Kind::IDENTIFIER: {
//             Symbol *sym = scope_get_symbol(current_scope, expr->identifier);
//             assert(sym);

//             assert_msg(!(sym->flags & SYM_FLAG_BUILTIN), "Maybe handle this?");

//             return sym->kind == Symbol_Kind::VAR || sym->kind == Symbol_Kind::PARAM;
//             break;
//         }

//         case AST_Expression_Kind::MEMBER: assert(false);
//         case AST_Expression_Kind::INDEX: assert(false);
//         case AST_Expression_Kind::CALL: assert(false);
//         case AST_Expression_Kind::UNARY: assert(false);
//         case AST_Expression_Kind::BINARY: assert(false);
//     }

//     assert(false);
//     return false;
// }

// bool is_const_expr(AST_Expression *expr)
// {
//     assert(expr);

//     switch (expr->kind) {
//         case AST_Expression_Kind::INVALID: assert(false);

//         case AST_Expression_Kind::INTEGER_LITERAL:
//         case AST_Expression_Kind::STRING_LITERAL:
//         case AST_Expression_Kind::NULL_LITERAL: return true;

//         case AST_Expression_Kind::IDENTIFIER: {
//             Symbol *sym = scope_get_symbol(current_scope, expr->identifier);
//             assert(sym);

//             assert_msg(!(sym->flags & SYM_FLAG_BUILTIN), "Maybe handle this?");

//             return sym->kind == Symbol_Kind::CONST;
//         }

//         case AST_Expression_Kind::MEMBER: assert(false);
//         case AST_Expression_Kind::INDEX: assert(false);
//         case AST_Expression_Kind::CALL: assert(false);
//         case AST_Expression_Kind::UNARY: assert(false);
//         case AST_Expression_Kind::BINARY: assert(false);
//     }

//     assert(false);
//     return false;
// }

// void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, va_list args)
// {
//     Resolve_Error err;

//     err.message = string_format(&ctx->resolve_error_allocator, fmt, args);
//     err.pos = pos;
//     err.fatal = fatal;

//     dynamic_array_append(&resolve_errors, err);
// }

// void resolve_error_(Source_Pos pos, bool fatal, const String_Ref fmt, ...)
// {
//     va_list args;
//     va_start(args, fmt);
//     resolve_error_(pos, fatal, fmt, args);
//     va_end(args);
// }

// void resolve_error_(AST_Declaration *decl, bool fatal, const String_Ref fmt, ...)
// {
//     va_list args;
//     va_start(args, fmt);
//     resolve_error_(decl->pos, fatal, fmt, args);
//     va_end(args);
// }

// void resolve_error_(AST_Statement *stmt, bool fatal, const String_Ref fmt, ...)
// {
//     va_list args;
//     va_start(args, fmt);
//     resolve_error_(stmt->pos, fatal, fmt, args);
//     va_end(args);
// }

// void resolve_error_(AST_Expression *expr, bool fatal, const String_Ref fmt, ...)
// {
//     va_list args;
//     va_start(args, fmt);
//     resolve_error_(expr->pos, fatal, fmt, args);
//     va_end(args);
// }

// void resolve_error_(AST_Type_Spec *ts, bool fatal, const String_Ref fmt, ...)
// {
//     va_list args;
//     va_start(args, fmt);
//     resolve_error_(ts->pos, fatal, fmt, args);
//     va_end(args);
// }
// }
