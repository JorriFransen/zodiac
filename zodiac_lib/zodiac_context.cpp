#include "zodiac_context.h"

#include "ast.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "containers/hash_table.h"
#include "error.h"
#include "lexer.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "scope.h"
#include "source_pos.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"

#include "error.h" // IWYU pragma: keep

#include <stdio.h>

#ifdef ZODIAC_VDL
#include "vld.h"
#endif // ZODIAC_VDL

namespace Zodiac
{

using namespace Bytecode;

void zodiac_context_create(Zodiac_Options options, Zodiac_Context *out_context)
{
    assert(out_context);

    *out_context = {};

    auto temp_mark = temporary_allocator_get_mark(temp_allocator());

    out_context->options = options;
    if (options.verbose) {
        logging_system_set_max_level(Log_Level::TRACE);
    }

    atom_table_init(&out_context->atoms);

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->ast_allocator_state);
    out_context->ast_allocator = linear_allocator_allocator(&out_context->ast_allocator_state);

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->bytecode_allocator_state);
    out_context->bytecode_allocator = linear_allocator_allocator(&out_context->bytecode_allocator_state);

    temporary_allocator_create(KIBIBYTE(8), nullptr, &out_context->error_allocator_state);
    out_context->error_allocator = temporary_allocator_allocator(&out_context->error_allocator_state);

    dynamic_array_create(c_allocator(), &out_context->errors);
    out_context->parse_error = false;
    out_context->fatal_resolve_error = false;

    if (!type_system_initialized) {
        bool result = type_system_initialize(out_context);
        assert(result);
    }

    dynamic_array_create(&dynamic_allocator, &out_context->files_to_parse);
    dynamic_array_create(&dynamic_allocator, &out_context->parsed_files);

    out_context->resolver = alloc<Resolver>(&dynamic_allocator);
    resolver_create(out_context->resolver, out_context);

    out_context->bytecode_builder = alloc<Bytecode_Builder>(&dynamic_allocator);
    bytecode_builder_init(&out_context->bytecode_allocator, out_context, out_context->bytecode_builder);

    out_context->bytecode_converter = alloc<Bytecode_Converter>(&dynamic_allocator);
    bytecode_converter_init(&out_context->bytecode_allocator, out_context, out_context->bytecode_builder, out_context->bytecode_converter);

    out_context->interp_stdout_file = nullptr;

    zodiac_register_keywords(&out_context->atoms);

    out_context->compiler_exe_path = filesystem_exe_path(&dynamic_allocator);
    assert(filesystem_exists(out_context->compiler_exe_path));

    out_context->compiler_exe_dir = filesystem_dir_name(&dynamic_allocator, out_context->compiler_exe_path);
    assert(filesystem_exists(out_context->compiler_exe_dir));

    auto tmp_install_dir = filesystem_dir_name(temp_allocator_allocator(), out_context->compiler_exe_dir);
    assert(filesystem_exists(tmp_install_dir));

    out_context->module_dir = string_format(&dynamic_allocator, "%s" ZODIAC_PATH_SEPARATOR "%s", tmp_install_dir.data, "modules");
    assert(filesystem_exists(out_context->module_dir));

    out_context->builtin_module_path = string_format(&dynamic_allocator, "%s" ZODIAC_PATH_SEPARATOR "%s", out_context->module_dir.data, "builtin.zc");
    assert(filesystem_exists(out_context->module_dir));

#ifdef ZPLATFORM_LINUX
    auto dynamic_support_lib_name = "/libzrs.so";
    auto static_support_lib_name = "/libzrs_s.a";

#elif ZPLATFORM_WINDOWS
    auto dynamic_support_lib_name = "\\libzrs.lib";
    auto dynamic_support_dll_name = "\\libzrs.dll";
    auto static_support_lib_name = "\\libzrs_s.lib";

    out_context->support_dll_dynamic_path = string_append(&dynamic_allocator, out_context->compiler_exe_dir, dynamic_support_dll_name);
    assert(filesystem_exists(out_context->support_dll_dynamic_path));
#else
    static_assert(false, "Unsupported platform");
#endif

    out_context->support_lib_dynamic_path = string_append(&dynamic_allocator, out_context->compiler_exe_dir, dynamic_support_lib_name);
    assert(filesystem_exists(out_context->support_lib_dynamic_path));

    out_context->support_lib_static_path = string_append(&dynamic_allocator, out_context->compiler_exe_dir, static_support_lib_name);
    assert(filesystem_exists(out_context->support_lib_static_path));

    out_context->interp = alloc<Interpreter>(&dynamic_allocator);
    interpreter_init(&dynamic_allocator, out_context, out_context->interp);

    out_context->builtin_string_type = nullptr;
    out_context->builtin_type_info_kind_type = nullptr;
    out_context->builtin_type_info_type = nullptr;
    out_context->builtin_type_info_int_type = nullptr;
    out_context->builtin_type_info_pointer_type = nullptr;
    out_context->builtin_type_info_struct_type = nullptr;
    out_context->builtin_type_info_struct_member_type = nullptr;
    out_context->builtin_type_info_enum_type = nullptr;
    out_context->builtin_type_info_enum_member_type = nullptr;
    out_context->builtin_type_info_static_array_type = nullptr;
    out_context->builtin_type_info_slice_type = nullptr;
    out_context->builtin_type_info_function_type = nullptr;

    dynamic_array_create(&dynamic_allocator, &out_context->type_infos);
    dynamic_array_create(&dynamic_allocator, &out_context->type_info_types);

    if (!out_context->options.output_filename_specified && out_context->options.input_file_name.length > 0) {

        auto in_base = filesystem_base_name(temp_allocator_allocator(), out_context->options.input_file_name);
        auto dot_index = string_last_index_of(in_base, '.');

        if (dot_index != -1) {
            in_base.length = dot_index;
        }
        out_context->options.output_file_name = string_append(&dynamic_allocator, in_base, ZPLATFORM_DEFAULT_EXE_EXTENSION);
        out_context->options.output_filename_specified = true;
    }

    temporary_allocator_reset(temp_allocator(), temp_mark);
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    // TODO: This should not be global state, but part of the context...
    type_system_initialized = false;

    atom_table_free(&context->atoms);

    dynamic_array_free(&context->errors);

    dynamic_array_free(&context->files_to_parse);
    dynamic_array_free(&context->parsed_files);

    resolver_destroy(context->resolver);
    free(&dynamic_allocator, context->resolver);

    bytecode_builder_free(context->bytecode_builder);
    free(&dynamic_allocator, context->bytecode_builder);

    bytecode_converter_destroy(context->bytecode_converter);
    free(&dynamic_allocator, context->bytecode_converter);

    free(&dynamic_allocator, context->compiler_exe_path.data);
    free(&dynamic_allocator, context->compiler_exe_dir.data);
    free(&dynamic_allocator, context->module_dir.data);
    free(&dynamic_allocator, context->builtin_module_path.data);

    linear_allocator_destroy(&context->ast_allocator_state);
    linear_allocator_destroy(&context->bytecode_allocator_state);
    linear_allocator_destroy(&context->error_allocator_state.linear_allocator);

#ifdef ZPLATFORM_WINDOWS
    free(&dynamic_allocator, context->support_dll_dynamic_path.data);
#endif // ZPLATFORM_WINDOWS

    free(&dynamic_allocator, context->support_lib_dynamic_path.data);
    free(&dynamic_allocator, context->support_lib_static_path.data);
}

bool zodiac_context_compile(Zodiac_Context *ctx, File_To_Parse ftp)
{
    debug_assert(ctx);

    if (ftp.kind == File_To_Parse_Kind::PATH) {
        if (!filesystem_is_regular(ftp.path)) {
            ZFATAL("Invalid path '%.*s' (input_file_name)", (int)ftp.path.length, ftp.path.data);
        }
    }

    // Add the builtin module
    File_To_Parse builtin_ftp = {
        .kind = File_To_Parse_Kind::PATH,
        .path = atom_get(&ctx->atoms, ctx->builtin_module_path),
    };

    dynamic_array_append(&ctx->files_to_parse, builtin_ftp);
    dynamic_array_append(&ctx->files_to_parse, ftp);

    bool parser_done = false;

    Resolve_Results resolve_result = RESOLVE_RESULT_NONE;

    int cycle = 0;

    while (!parser_done || !(resolve_result & RESOLVE_RESULT_DONE) || ctx->resolver->nodes_to_run_bytecode.count) {

        parser_done = do_parse_jobs(ctx);
        if (ctx->parse_error) {
            // do_parse_jobs() should have reported any errors
            return false;
        }

        if (ctx->options.report_errors) {
            if (resolver_report_errors(ctx->resolver)) {
                return false;
            }
        } else if (ctx->errors.count) return false;


        resolve_result = resolver_cycle(ctx->resolver);

        if (ctx->options.report_errors) {
            if (resolver_report_errors(ctx->resolver)) {
                return false;
            }
        } else if (ctx->errors.count) return false;

        assert(ctx->errors.count == 0);
        bool bc_res = emit_bytecode(ctx->resolver, ctx->bytecode_converter);
        if (!bc_res) return false;

        auto old_run_count = ctx->resolver->nodes_to_run_bytecode.count;

        bool bytecode_error = false;
        for (s64 i = 0; i < ctx->resolver->nodes_to_run_bytecode.count; i++) {
            auto node = ctx->resolver->nodes_to_run_bytecode[i];

            if (do_run_job(ctx, node)) {
                dynamic_array_remove_ordered(&ctx->resolver->nodes_to_run_bytecode, i);
                i--;
            } else {
                bytecode_error = true;
            }
        }

        if (bytecode_error && (resolve_result & RESOLVE_RESULT_PROGRESS)) {
            ctx->errors.count = 0;
            temporary_allocator_reset(&ctx->error_allocator_state);
        } else {

            if (ctx->options.report_errors) {
                if (resolver_report_errors(ctx->resolver)) {
                    return false;
                }
            } else if (ctx->errors.count) return false;
        }

        if ((!(resolve_result & RESOLVE_RESULT_PROGRESS)) && ctx->resolver->nodes_to_run_bytecode.count == old_run_count) {
            ZERROR("Exitting compile loop, there was no resolve progress, and no bytecode ran (cycle %i)", cycle);
            return false;
        }

        cycle++;
    }

    ZTRACE("Done after %i cycles");

    if (ctx->options.print_ast) {
        for (s64 i = 0; i < ctx->parsed_files.count; i++) {
            ast_print_file(ctx->parsed_files[i]);
        }
    }

    if (ctx->options.validate_bytecode) {
        Bytecode_Validator validator = {};
        bytecode_validator_init(ctx, temp_allocator_allocator(), &validator, ctx->bytecode_builder->functions, nullptr);
        defer { bytecode_validator_free(&validator); };
        bool bytecode_valid = validate_bytecode(&validator);

        if (!bytecode_valid) {
            assert(validator.errors.count);

            if (ctx->options.print_bytecode) {
                bytecode_print(ctx->bytecode_builder, temp_allocator_allocator());
            }

            bytecode_validator_print_errors(&validator);
            return false;
        }
    }

    if (ctx->options.print_bytecode) bytecode_print(ctx->bytecode_builder, temp_allocator_allocator());

    if (!ctx->options.dont_emit_binary) {

        auto program = bytecode_get_program(ctx->bytecode_builder);
        LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), ctx->bytecode_builder);

        llvm_builder_emit_program(&llvm_builder, &program);

        if (ctx->options.print_llvm_ir) llvm_builder_print(&llvm_builder);

        llvm_builder_emit_binary(&llvm_builder);

        llvm_builder_free(&llvm_builder);
    }

    return true;
}

bool zodiac_context_compile(Zodiac_Context *ctx, String_Ref code, Atom origin)
{
    File_To_Parse ftp = {
        .kind = File_To_Parse_Kind::STRING,
        .path = origin,
        .source = code,
    };
    return zodiac_context_compile(ctx, ftp);
}

bool zodiac_context_compile(Zodiac_Context *ctx, String_Ref code, const char *origin)
{
    Atom origin_atom = atom_get(&ctx->atoms, origin);
    return zodiac_context_compile(ctx, code, origin_atom);
}

bool zodiac_context_compile(Zodiac_Context *ctx)
{
    File_To_Parse ftp = {
        .kind = File_To_Parse_Kind::PATH,
        .path = atom_get(&ctx->atoms, ctx->options.input_file_name),
    };
    return zodiac_context_compile(ctx, ftp);
}

bool do_parse_jobs(Zodiac_Context *ctx)
{
    for (s64 i = 0; i < ctx->files_to_parse.count; i++) {
        auto ftp = &ctx->files_to_parse[i];

        String src_;
        if (ftp->kind == File_To_Parse_Kind::PATH) {

            bool read_res = filesystem_read_entire_file(&dynamic_allocator, ftp->path, &src_);
            assert(read_res);
            ftp->source = src_;
        } else {
            assert(ftp->kind == File_To_Parse_Kind::STRING);
        }

        Lexer lexer;
        lexer_create(ctx, &lexer);
        defer { lexer_destroy(&lexer); };

        lexer_init_stream(&lexer, ftp->source, ftp->path);

        Parser parser;
        parser_create(ctx, &lexer, &parser);
        defer { parser_destroy(&parser); };

        assert(ftp->path.length);
        AST_File *file = parse_file(&parser);

        if (parser.error) {
            bool lex_err = false;
            if (ctx->options.report_errors) {
                for (s64 i = 0; i < ctx->errors.count; i++) {
                    auto &err = ctx->errors[i];
                    assert(err.kind == Zodiac_Error_Kind::ZODIAC_LEX_ERROR || err.kind == Zodiac_Error_Kind::ZODIAC_PARSE_ERROR);
                    if (err.kind == Zodiac_Error_Kind::ZODIAC_LEX_ERROR) {
                        lex_err = true;
                    }

                    if (err.kind == Zodiac_Error_Kind::ZODIAC_PARSE_ERROR && lex_err) {
                        continue;
                    }

                    auto start = err.source_range.start;
                    fprintf(stderr, "%s:%llu:%llu: error: %s\n", start.name.data, start.line, start.index_in_line, err.message.data);
                }
            }

            ctx->parse_error = true;
            return false;
        }

        if (ftp->kind == File_To_Parse_Kind::PATH) {
             free(&dynamic_allocator, src_.data);
        }

        assert(file);
        resolver_add_file(ctx->resolver, file);
    }

    ctx->files_to_parse.count = 0;

    return true;
}

bool do_run_job(Zodiac_Context *ctx, Flat_Root_Node *root_node)
{
    assert(root_node->root.kind == Flat_Node_Kind::RUN ||
           root_node->root.kind == Flat_Node_Kind::DECL);

    AST_Directive *directive = nullptr;
    bool from_expr = false;

    if (root_node->root.kind == Flat_Node_Kind::DECL) {
        auto decl = root_node->root.decl;

        assert(decl->kind == AST_Declaration_Kind::RUN_DIRECTIVE);
        assert(decl->directive->kind == AST_Directive_Kind::RUN);
        directive = decl->directive;

    } else {
        assert(root_node->root.kind == Flat_Node_Kind::RUN);
        directive = root_node->root.run.expr->directive.directive;
        from_expr = true;
    }

    assert(directive);

    Bytecode_Function_Handle wrapper_handle;
    bool found = hash_table_find(&ctx->bytecode_converter->run_directives, directive, &wrapper_handle);
    assert(found);


    if (!check_run_dependencies(ctx, directive)) {
        return false;
    }

    File_Handle std_out_handle;
    if (ctx->interp_stdout_file) {
        std_out_handle = *ctx->interp_stdout_file;
    } else {
        filesystem_stdout_file(&std_out_handle);
    }
    auto run_res = execute_run_wrapper(ctx->bytecode_converter, wrapper_handle, std_out_handle);

    if (from_expr) {
        auto expr = root_node->root.run.expr;
        assert(run_res.value.type == expr->resolved_type);

        Scope *scope = directive->run.scope;
        Source_Range range = expr->sr;
        AST_Expression *new_expr = interpreter_register_to_ast_expression(ctx->bytecode_converter, run_res.value, scope, range);

        assert(new_expr->resolved_type);
        assert(EXPR_IS_CONST(new_expr));

        Bytecode_Register value_reg = ast_expr_to_bytecode(ctx->bytecode_converter, new_expr);
        expr->directive.generated_expression = new_expr;

        hash_table_add(&ctx->bytecode_converter->run_results, directive, value_reg);
    }

    return true;
}

bool check_run_dependencies(Zodiac_Context *ctx, AST_Directive *directive)
{
    assert(directive->kind == AST_Directive_Kind::RUN);

    if (directive->run.kind == AST_Run_Directive_Kind::EXPR) {

        return check_run_dependencies(ctx, directive->run.expr);

    } else if (directive->run.kind == AST_Run_Directive_Kind::STMT) {

        return check_run_dependencies(ctx, directive->run.stmt);

    } else {
        assert(false);
    }
    return true;
}

bool check_run_dependencies(Zodiac_Context *ctx, AST_Expression *expr)
{
    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::REAL_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL:
        case AST_Expression_Kind::CHAR_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL:
        case AST_Expression_Kind::TYPE_INFO:
        case AST_Expression_Kind::TYPE:
        case AST_Expression_Kind::IDENTIFIER: {
            return true;
        }

        case AST_Expression_Kind::MEMBER: {
            return check_run_dependencies(ctx, expr->member.base);
        }

        case AST_Expression_Kind::INDEX: {
            return check_run_dependencies(ctx, expr->index.base);
        }

        case AST_Expression_Kind::CALL: {

            if (!check_run_dependencies(ctx, expr->call.base)) {
                return false;
            }

            AST_Expression *base = expr->call.base;

            if (base->kind == AST_Expression_Kind::IDENTIFIER) {

                Symbol *sym = scope_get_symbol(base->identifier.scope, base->identifier.name);
                assert(sym);

                if (sym->kind == Symbol_Kind::FUNC) {
                    assert(sym->decl && sym->decl->kind == AST_Declaration_Kind::FUNCTION);

                    auto fn_decl = sym->decl;

                    if (fn_decl->flags & AST_DECL_FLAG_FOREIGN) {
                        return true;
                    }

                    if (fn_decl->flags & AST_DECL_FLAG_BYTECODE_EMITTED) {
                        // Check this function as well.
                        return check_run_dependencies(ctx, fn_decl);
                    } else {
                        resolve_error(ctx, expr, "Waiting for '%s' to emitted before executing run job '%s'", fn_decl->identifier.name.data, fn_decl->identifier.name.data);
                        return false;
                    }
                } else {

                    // Calling function pointer
                    assert(sym->kind == Symbol_Kind::VAR ||
                           sym->kind == Symbol_Kind::CONST ||
                           sym->kind == Symbol_Kind::PARAM);
                    assert(sym->decl->variable.resolved_type->kind == Type_Kind::FUNCTION);
                    return true;
                }

            } else {

                assert(!EXPR_IS_CONST(base)); // These can be checked?

                // Calling function pointer
                assert(base->resolved_type->kind == Type_Kind::FUNCTION);
                return true;
            }

            assert(false);
            break;
        }

        case AST_Expression_Kind::UNARY: {
            return check_run_dependencies(ctx, expr->unary.operand);
        }

        case AST_Expression_Kind::BINARY: {
            if (!check_run_dependencies(ctx, expr->binary.lhs)) {
                return false;
            }

            if (!check_run_dependencies(ctx, expr->binary.rhs)) {
                return false;
            }

            return true;
        }

        case AST_Expression_Kind::RANGE: assert(false); break;

        case AST_Expression_Kind::CAST: {
            return check_run_dependencies(ctx, expr->cast.value);
        }

        case AST_Expression_Kind::RUN_DIRECTIVE: {
            if (expr->directive.generated_expression) {
                return true;
            }

            return false;
        }

        case AST_Expression_Kind::COMPOUND: {

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {
                if (!check_run_dependencies(ctx, expr->compound.expressions[i])) {
                    return false;
                }
            }

            return true;
        }
    }
}

bool check_run_dependencies(Zodiac_Context *ctx, AST_Declaration *decl)
{
    if (decl->flags & AST_DECL_FLAG_BYTECODE_DEPS_EMITTED) {
        // Recursive call, or checked before
        return true;
    }

    bool result = true;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (decl->variable.value) {
                result = check_run_dependencies(ctx, decl->variable.value);
            }

            break;
        }

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            return true;
        }

        case AST_Declaration_Kind::PARAMETER: assert(false); break;
        case AST_Declaration_Kind::FIELD: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {
            // We already know bytecode for this function has been emitted

            // Set this before the body to make recursive calls return true
            decl->flags |= AST_DECL_FLAG_BYTECODE_DEPS_EMITTED;

            bool fn_result = true;

            for (s64 i = 0; i < decl->function.body.count; i++) {
                auto stmt = decl->function.body[i];

                if (!check_run_dependencies(ctx, stmt)) {
                    fn_result = false;
                    break;
                }
            }

            if (!fn_result) {
                decl->flags &= ~AST_DECL_FLAG_BYTECODE_DEPS_EMITTED;
            }

            result = fn_result;
            break;
        }

        case AST_Declaration_Kind::STRUCT: assert(false); break;
        case AST_Declaration_Kind::UNION: assert(false); break;
        case AST_Declaration_Kind::ENUM_MEMBER: assert(false); break;
        case AST_Declaration_Kind::ENUM: assert(false); break;
        case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Declaration_Kind::IMPORT_DIRECTIVE: assert(false); break;
    }

    if (result) {
        decl->flags |= AST_DECL_FLAG_BYTECODE_DEPS_EMITTED;
    }

    return result;
}

bool check_run_dependencies(Zodiac_Context *ctx, AST_Statement *stmt)
{
    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::BLOCK: {
            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                if (!check_run_dependencies(ctx, stmt->block.statements[i])) {
                    return false;
                }
            }

            return true;
        }

        case AST_Statement_Kind::DECLARATION: {
            return check_run_dependencies(ctx, stmt->decl.decl);
        }

        case AST_Statement_Kind::ASSIGN: {
            if (!check_run_dependencies(ctx, stmt->assign.dest)) {
                return false;
            }

            if (!check_run_dependencies(ctx, stmt->assign.value)) {
                return false;
            }

            return true;
        }

        case AST_Statement_Kind::CALL: {
            return check_run_dependencies(ctx, stmt->call.call);
        }

        case AST_Statement_Kind::IF: {
            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto &block = stmt->if_stmt.blocks[i];

                if (!check_run_dependencies(ctx, block.cond)) {
                    return false;
                }

                if (!check_run_dependencies(ctx, block.then)) {
                    return false;
                }
            }

            if (stmt->if_stmt.else_stmt && !check_run_dependencies(ctx, stmt->if_stmt.else_stmt)) {
                return false;
            }

            return true;
        }

        case AST_Statement_Kind::WHILE: assert(false); break;

        case AST_Statement_Kind::FOR: {
            if (!check_run_dependencies(ctx, stmt->for_stmt.init_decl)) {
                return false;
            }

            if (!check_run_dependencies(ctx, stmt->for_stmt.cond_expr)) {
                return false;
            }

            if (!check_run_dependencies(ctx, stmt->for_stmt.inc_stmt)) {
                return false;
            }

            if (!check_run_dependencies(ctx, stmt->for_stmt.body_stmt)) {
                return false;
            }

            return true;
        }

        case AST_Statement_Kind::SWITCH: {
            if (!check_run_dependencies(ctx, stmt->switch_stmt.value)) {
                return false;
            }

            for (s64 i = 0; i < stmt->switch_stmt.cases.count; i++) {
                if (!check_run_dependencies(ctx, stmt->switch_stmt.cases[i])) {
                    return false;
                }
            }

            return true;
        }

        case AST_Statement_Kind::SWITCH_CASE: {
            if (!stmt->switch_case_stmt.is_default) {
                for (s64 i = 0; i < stmt->switch_case_stmt.case_values.count; i++) {
                    if (!check_run_dependencies(ctx, stmt->switch_case_stmt.case_values[i])) {
                        return false;
                    }
                }
            }

            if (!check_run_dependencies(ctx, stmt->switch_case_stmt.case_stmt)) {
                return false;
            }

            return true;
        }

        case AST_Statement_Kind::FALLTROUGH: assert(false); break;
        case AST_Statement_Kind::DEFER: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_stmt.value && !check_run_dependencies(ctx, stmt->return_stmt.value)) {
                return false;
            }

            return true;
        }

        case AST_Statement_Kind::PRINT: {
            for (s64 i = 0; i < stmt->print_expr.expressions.count; i++) {
                if (!check_run_dependencies(ctx, stmt->print_expr.expressions[i])) {
                    return false;
                }
            }

            return true;
        }
    }
}

}
