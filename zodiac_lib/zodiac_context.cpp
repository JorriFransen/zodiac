#include "zodiac_context.h"

#include "ast.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "containers/hash_table.h"
#include "lexer.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
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

struct Scope;

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

    out_context->builtin_string_type = nullptr;
    out_context->builtin_type_info_kind_type = nullptr;
    out_context->builtin_type_info_type = nullptr;
    out_context->builtin_type_info_int_type = nullptr;
    out_context->builtin_type_info_pointer_type = nullptr;
    out_context->builtin_type_info_struct_type = nullptr;
    out_context->builtin_type_info_struct_member_type = nullptr;

    dynamic_array_create(&dynamic_allocator, &out_context->type_infos);

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

        bool ran_bytecode_jobs = false;

        // TODO: FIXME:
        // This is a temporary fix, we need to check if all called functions are emitted before running,
        // right now we only check calls directly in the run directive.
        if (!(resolve_result & RESOLVE_RESULT_PROGRESS)) {

            for (s64 i = 0; i < ctx->resolver->nodes_to_run_bytecode.count; i++) {
                auto node = ctx->resolver->nodes_to_run_bytecode[i];

                if (do_run_job(ctx, node)) {
                    dynamic_array_remove_ordered(&ctx->resolver->nodes_to_run_bytecode, i);
                    i--;
                    ran_bytecode_jobs = true;
                }
            }
        }

        if (ran_bytecode_jobs) {
            ctx->errors.count = 0;
            temporary_allocator_reset(&ctx->error_allocator_state);
        }

        if (ctx->options.report_errors) {
            if (resolver_report_errors(ctx->resolver)) {
                return false;
            }
        } else if (ctx->errors.count) return false;

        assert(ctx->errors.count == 0);
        emit_bytecode(ctx->resolver, ctx->bytecode_converter);
        if (ctx->errors.count) return false;
    }

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

    auto wrapper_fn = &ctx->bytecode_converter->builder->functions[wrapper_handle];

    for (s64 i = 0; i < directive->run.called_functions.count; i++) {
        AST_Declaration *called_fn_decl = directive->run.called_functions[i];

        Bytecode_Function_Handle called_fn_handle;
        bool found = hash_table_find(&ctx->bytecode_converter->functions,  called_fn_decl, &called_fn_handle);
        assert(found);

        auto called_fn = ctx->bytecode_converter->builder->functions[called_fn_handle];

        // No block means not emitted yet
        if (called_fn.blocks.count == 0) {
            ZTRACE("Waiting for '%s' to be emitted before executing run job '%s'", called_fn.name.data, wrapper_fn->name.data);
            return false;
        }
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

    free_run_wrapper_result(&run_res);

    return true;
}

}
