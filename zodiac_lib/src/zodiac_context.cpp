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

namespace Zodiac
{

struct Scope;

using namespace Bytecode;

void zodiac_context_create(Zodiac_Options options, Zodiac_Context *out_context)
{
    assert(out_context);

    *out_context = {};

    out_context->options = options;
    if (options.verbose) {
        logging_system_set_max_level(Log_Level::TRACE);
    }

    atom_table_init(&out_context->atoms);

    if (!type_system_initialized) {
        bool result = type_system_initialize(out_context);
        assert(result);
    }

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->ast_allocator_state);
    out_context->ast_allocator = linear_allocator_allocator(&out_context->ast_allocator_state);

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->bytecode_allocator_state);
    out_context->bytecode_allocator = linear_allocator_allocator(&out_context->bytecode_allocator_state);

    temporary_allocator_create(KIBIBYTE(64), nullptr, &out_context->temp_allocator_state);
    out_context->temp_allocator = temporary_allocator_allocator(&out_context->temp_allocator_state);

    temporary_allocator_create(KIBIBYTE(8), nullptr, &out_context->error_allocator_state);
    out_context->error_allocator = temporary_allocator_allocator(&out_context->error_allocator_state);

    dynamic_array_create(c_allocator(), &out_context->errors);
    out_context->fatal_resolve_error = false;

    out_context->resolver = alloc<Resolver>(&dynamic_allocator);
    resolver_create(out_context->resolver, out_context);

    out_context->bytecode_builder = alloc<Bytecode_Builder>(&dynamic_allocator);
    bytecode_builder_init(&out_context->bytecode_allocator, out_context, out_context->bytecode_builder);

    out_context->bytecode_converter = alloc<Bytecode_Converter>(&dynamic_allocator);
    bytecode_converter_init(&out_context->bytecode_allocator, out_context, out_context->bytecode_builder, out_context->bytecode_converter);

    out_context->interp_stdout_file = nullptr;

    zodiac_register_keywords(&out_context->atoms);

    out_context->compiler_exe_path = filesystem_exe_path(c_allocator());
    assert(filesystem_exists(out_context->compiler_exe_path));

    out_context->compiler_exe_dir = filesystem_dir_name(c_allocator(), out_context->compiler_exe_path);
    assert(filesystem_exists(out_context->compiler_exe_dir));

#ifdef ZPLATFORM_LINUX
    auto dynamic_support_lib_name = "/libzrs.so";
    auto static_support_lib_name = "/libzrs_s.a";

#elif ZPLATFORM_WINDOWS
    auto dynamic_support_lib_name = "\\libzrs.lib";
    auto dynamic_support_dll_name = "\\libzrs.dll";
    auto static_support_lib_name = "\\libzrs_s.lib";

    out_context->support_dll_dynamic_path = string_append(c_allocator(), out_context->compiler_exe_dir, dynamic_support_dll_name);
    assert(filesystem_exists(out_context->support_dll_dynamic_path));

#endif

    out_context->support_lib_dynamic_path = string_append(c_allocator(), out_context->compiler_exe_dir, dynamic_support_lib_name);
    assert(filesystem_exists(out_context->support_lib_dynamic_path));

    out_context->support_lib_static_path = string_append(c_allocator(), out_context->compiler_exe_dir, static_support_lib_name);
    assert(filesystem_exists(out_context->support_lib_static_path));
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    // TODO: This should not be global state, but part of the context...
    type_system_initialized = false;

    atom_table_free(&context->atoms);

    auto ca = c_allocator();

    if (context->options.input_file_name.length) free(ca, context->options.input_file_name.data);
    if (context->options.output_file_name.length) free(ca, context->options.output_file_name.data);

    dynamic_array_free(&context->errors);

    resolver_destroy(context->resolver);
    free(&dynamic_allocator, context->resolver);

    bytecode_builder_free(context->bytecode_builder);
    free(&dynamic_allocator, context->bytecode_builder);

    bytecode_converter_destroy(context->bytecode_converter);
    free(&dynamic_allocator, context->bytecode_converter);

    free(ca, context->compiler_exe_path.data);
    free(ca, context->compiler_exe_dir.data);

    linear_allocator_destroy(&context->ast_allocator_state);
    linear_allocator_destroy(&context->bytecode_allocator_state);
    linear_allocator_destroy(&context->temp_allocator_state.linear_allocator);
}

bool zodiac_context_compile(Zodiac_Context *ctx, String_Ref source, String_Ref source_name)
{
    debug_assert(ctx);

    Lexer lexer;
    lexer_create(ctx, &lexer);
    defer { lexer_destroy(&lexer); };

    lexer_init_stream(&lexer, source, source_name);

    Parser parser;
    parser_create(ctx, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);

    if (parser.error) {
        bool lex_err = false;
        if (!ctx->options.report_errors) {
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

        return false;
    }

    assert(file);

    resolver_add_file(ctx->resolver, file);

    bool resolver_done = false;
    while (!resolver_done) {
        resolver_done = resolver_cycle(ctx->resolver);

        if (ctx->options.report_errors) {
            if (resolver_report_errors(ctx->resolver)) {
                return false;
            }
        } else if (ctx->fatal_resolve_error) return false;

        assert(ctx->errors.count == 0);
        emit_bytecode(ctx->resolver, ctx->bytecode_converter);
        assert(ctx->errors.count == 0);

        for (s64 i = 0; i < ctx->resolver->nodes_to_run_bytecode.count; i++) {
            Flat_Root_Node *root_node = ctx->resolver->nodes_to_run_bytecode[i];

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
                Source_Range range = expr->range;
                AST_Expression *new_expr = interpreter_register_to_ast_expression(ctx->bytecode_converter, run_res.value, scope, range);

                assert(new_expr->resolved_type);
                assert(EXPR_IS_CONST(new_expr));

                Bytecode_Register value_reg = ast_expr_to_bytecode(ctx->bytecode_converter, new_expr);
                expr->directive.generated_expression = new_expr;

                hash_table_add(&ctx->bytecode_converter->run_results, directive, value_reg);
            }

            free_run_wrapper_result(&run_res);
        }

        // For now assume runs never fail...
        ctx->resolver->nodes_to_run_bytecode.count = 0;
    }

    if (ctx->options.print_ast) ast_print_file(file);

    if (ctx->options.print_bytecode) bytecode_print(ctx->bytecode_builder, temp_allocator_allocator());

    Bytecode_Validator validator = {};
    bytecode_validator_init(ctx, temp_allocator_allocator(), &validator, ctx->bytecode_builder->functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    if (!bytecode_valid) {
        assert(validator.errors.count);

        bytecode_validator_print_errors(&validator);
        return false;
    }

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

bool zodiac_context_compile(Zodiac_Context *ctx, String_Ref source_file_name)
{
    debug_assert(ctx);

    if (!filesystem_is_regular(source_file_name)) {
        ZFATAL("Invalid path '%.*s' (input_file_name)", (int)source_file_name.length, source_file_name.data);
    }

    String file_content = {};
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, source_file_name, &file_content);
    assert(read_result);

    bool result = zodiac_context_compile(ctx, file_content, source_file_name);

    free(&dynamic_allocator, file_content.data);

    return result;
}

bool zodiac_context_compile(Zodiac_Context *ctx)
{
    return zodiac_context_compile(ctx, ctx->options.input_file_name);
}

}
