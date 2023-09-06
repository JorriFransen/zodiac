#include "ast.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "command_line_arguments.h"
#include "containers/dynamic_array.h"
#include "containers/hash_table.h"
#include "defines.h"
#include "error.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "platform/platform.h"
#include "resolve.h"
#include "source_pos.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

#include <stdio.h>

using namespace Zodiac;
using namespace Bytecode;

namespace Zodiac {
    struct Scope;
}

int main(int argc, const char **argv) {

    if (!logging_system_initialize()) return 1;
    if (!memory_system_initialize()) return 1;

    Zodiac_Options options = {};
    parse_command_line_options(&options, argc, argv);

    Zodiac_Context c;
    zodiac_context_create(options, &c);
    defer { zodiac_context_destroy(&c); };


    Lexer lexer;
    lexer_create(&c, &lexer);
    defer { lexer_destroy(&lexer); };

    String stream = {};
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, options.input_file_name, &stream);
    assert(read_result);
    if (!read_result) {
        return 1;
    }

    lexer_init_stream(&lexer, stream, options.input_file_name);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);

    if (parser.error) {
        bool lex_err = false;
        for (s64 i = 0; i < c.errors.count; i++) {
            auto &err = c.errors[i];
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

        platform_exit(1);
    }

    assert(file);

    free(&dynamic_allocator, stream.data);

    Resolver resolver;
    resolver_create(&resolver, &c);
    defer { resolver_destroy(&resolver); };

    resolver_add_file(&resolver, file);

    free(&dynamic_allocator, stream.data);

    // This bytecode builder should maybe be part of the context?
    Bytecode_Builder bb = bytecode_builder_create(&c.bytecode_allocator, &c);
    defer { bytecode_builder_free(&bb); };

    Bytecode_Converter bc = bytecode_converter_create(&c.bytecode_allocator, &c, &bb);
    defer { bytecode_converter_destroy(&bc); };

    bool resolver_done = false;
    while (!resolver_done) {
        resolver_done = resolver_cycle(&resolver);

        if (resolver_report_errors(&resolver)) {
            return 42;
        }

        assert(c.errors.count == 0);
        emit_bytecode(&resolver, &bc);
        assert(c.errors.count == 0);

        for (s64 i = 0; i < resolver.nodes_to_run_bytecode.count; i++) {
            Flat_Root_Node *root_node = resolver.nodes_to_run_bytecode[i];

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
            bool found = hash_table_find(&bc.run_directives, directive, &wrapper_handle);
            assert(found);

            auto run_res = execute_run_wrapper(&bc, wrapper_handle);

            if (from_expr) {
                auto expr = root_node->root.run.expr;
                assert(run_res.value.type == expr->resolved_type);

                Scope *scope = directive->run.scope;
                Source_Range range = expr->range;
                AST_Expression *new_expr = interpreter_register_to_ast_expression(&bc, run_res.value, scope, range);

                assert(new_expr->resolved_type);
                assert(EXPR_IS_CONST(new_expr));

                Bytecode_Register value_reg = ast_expr_to_bytecode(&bc, new_expr);
                expr->directive.generated_expression = new_expr;

                hash_table_add(&bc.run_results, directive, value_reg);
            }

            free_run_wrapper_result(&run_res);
        }

        // For now assume runs never fail...
        resolver.nodes_to_run_bytecode.count = 0;
    }

    if (options.print_ast) ast_print_file(file);

    if (options.print_bytecode) bytecode_print(&bb, temp_allocator_allocator());

    Bytecode_Validator validator = {};
    bytecode_validator_init(&c, temp_allocator_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    if (!bytecode_valid) {
        assert(validator.errors.count);

        bytecode_validator_print_errors(&validator);
        return 1;
    }

    if (!options.dont_emit_binary) {

        auto program = bytecode_get_program(bc.builder);
        LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &bb);

        llvm_builder_emit_program(&llvm_builder, &program);

        if (options.print_llvm_ir) llvm_builder_print(&llvm_builder);

        llvm_builder_emit_binary(&llvm_builder);

        llvm_builder_free(&llvm_builder);
    }

    return 0;
}

