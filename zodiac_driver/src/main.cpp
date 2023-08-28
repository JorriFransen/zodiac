#include "ast.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "command_line_arguments.h"
#include "containers/dynamic_array.h"
#include "containers/hash_table.h"
#include "defines.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

using namespace Zodiac;
using namespace Bytecode;

int main(int argc, const char **argv) {

    if (!logging_system_initialize()) return 1;
    if (!memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);
    defer { zodiac_context_destroy(&c); };

    auto opts = &c.options;
    parse_command_line_options(opts, argc, argv);

    Lexer lexer;
    lexer_create(&c, &lexer);
    defer { lexer_destroy(&lexer); };

    String stream = {};
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, opts->input_file_name, &stream);
    assert(read_result);
    if (!read_result) {
        return 1;
    }

    lexer_init_stream(&lexer, stream, opts->input_file_name);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);

    if (parser.error) return 1;
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

            assert(root_node->root.kind == Flat_Node_Kind::DECL);
            auto decl = root_node->root.decl;

            assert(decl->kind == AST_Declaration_Kind::RUN_DIRECTIVE);
            assert(decl->directive->kind == AST_Directive_Kind::RUN);

            Bytecode_Function_Handle wrapper_handle;
            bool found = hash_table_find(&bc.run_directives, decl->directive, &wrapper_handle);
            assert(found);

            auto run_res = execute_run_wrapper(&bc, wrapper_handle);
            free_run_wrapper_result(&run_res);
        }

        // For now assume runs never fail...
        resolver.nodes_to_run_bytecode.count = 0;
    }

    if (opts->print_ast) ast_print_file(file);

    if (opts->print_bytecode) bytecode_print(&bb, temp_allocator_allocator());

    Bytecode_Validator validator = {};
    bytecode_validator_init(&c, temp_allocator_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    if (!bytecode_valid) {
        assert(validator.errors.count);

        bytecode_validator_print_errors(&validator);
        return 1;
    }

    if (!opts->dont_emit_binary) {

        auto program = bytecode_get_program(bc.builder);
        LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &bb);

        llvm_builder_emit_program(&llvm_builder, &program);

        if (opts->print_llvm_ir) llvm_builder_print(&llvm_builder);

        llvm_builder_emit_binary(&llvm_builder);

        llvm_builder_free(&llvm_builder);
    }

    return 0;
}

