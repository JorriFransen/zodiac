#include "ast.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "command_line_arguments.h"
#include "common.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "type.h"
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
    defer { free(&dynamic_allocator, stream.data); };

    lexer_init_stream(&lexer, stream, opts->input_file_name);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);

    if (parser.error) return 1;
    assert(file);


    Resolver resolver;
    resolver_create(&resolver, &c);
    defer { resolver_destroy(&resolver); };

    resolve_file(&resolver, file);

    if (resolver_report_errors(&resolver)) {
        return 42;
    }

    free(&dynamic_allocator, stream.data);

    if (opts->print_ast) ast_print_file(file);

    // Assume this stage won't cause any errors atm
    // This bytecode builder should maybe be part of the context?
    Bytecode_Builder bb = bytecode_builder_create(&c.bytecode_allocator, &c);

    Bytecode_Converter bc = bytecode_converter_create(&c.bytecode_allocator, &c, &bb);

    emit_bytecode(&resolver, &bc);
    assert(c.errors.count == 0);

    if (opts->print_bytecode) bytecode_print(&bb, temp_allocator_allocator());

    Bytecode_Validator validator = {};
    bytecode_validator_init(&c, temp_allocator_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    if (!bytecode_valid) {
        assert(validator.errors.count);

        bytecode_validator_print_errors(&validator);
        return 1;
    }

    Interpreter interp = interpreter_create(c_allocator(), &c);
    auto program = bytecode_get_program(bc.builder);

    assert(program.entry_handle == -1);
    program.entry_handle= bytecode_find_entry(program);

    Interpreter_Register result_reg = interpreter_start(&interp, program);
    if (result_reg.type->kind == Type_Kind::INTEGER) {
        ZTRACE("Entry point returned: %lli", result_reg.value.integer.s64);
    } else {
        assert_msg(false, "Unexpected return type from entry point")
    }

    if (!opts->dont_emit_binary) {
        LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &bb);

        llvm_builder_emit_program(&llvm_builder, &program);

        if (opts->print_llvm_ir) llvm_builder_print(&llvm_builder);

        llvm_builder_emit_binary(&llvm_builder);

        llvm_builder_free(&llvm_builder);
    }

    return 0;
}

