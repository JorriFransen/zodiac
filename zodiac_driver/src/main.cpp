#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/printer.h"
#include "bytecode/validator.h"
#include "common.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
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
#include "zodiac_context.h"

#include <stdio.h>

namespace Zodiac {
    struct AST_File;
}

using namespace Zodiac;
using namespace Bytecode;

int main() {
    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    Lexer lexer;
    lexer_create(&c, &lexer);
    String stream = {};

    auto filename = "tests/simple.zc";
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, filename, &stream);
    assert(read_result);
    if (!read_result) {
        return 1;
    }

    lexer_init_stream(&lexer, stream, filename);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    AST_File *file = parse_file(&parser);
    if (parser.error) return 1;
    assert(file);

    Scope *global_scope = scope_new(&dynamic_allocator, Scope_Kind::GLOBAL, nullptr);

    Resolver resolver;
    resolver_create(&resolver, &c, global_scope);

    resolve_file(&resolver, file);

    for (s64 i = 0; i < c.errors.count; i++) {

        auto err = c.errors[i];

        if (!c.fatal_resolve_error) assert(!err.fatal);

        bool print = c.fatal_resolve_error == err.fatal;

        if (print) {
            auto start = err.source_range.start;
            printf("%s:%llu:%llu: error: %s\n", start.name.data, start.line, start.index_in_line, err.message.data);
        }
    }

    free(&dynamic_allocator, stream.data);

    // Assume this stage won't cause any errors atm
    // This bytecode builder should maybe be part of the context?
    Bytecode_Builder bb = bytecode_builder_create(&c.bytecode_allocator, &c);

    Bytecode_Converter bc = bytecode_converter_create(&c.bytecode_allocator, &c, &bb);

    emit_bytecode(&resolver, &bc);

    bytecode_print(&bb, temp_allocator_allocator());

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
        printf("Entry point returned: %lli\n", result_reg.value.integer.s64);
    } else {
        assert_msg(false, "Unexpected return type from entry point")
    }

    LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &bb);

    for (s64 i = 0; i < program.globals.count; i++) {
        llvm_builder_emit_global(&llvm_builder, i);
    }

    for (s64 i = 0; i < program.functions.count; i++) {
        llvm_builder_register_function(&llvm_builder, i);
    }

    for (s64 i = 0; i < program.functions.count; i++) {
        bool result = llvm_builder_emit_function(&llvm_builder, i);
        assert(result);
    }

    llvm_builder_print(&llvm_builder);

    llvm_builder_emit_binary(&llvm_builder);

    llvm_builder_free(&llvm_builder);

    return 0;
}

