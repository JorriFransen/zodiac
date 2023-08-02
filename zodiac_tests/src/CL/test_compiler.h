#pragma once

#include <munit/munit.h>

#include "test_common.h"

#include "lexer.h"
#include "parser.h"
#include "resolve.h"
#include "scope.h"
#include "zodiac_context.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/validator.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "type.h"

namespace Zodiac { namespace Compiler_Tests {

using namespace Bytecode;

static MunitResult compile_and_run(String_Ref code_str, s64 exit_code) {

    auto ta = temp_allocator_allocator();

    Zodiac_Context c;
    zodiac_context_create(&c);
    defer { zodiac_context_destroy(&c); };

    c.options.verbose = true;

    Lexer lexer;
    lexer_create(&c, &lexer);
    defer { lexer_destroy(&lexer); };

    lexer_init_stream(&lexer, code_str, "<test_code>");

    Parser parser;
    parser_create(&c, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);
    munit_assert_ptr_not_null(file);
    munit_assert_false(parser.error);

    Resolver resolver;
    resolver_create(&resolver, &c);
    defer { resolver_destroy(&resolver); };

    resolve_file(&resolver, file);

    munit_assert_false(resolver_report_errors(&resolver));

    Bytecode_Builder bb = Bytecode::bytecode_builder_create(&c.bytecode_allocator, &c);
    Bytecode_Converter bc = bytecode_converter_create(&c.bytecode_allocator, &c, &bb);

    emit_bytecode(&resolver, &bc);
    munit_assert(c.errors.count == 0);

    // bytecode_print(&bb, temp_allocator_allocator());

    Bytecode_Validator validator;
    bytecode_validator_init(&c, ta, &validator, bb.functions, nullptr);

    bool bytecode_valid = validate_bytecode(&validator);
    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        return MUNIT_FAIL;
    }

    Interpreter interp = interpreter_create(c_allocator(), &c);
    auto program = bytecode_get_program(bc.builder);

    munit_assert(program.entry_handle == -1);
    program.entry_handle= bytecode_find_entry(program);

    Interpreter_Register result_reg = interpreter_start(&interp, program);
    munit_assert(result_reg.type->kind == Type_Kind::INTEGER);

    munit_assert_int64(result_reg.value.integer.s64, ==, exit_code);

    LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &bb);
    defer { llvm_builder_free(&llvm_builder); };

    llvm_builder_emit_program(&llvm_builder, &program);

    auto out_file_name = c.options.output_file_name;
    llvm_builder_emit_binary(&llvm_builder);
    defer { filesystem_remove(out_file_name); };

    munit_assert(filesystem_exists(c.options.output_file_name));

    Array_Ref<String_Ref> args({string_append(ta, Array_Ref<String_Ref>({ filesystem_cwd(ta), "/", out_file_name } ))});
    Process_Result pr = platform_execute_process(&args);
    defer { platform_free_process_result(&pr); };

    bool expected_result = true;
    if (exit_code != 0) expected_result = false;

    munit_assert_int64(pr.exit_code, ==, exit_code);
    munit_assert(pr.success == expected_result);

    return MUNIT_OK;
}

static MunitResult Return_0(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 0;
        }
    )CODE_STR";

    return compile_and_run(code_string, 0);
}

static MunitResult Return_1(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 1;
        }
    )CODE_STR";

    return compile_and_run(code_string, 1);
}

static MunitResult Infer_Void_Return(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 { return 0; }

        infer_void_return1 :: () {
            return;
        }

        infer_void_return2 :: () { }

        // void_return_ts :: () -> void { }
    )CODE_STR";

    return compile_and_run(code_string, 0);
    return MUNIT_OK;
}

START_TESTS(compiler_tests)
    DEFINE_TEST(Return_0),
    DEFINE_TEST(Return_1),
    DEFINE_TEST(Infer_Void_Return),
END_TESTS()

}}
