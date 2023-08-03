#pragma once

#include <munit/munit.h>

#include "test_common.h"

#include "lexer.h"
#include "parser.h"
#include "resolve.h"
#include "zodiac_context.h"
#include "bytecode/bytecode.h"
#include "bytecode/converter.h"
#include "bytecode/validator.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "type.h"

namespace Zodiac { namespace Compiler_Tests {

using namespace Bytecode;

#define RESOLVE_ERR(f, m) (Expected_Error { .kind = ZODIAC_RESOLVE_ERROR, .fatal = (f), .message = (m)})

struct Expected_Error
{
    Zodiac_Error_Kind kind;
    bool fatal;
    String_Ref message;
};

struct Expected_Results
{
    s64 exit_code = 0;

    Array_Ref<Expected_Error> resolve_errors = {};
};

struct Compile_Run_Results
{
    MunitResult result;
    Bytecode_Program program;

    Bytecode_Builder builder;
    Zodiac_Context context;
};

static void free_compile_run_results(Compile_Run_Results *r)
{
    bytecode_builder_free(&r->builder);
    zodiac_context_destroy(&r->context);
}

static Compile_Run_Results compile_and_run(String_Ref code_str, Expected_Results expected_results) {

    Compile_Run_Results result = { .result = MUNIT_OK };

    auto ta = temp_allocator_allocator();

    zodiac_context_create(&result.context);

    result.context.options.verbose = true;

    Lexer lexer;
    lexer_create(&result.context, &lexer);
    defer { lexer_destroy(&lexer); };

    lexer_init_stream(&lexer, code_str, "<test_code>");

    Parser parser;
    parser_create(&result.context, &lexer, &parser);
    defer { parser_destroy(&parser); };

    AST_File *file = parse_file(&parser);
    munit_assert_ptr_not_null(file);
    munit_assert_false(parser.error);

    Resolver resolver;
    resolver_create(&resolver, &result.context);
    defer { resolver_destroy(&resolver); };

    resolve_file(&resolver, file);

    munit_assert_int64(result.context.errors.count, ==, expected_results.resolve_errors.count);
    if (expected_results.resolve_errors.count) {

        for (s64 i = 0; i < expected_results.resolve_errors.count; i++) {
            auto expected_err = &expected_results.resolve_errors[i];
            auto actual_err = &result.context.errors[i];

            munit_assert(expected_err->kind == actual_err->kind);
            munit_assert(expected_err->fatal == actual_err->fatal);
            munit_assert_string_equal(expected_err->message.data, actual_err->message.data);
        }

        return result;
    }

    result.builder = bytecode_builder_create(&result.context.bytecode_allocator, &result.context);
    Bytecode_Converter bc = bytecode_converter_create(&result.context.bytecode_allocator, &result.context, &result.builder);
    defer { bytecode_converter_destroy(&bc); };

    emit_bytecode(&resolver, &bc);
    munit_assert(result.context.errors.count == 0);

    // bytecode_print(&result.builder, temp_allocator_allocator());

    Bytecode_Validator validator;
    bytecode_validator_init(&result.context, ta, &validator, result.builder.functions, nullptr);
    defer { bytecode_validator_free(&validator); };

    bool bytecode_valid = validate_bytecode(&validator);
    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result.result = MUNIT_FAIL;
        return result;
    }

    result.program = bytecode_get_program(bc.builder);

    Interpreter interp = interpreter_create(c_allocator(), &result.context);
    defer { interpreter_free(&interp); };

    munit_assert(result.program.entry_handle == -1);
    result.program.entry_handle= bytecode_find_entry(result.program);

    Interpreter_Register result_reg = interpreter_start(&interp, result.program);
    munit_assert(result_reg.type->kind == Type_Kind::INTEGER);

    munit_assert_int64(result_reg.value.integer.s64, ==, expected_results.exit_code);

    LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &result.builder);
    defer { llvm_builder_free(&llvm_builder); };

    llvm_builder_emit_program(&llvm_builder, &result.program);

    auto out_file_name = result.context.options.output_file_name;
    llvm_builder_emit_binary(&llvm_builder);
    defer { filesystem_remove(out_file_name); };

    munit_assert(filesystem_exists(result.context.options.output_file_name));

    Array_Ref<String_Ref> args({string_append(ta, Array_Ref<String_Ref>({ filesystem_cwd(ta), "/", out_file_name } ))});
    Process_Result pr = platform_execute_process(&args);
    defer { platform_free_process_result(&pr); };

    bool expected_result = true;
    if (expected_results.exit_code != 0) expected_result = false;

    munit_assert_int64(pr.exit_code, ==, expected_results.exit_code);
    munit_assert(pr.success == expected_result);

    return result;
}

static MunitResult Return_0(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 0;
        }
    )CODE_STR";

    Expected_Results expected = {};
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };
    return result.result;
}

static MunitResult Return_1(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 1;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 1 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };
    return result.result;
}

static MunitResult Infer_Void_Return(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 { return 0; }

        infer_void_return1 :: () {
            return;
        }

        infer_void_return2 :: () { }

        void_return_ts :: () -> void { }
    )CODE_STR";

    Expected_Results expected = {};
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    for (u64 i = 0; i < result.program.functions.count; i++) {

        auto fn = &result.program.functions[i];

        if (fn->name != "main") {
            auto fn_type = fn->type;
            munit_assert(fn_type->kind == Type_Kind::FUNCTION);

            auto return_type = fn_type->function.return_type;
            munit_assert(return_type->kind == Type_Kind::VOID);

            munit_assert_ptr_equal(return_type, &builtin_type_void);
        }
    }

    return result.result;
}

static MunitResult Invalid_Return_Type(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 { return 0; }

        mismatching_return :: () -> void { return 1; }
    )CODE_STR";

    Expected_Results expected = {
        .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Could not convert integer literal to inferred type 'void'")})
    };

    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

#undef RESOLVE_ERR

START_TESTS(compiler_tests)
    DEFINE_TEST(Return_0),
    DEFINE_TEST(Return_1),
    DEFINE_TEST(Infer_Void_Return),
    DEFINE_TEST(Invalid_Return_Type),
END_TESTS()

}}
