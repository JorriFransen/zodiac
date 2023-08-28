#include "test_compiler.h"

#include <munit/munit.h>

#include "ast.h"
#include "atom.h"
#include "bytecode/converter.h"
#include "bytecode/interpreter.h"
#include "bytecode/llvm_builder.h"
#include "bytecode/validator.h"
#include "common.h"
#include "containers/hash_table.h"
#include "lexer.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "platform/platform.h"
#include "resolve.h"
#include "type.h"
#include "util/asserts.h"

namespace Zodiac { namespace Compiler_Tests {

#define RESOLVE_ERR(f, m) (Expected_Error { .kind = ZODIAC_RESOLVE_ERROR, .fatal = (f), .message = (m)})

void free_compile_run_results(Compile_Run_Results *r)
{
    bytecode_builder_free(&r->builder);
    zodiac_context_destroy(&r->context);
}

Compile_Run_Results compile_and_run(String_Ref code_str, Expected_Results expected_results, Zodiac_Options options/*={}*/) {

    if (expected_results.std_out.length) {
        munit_assert_int64(expected_results.compiletime_std_out.length, ==, 0);
        munit_assert_int64(expected_results.runtime_std_out.length, ==, 0);

        expected_results.compiletime_std_out = expected_results.std_out;
        expected_results.runtime_std_out = expected_results.std_out;
    }

    Compile_Run_Results result = { .result = MUNIT_OK };

    auto ta = temp_allocator_allocator();

    zodiac_context_create(&result.context);

    result.context.options = options;
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

    result.builder = bytecode_builder_create(&result.context.bytecode_allocator, &result.context);
    Bytecode_Converter bc = bytecode_converter_create(&result.context.bytecode_allocator, &result.context, &result.builder);
    defer { bytecode_converter_destroy(&bc); };

    File_Handle stdout_file = {};
    filesystem_temp_file(&stdout_file);
    defer { filesystem_close(&stdout_file); };

    resolver_add_file(&resolver, file);
    bool resolver_done = false;
    while (!resolver_done) {
        resolver_done = resolver_cycle(&resolver);

        if (result.context.errors.count) break;

        assert(result.context.errors.count == 0);
        emit_bytecode(&resolver, &bc);
        assert(result.context.errors.count == 0);

        for (s64 i = 0; i < resolver.nodes_to_run_bytecode.count; i++) {
            Flat_Root_Node *root_node = resolver.nodes_to_run_bytecode[i];

            assert(root_node->root.kind == Flat_Node_Kind::DECL);
            auto decl = root_node->root.decl;
            assert(decl->kind == AST_Declaration_Kind::RUN_DIRECTIVE);
            assert(decl->directive->kind == AST_Directive_Kind::RUN);

            Bytecode_Function_Handle wrapper_handle;
            bool found = hash_table_find(&bc.run_directives, decl->directive, &wrapper_handle);
            assert(found);

            auto run_res = execute_run_wrapper(&bc, wrapper_handle, stdout_file);
            free_run_wrapper_result(&run_res);
        }

        // For now assume runs never fail...
        resolver.nodes_to_run_bytecode.count = 0;
    }

    if (resolve_error_count(&result.context) != expected_results.resolve_errors.count) {
        resolver_report_errors(&resolver);
    }

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

    print_bytecode(&result.builder);

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

    interp.std_out = stdout_file;

    munit_assert(result.program.entry_handle == -1);
    result.program.entry_handle = bytecode_find_entry(result.program);

    Interpreter_Register result_reg = interpreter_start(&interp, result.program);
    munit_assert(result_reg.type->kind == Type_Kind::INTEGER);

    munit_assert_int64(result_reg.value.integer.s64, ==, expected_results.exit_code);

    assert_zodiac_stream(stdout_file, expected_results.compiletime_std_out);

    LLVM_Builder llvm_builder = llvm_builder_create(c_allocator(), &result.builder);
    defer { llvm_builder_free(&llvm_builder); };

    llvm_builder_emit_program(&llvm_builder, &result.program);

    // llvm_builder_print(&llvm_builder);

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

    String_Ref std_out_str("");
    if (expected_results.runtime_std_out.length) {
        std_out_str = string_append(ta, expected_results.runtime_std_out, "\n");
    }

    munit_assert_int64(pr.result_string.length, ==, std_out_str.length);

    if (pr.result_string.length) munit_assert_string_equal(pr.result_string.data, std_out_str.data);

    return result;
}

MunitResult Return_0(const MunitParameter params[], void* user_data_or_fixture) {

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

MunitResult Return_1(const MunitParameter params[], void* user_data_or_fixture) {

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

MunitResult Infer_Void_Return(const MunitParameter params[], void* user_data_or_fixture) {

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

MunitResult Invalid_Return_Type(const MunitParameter params[], void* user_data_or_fixture) {

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

MunitResult Print(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            print(42);
            return 0;
        }
    )CODE_STR";

    Expected_Results expected = { .std_out = "42" };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Add_Int_Const(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 40 + 2;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Sub_Int_Const(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 44 - 2;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Mul_Int_Const(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 21 * 2;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Div_Int_Const(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            return 84 / 2;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Add_Int(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            a := 40;
            b := 2;
            return a + b;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Sub_Int(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            a := 44;
            b := 2;
            return a - b;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Mul_Int(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            a := 21;
            b := 2;
            return a * b;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Binop_Div_Int(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            a := 84;
            b := 2;
            return a / b;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Global_Constant_With_Typespec(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_const : s64 : 42;
        main :: () -> s64 { return global_const; }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Global_Constant_Without_Typespec(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_const :: 42;
        main :: () -> s64 { return global_const; }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Modify_Global_Constant(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_const :: 42;
        main :: () -> s64 {
            global_const = 1;
            return 0;
        }
    )CODE_STR";

    Expected_Results expected = {
        .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Left side of assignment must be an lvalue")})
    };

    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Global_Variable_TS(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_var : s64 = 42;
        main :: () -> s64 {
            return global_var;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Global_Variable_No_TS(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_var := 42;
        main :: () -> s64 {
            return global_var;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Global_Variable_Assign(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_var := 40;
        main :: () -> s64 {
            global_var = global_var + 2;
            return global_var;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Local_Constant(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            local_const :: 42;
            return local_const;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Modify_Local_Constant(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            local_const :: 42;
            local_const = 1;
            return local_const;
        }
    )CODE_STR";

    Expected_Results expected = {
        .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Left side of assignment must be an lvalue")})
    };

    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Local_Variable(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            local_var := 42;
            return local_var;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Modify_Local_Variable(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        main :: () -> s64 {
            local_var := 40;
            local_var = local_var + 2;
            return local_var;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Args_And_Return_Val(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        add :: (a: s64, b: s64) -> s64 { return a + b; }
        main :: () -> s64 {
            return add(40, 2);
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 42 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult If_Statements(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        if_test :: (x: s64) -> s64 {
            result : s64 = 0;
            if (x == 1) {
                result = 1;
            } else if (x == 2) {
                result = 2;
            } else if (x == 3) {
                return 3;
            } else {
                result = 4;
            }

            print("after if");
            return result;
        }

        main :: () -> s64 {

            print(if_test(1));
            print(if_test(2));
            print(if_test(3));
            print(if_test(4));
            print(if_test(4));

            return 0;
        }
    )CODE_STR";

    Expected_Results expected = { .std_out = "after if\n1\nafter if\n2\n3\nafter if\n4\nafter if\n4" };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Boolean_If_Statements(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        pbool1 :: (x: bool) {
            if (x == true) {
                print("true");
            } else {
                print("false");
            }
        }

        pbool2 :: (x: bool) {
            if (x) {
                print("true");
            } else {
                print("false");
            }
        }

        main :: () -> s64 {

            pbool1(true);
            pbool1(false);
            pbool2(true);
            pbool2(false);

            return 0;
        }
    )CODE_STR";

    Expected_Results expected = { .std_out = "true\nfalse\ntrue\nfalse" };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Define_Struct_Type(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        Vec2 :: struct {
            x: s32;
            y: s32;
        }

        Rect :: struct {
            pos: Vec2;
            size: Vec2;
        }

        main :: () -> s64 {
            p : Vec2;
            r : Rect;
            return 0;
        }
    )CODE_STR";

    Expected_Results expected = {};
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Struct_Offset_Ptr(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        Vec2 :: struct {
            x: s32;
            y: s32;
        }

        main :: () -> s64 {
            p : Vec2;

            p.x = 1;
            p.y = 2;

            return p.x + p.y;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 3 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Nested_Struct_Offset_Ptr(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        Vec2 :: struct {
            x: s32;
            y: s32;
        }

        Rect :: struct {
            pos: Vec2;
            size: Vec2;
        }

        main :: () -> s64 {
            r : Rect;

            r.pos.x = 1;
            r.pos.y = 2;
            r.size.x = 3;
            r.size.y = 4;

            result := r.pos.x + r.pos.y + r.size.x + r.size.y;

            r2 : Rect;
            r2.pos = r.pos;
            r2.size = r.size;

            result = result + r2.pos.x + r2.pos.y + r2.size.x + r2.size.y;

            return result;
        }
    )CODE_STR";

    Expected_Results expected = { .exit_code = 20 };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Static_Array_Basics(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        global_ints : [4]s64;

        main :: () {
            ints : [5]s64;

            ints[0] = 1;
            global_ints[0] = 1;

            i1 : s64 = 1;
            ints[i1] = 2;
            global_ints[i1] = 2;

            print(ints[0]);
            print(ints[i1]);
            print(global_ints[0]);
            print(global_ints[i1]);

            v : [2]s64 = { 3, 1 };
            arr_print(v);
            v = arr_add(v, 1);
            arr_print(v);
            arr_print({1, 2});
            v = { 44, 22 };
            arr_print(v);

            arr_print(test_arr());

            return 0;
        }

        arr_add :: (a: [2]s64, x: s64) -> [2]s64 {
            a[0] = a[0] + x;
            a[1] = a[1] + x;
            return a;
        }

        arr_print :: (a: [2]s64) {
            print(a[0], ", ", a[1]);
        }

        test_arr :: () -> [2]s64 {
            return { 5, 4 };
        }
    )CODE_STR";

    Expected_Results expected = { .std_out = "1\n2\n1\n2\n3, 1\n4, 2\n1, 2\n44, 22\n5, 4" };
    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Global_Run_Directive_Return_Void(const MunitParameter params[], void* user_data_or_fixture) {

    // Running main
    {
        String_Ref code_string = R"CODE_STR(
            #run main();
            main :: () -> s64 {
                print("main");
                return 0;
            }
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "main\nmain",
            .runtime_std_out = "main"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    // print
    {
        String_Ref code_string = R"CODE_STR(
            #run print(42);
            main :: () -> s64 {
                print("main");
                return 0;
            }
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "42\nmain",
            .runtime_std_out = "main"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    // print 2
    {
        String_Ref code_string = R"CODE_STR(
            #run print(x);
            main :: () -> s64 {
                print("main");

                return 0;
            }
            x :: 42;
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "42\nmain",
            .runtime_std_out = "main"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    // block
    {
        String_Ref code_string = R"CODE_STR(
            #run {
                main();
                print(x);
                print(1);
            }
            main :: () -> s64 {
                print("main");

                return 0;
            }
            x :: 42;
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "main\n42\n1\nmain",
            .runtime_std_out = "main"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }
    return MUNIT_OK;
}

MunitResult Global_Run_Directive_Variable(const MunitParameter params[], void* user_data_or_fixture) {

    // Running main depending on variable depending on return value of another run
    {
        String_Ref code_string = R"CODE_STR(
            #run main();
            main :: () {
                print("Hello zodiac!");
                print(x);
                return 0;
            }
            x := #run ret_1();
            ret_1 :: () -> s64 {
                return 1;
            }
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "Hello zodiac!\n1\nHello zodiac!\n1",
            .runtime_std_out = "Hello zodiac!\n1"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    return MUNIT_OK;
}

MunitResult Global_Run_Directive_Constant(const MunitParameter params[], void* user_data_or_fixture) {

    // Running main depending on constant depending on return value of another run
    {
        String_Ref code_string = R"CODE_STR(
            #run main();
            main :: () {
                print("Hello zodiac!");
                print(x);
                return 0;
            }
            x :: #run ret_1();
            ret_1 :: () -> s64 {
                return 1;
            }
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "Hello zodiac!\n1\nHello zodiac!\n1",
            .runtime_std_out = "Hello zodiac!\n1"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    return MUNIT_OK;
}

MunitResult Local_Run_Directives(const MunitParameter params[], void* user_data_or_fixture) {

    // Running main depending on constant depending on return value of another run
    {
        String_Ref code_string = R"CODE_STR(
            #run main();
            main :: () {
                x := #run 20 * 2;
                y :: #run ret_x(2);
                print(x + y);
                print(#run y * 21);
                return 0;
            }
            ret_x :: (x: s64) -> s64 {
                return x;
            }
        )CODE_STR";

        // Compile and run will always run main at compile time, so account for that in the output
        Expected_Results expected = {
            .compiletime_std_out = "42\n42\n42\n42",
            .runtime_std_out = "42\n42"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    return MUNIT_OK;
}

MunitResult Run_Directive_Global_Var_Types(const MunitParameter params[], void* user_data_or_fixture) {

    // Running main depending on constant depending on return value of another run
    {
        String_Ref code_string = R"CODE_STR(
            signed_integer := #run return_signed_integer(7);
            unsigned_integer := #run return_unsigned_integer(8);
            float := #run return_float(4.2);
            double := #run return_double(8.4);
            bool_true := #run return_bool(true);
            bool_false := #run return_bool(false);
            vector := #run make_vector(2, 3);
            const_vector : Vec2 : { 2, 3 };
            aabb := #run make_aabb(const_vector, {3, 4});
            test_array := #run make_test_array();
            short_array := #run make_short_array(44, 22);

            main :: () {
                print(signed_integer);
                print(unsigned_integer);
                print(float);
                print(double);
                print(bool_true);
                print(bool_false);
                print_vector(vector);
                print_aabb(aabb);
                print_test_array(test_array);
                print_short_array(short_array);
                return 0;
            }
            return_signed_integer :: (x: s64) -> s64 { return x; }
            return_unsigned_integer :: (x: u64) -> u64 { return x; }
            return_float :: (x: r32) -> r32 { return x; }
            return_double :: (x: r64) -> r64 { return x; }
            return_bool :: (x: bool) -> bool { return x; }

            Vec2 :: struct { x, y: s64; }
            make_vector :: (x: s64, y: s64) -> Vec2 {
                result: Vec2;
                result.x = x;
                result.y = y;
                return result;
            }
            print_vector :: (v: Vec2) {
                print(v.x, ", ", v.y);
            }

            AABB :: struct { pos, size: Vec2; }
            make_aabb :: (pos: Vec2, size: Vec2) -> AABB {
                result: AABB;
                result.pos = pos;
                result.size = size;
                return result;
            }
            print_aabb :: (r: AABB) {
                print(r.pos.x, ", ", r.pos.y, ", ", r.size.x, ", ", r.size.y);
            }
            make_test_array :: () -> [5]s64 {
                return { 5, 4, 3, 2, 1 };
            }
            print_test_array :: (a: [5]s64) {
                print(a[0], ", ", a[1], ", ", a[2], ", ", a[3], ", ", a[4]);
            }
            make_short_array :: (x0: s64, x1: s64) -> [2]s64 {
                r: [2]s64;
                r[0] = x0;
                r[1] = x1;
                return r;
            }
            print_short_array :: (r: [2]s64) {
                print(r[0], ", ", r[1]);
            }
        )CODE_STR";

        Expected_Results expected = {
            .std_out = "7\n8\n4.200000\n8.400000\ntrue\nfalse\n2, 3\n2, 3, 3, 4\n5, 4, 3, 2, 1\n44, 22",
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    return MUNIT_OK;
}

MunitResult Run_Directive_Global_Const_Types(const MunitParameter params[], void* user_data_or_fixture) {

    // Running main depending on constant depending on return value of another run
    {
        String_Ref code_string = R"CODE_STR(
            signed_integer :: #run return_signed_integer(7);
            unsigned_integer :: #run return_unsigned_integer(8);
            float :: #run return_float(4.2);
            double :: #run return_double(8.4);
            bool_true :: #run return_bool(true);
            bool_false :: #run return_bool(false);
            vector :: #run make_vector(2, 3);
            aabb :: #run make_aabb(vector, {3, 4});
            test_array :: #run make_test_array();
            short_array :: #run make_short_array(44, 22);

            main :: () {
                print(signed_integer);
                print(unsigned_integer);
                print(float);
                print(double);
                print(bool_true);
                print(bool_false);
                print_vector(vector);
                print_aabb(aabb);
                print_test_array(test_array);
                print_short_array(short_array);
                return 0;
            }
            return_signed_integer :: (x: s64) -> s64 { return x; }
            return_unsigned_integer :: (x: u64) -> u64 { return x; }
            return_float :: (x: r32) -> r32 { return x; }
            return_double :: (x: r64) -> r64 { return x; }
            return_bool :: (x: bool) -> bool { return x; }

            Vec2 :: struct { x, y: s64; }
            make_vector :: (x: s64, y: s64) -> Vec2 {
                result: Vec2;
                result.x = x;
                result.y = y;
                return result;
            }
            print_vector :: (v: Vec2) {
                print(v.x, ", ", v.y);
            }

            AABB :: struct { pos, size: Vec2; }
            make_aabb :: (pos: Vec2, size: Vec2) -> AABB {
                result: AABB;
                result.pos = pos;
                result.size = size;
                return result;
            }
            print_aabb :: (r: AABB) {
                print(r.pos.x, ", ", r.pos.y, ", ", r.size.x, ", ", r.size.y);
            }
            make_test_array :: () -> [5]s64 {
                return { 5, 4, 3, 2, 1 };
            }
            print_test_array :: (a: [5]s64) {
                print(a[0], ", ", a[1], ", ", a[2], ", ", a[3], ", ", a[4]);
            }
            make_short_array :: (x0: s64, x1: s64) -> [2]s64 {
                r: [2]s64;
                r[0] = x0;
                r[1] = x1;
                return r;
            }
            print_short_array :: (r: [2]s64) {
                print(r[0], ", ", r[1]);
            }
        )CODE_STR";

        Expected_Results expected = {
            .std_out = "7\n8\n4.200000\n8.400000\ntrue\nfalse\n2, 3\n2, 3, 3, 4\n5, 4, 3, 2, 1\n44, 22",
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };

        if (result.result != MUNIT_OK) return result.result;
    }

    return MUNIT_OK;
}

MunitResult Run_Directive_Struct_Member_Types(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        S :: struct {
            _u64 : u64;
            _s64 : s64;
            _u32 : u32;
            _s32 : s32;
            _u16 : u16;
            _s16 : s16;
            _u8  :  u8;
            _s8  :  s8;
            _r64 : r64;
            _r32 : r32;
            _bool1 : bool;
            _bool2 : bool;
            _ptr_vec2 : *Vec2;
            _ss : SS;
            _bytes : [7]u8;
            // TODO: function (pointer)?
        }
        ss: SS : { 1, { 2.2, 3.3 }};
        v1: Vec2 : { 1.0, 2.0 };
        gs : S = #run make_s(11, 22, 33, 44, 55, 66, 77, 88, 9.9, 10.10, true, false, *v1, ss, { 1, 2, 3, 4, 5, 6, 7 });
        gcs : S : #run make_s(1, 2, 3, 4, 5, 6, 7, 8, 9.99, 10.1010, false, true, *v1, ss, { 7, 6, 5, 4, 3, 2, 1 });
        main :: () {
            print_s(*gs);
            print_s(*gcs);
            return 0;
        }
        Vec2 :: struct { x, y: r64; }
        SS :: struct { id : u32; v  : Vec2; }
        make_s :: (_u64: u64, _s64: s64, _u32: u32, _s32: s32, _u16: u16, _s16: s16, _u8: u8, _s8: s8, _r32: r32, _r64: r64, _b1: bool, _b2: bool, _ptr_vec2: *Vec2, _ss: SS, _bytes: [7]u8) -> S {
            s: S;
            s._u64 = _u64;
            s._s64 = _s64;
            s._u32 = _u32;
            s._s32 = _s32;
            s._u16 = _u16;
            s._s16 = _s16;
            s._u8  =  _u8;
            s._s8  =  _s8;
            s._r64 = _r64;
            s._r32 = _r32;
            s._bool1 = _b1;
            s._bool2 = _b2;
            s._ptr_vec2 = _ptr_vec2;
            s._ss = _ss;
            s._bytes = _bytes;
            return s;
        }
        print_s :: (s : *S) {
            print("{ ",
                  s._u64, ", ",
                  s._s64, ", ",
                  s._u32, ", ",
                  s._s32, ", ",
                  s._u16, ", ",
                  s._s16, ", ",
                  s._u8,  ", ",
                  s._s8,  ", ",
                  s._r64,  ", ",
                  s._r32,  ", ",
                  s._bool1,  ", ",
                  s._bool2,  ", ",
                  // s._ptr_vec2,  ", ", // Can't really test this
                  "{ ", s._ss.id, ", { ", s._ss.v.x, ", ", s._ss.v.y, " } }, ",
                  s._bytes,
                  " }");
        }
    )CODE_STR";

    Expected_Results expected = {
        .std_out = R"OUT_STR({ 11, 22, 33, 44, 55, 66, 77, 88, 10.100000, 9.900000, true, false, { 1, { 2.200000, 3.300000 } }, { 1, 2, 3, 4, 5, 6, 7 } }
{ 1, 2, 3, 4, 5, 6, 7, 8, 10.101000, 9.990000, false, true, { 1, { 2.200000, 3.300000 } }, { 7, 6, 5, 4, 3, 2, 1 } })OUT_STR",

    };

    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Run_Directive_And_Pointer_To_Const(const MunitParameter params[], void* user_data_or_fixture) {

    String_Ref code_string = R"CODE_STR(
        S :: struct { val : s64; }
        X : s64 : #run return_x(42);
        gcs : S : #run make_s(*X);
        main :: () {
            print_s(*gcs);
            return 0;
        }
        return_x :: (x: s64) { return x; }
        make_s :: (val_ptr: *s64) -> S {
            result: S;
            result.val = <val_ptr;
            return result;
        }
        print_s :: (s: *S) {
            print(s.val);
        }
    )CODE_STR";

    Expected_Results expected = {
        .std_out = "42",
    };

    auto result = compile_and_run(code_string, expected);
    defer { free_compile_run_results(&result); };

    return result.result;
}

MunitResult Run_Expr_Const(const MunitParameter params[], void* user_data_or_fixture) {

    {
        String_Ref code_string = R"CODE_STR(
            x : s64 = 42;
            y := #run x;
        )CODE_STR";

        Expected_Results expected = {
            .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Run directive expression must be constant")})
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    {
        String_Ref code_string = R"CODE_STR(
            X : s64 : 42;
            y := #run X;
            main :: () { return 0; }
        )CODE_STR";

        Expected_Results expected = {
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    return MUNIT_OK;
}

MunitResult Run_Call_Arg_Const(const MunitParameter params[], void* user_data_or_fixture) {

    {
        String_Ref code_string = R"CODE_STR(
            return_x :: (x: s64) { return x; }
            x : s64 = 42;
            y := #run return_x(x);
        )CODE_STR";

        Expected_Results expected = {
            .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Run directive expression must be constant")})
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    {
        String_Ref code_string = R"CODE_STR(
            return_x :: (x: s64) { return x; }
            x : s64 : 42;
            y := #run return_x(x);
            main :: () { return 0; }
        )CODE_STR";

        Expected_Results expected = {
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    return MUNIT_OK;
}

MunitResult Run_Print_Arg_Const(const MunitParameter params[], void* user_data_or_fixture) {

    {
        String_Ref code_string = R"CODE_STR(
            x : s64 = 42;
            #run print(x);
        )CODE_STR";

        Expected_Results expected = {
            .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Arguments to print in #run must be constant")})
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    {
        String_Ref code_string = R"CODE_STR(
            x : s64 : 42;
            #run print(x);
            main :: () { return 0; }
        )CODE_STR";

        Expected_Results expected = {
            .compiletime_std_out = "42"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    return MUNIT_OK;
}

MunitResult Run_Assignment_Is_Expression(const MunitParameter params[], void* user_data_or_fixture) {

    {
        String_Ref code_string = R"CODE_STR(
            x : s64 : 42;
            y := #run print(x);
        )CODE_STR";

        Expected_Results expected = {
            .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Expected expression after #run in assignment")})
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    return MUNIT_OK;
}

MunitResult Run_Call_Arg_In_Block_Const(const MunitParameter params[], void* user_data_or_fixture) {

    {
        String_Ref code_string = R"CODE_STR(
            return_x :: (x: s64) { return x; }
            x : s64 = 42;
            #run { return_x(x); }
        )CODE_STR";

        Expected_Results expected = {
            .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Run directive expression must be constant")})
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    {
        String_Ref code_string = R"CODE_STR(
            return_x :: (x: s64) { return x; }
            x : s64 : 42;
            #run { return_x(x); }
            main :: () { return 0; }
        )CODE_STR";

        Expected_Results expected = {
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    return MUNIT_OK;
}

MunitResult Run_Print_Arg_In_Block_Const(const MunitParameter params[], void* user_data_or_fixture) {

    {
        String_Ref code_string = R"CODE_STR(
            x : s64 = 42;
            #run { print(x); }
        )CODE_STR";

        Expected_Results expected = {
            .resolve_errors = Array_Ref<Expected_Error>({ RESOLVE_ERR(true, "Arguments to print in #run must be constant")})
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    {
        String_Ref code_string = R"CODE_STR(
            x : s64 : 42;
            #run { print(x); }
            main :: () { return 0; }
        )CODE_STR";

        Expected_Results expected = {
            .compiletime_std_out = "42"
        };

        auto result = compile_and_run(code_string, expected);
        defer { free_compile_run_results(&result); };


        munit_assert(result.result == MUNIT_OK);
    }

    return MUNIT_OK;
}

#undef RESOLVE_ERR

}}
