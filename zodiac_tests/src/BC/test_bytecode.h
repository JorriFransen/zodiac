#pragma once

#include <munit/munit.h>

#include "test_common.h"

#include "bytecode/bytecode.h"
#include "bytecode/interpreter.h"
#include "bytecode/validator.h"
#include "type.h"
#include "zodiac_context.h"

#ifndef PRINT_BYTECODE_IN_TESTS
#define PRINT_BYTECODE_IN_TESTS 0
#endif // PRINT_BYTECODE_IN_TESTS

#if PRINT_BYTECODE_IN_TESTS
#include "bytecode/printer.h"
#endif

namespace Zodiac { namespace Bytecode_Tests {

using namespace Bytecode;

file_local void print_bytecode(const Bytecode_Builder &bb)
{
#if PRINT_BYTECODE_IN_TESTS

    printf("\n\n");

    // Assume this uses printf
    bytecode_print(&bb, temp_allocator_allocator());
#endif
}

file_local MunitResult Building_1(const MunitParameter params[], void* user_data_or_fixture)
{
    Zodiac_Context zc;
    zodiac_context_create(&zc);

    Bytecode_Builder bb = bytecode_builder_create(c_allocator(), &zc);
    Type *main_fn_type = get_function_type(&builtin_type_void, {}, &zc.ast_allocator);

    Bytecode_Function_Handle fn_handle = bytecode_function_create(&bb, "main_fn", main_fn_type);
    Bytecode_Block_Handle block_handle = bytecode_append_block(&bb, fn_handle, "entry");

    bytecode_set_insert_point(&bb, fn_handle, block_handle);

    Bytecode_Register a = bytecode_integer_literal(&bb, &builtin_type_s64, 40);
    Bytecode_Register b = bytecode_integer_literal(&bb, &builtin_type_s64, 2);

    Bytecode_Register result = bytecode_emit_add(&bb, a, b);

    munit_assert_int64(bb.functions.count, ==, 1);
    munit_assert_int64(bb.functions[0].registers.count, ==, 1);
    munit_assert_int64(bb.functions[0].blocks.count, ==, 1);
    munit_assert_int64(bb.functions[0].blocks[0].instructions.count, ==, 1);

    munit_assert(a.flags & BC_REGISTER_FLAG_LITERAL);
    munit_assert(b.flags & BC_REGISTER_FLAG_LITERAL);

    munit_assert(a.kind == Bytecode_Register_Kind::TEMPORARY);
    munit_assert(b.kind == Bytecode_Register_Kind::TEMPORARY);
    munit_assert(result.kind == Bytecode_Register_Kind::TEMPORARY);

    Bytecode_Instruction add_instr = bb.functions[0].blocks[0].instructions[0];
    munit_assert(add_instr.op == Bytecode_Opcode::I_ADD);
    munit_assert_int64(add_instr.a.index, ==, a.index);
    munit_assert_int64(add_instr.b.index, ==, b.index);
    munit_assert_int64(add_instr.dest.index, ==, add_instr.dest.index);

    print_bytecode(bb);

    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return MUNIT_OK;
}

#define assert_zodiac_stream(stream, expected_string) { \
    filesystem_flush(&stream); \
    u64 length; \
    filesystem_size(&stream, &length); \
    const auto _buf_size = 1024; \
    munit_assert((s64)_buf_size > length); \
    char _buf[_buf_size]; \
    u64 read_length; \
    bool read_res = filesystem_read(&stream, length, (u8 *)_buf, &read_length); \
    munit_assert_int(read_length, ==, length); \
    _buf[length] = '\0'; \
    munit_assert_int((int)strlen(_buf), ==, (int)strlen(expected_string)); \
    munit_assert_string_equal(_buf, expected_string); \
}

file_local MunitResult Simple_Function_Call(const MunitParameter params[], void* user_data_or_fixture)
{
    Zodiac_Context zc;
    zodiac_context_create(&zc);

    Bytecode_Builder bb = bytecode_builder_create(c_allocator(), &zc);
    Type *main_fn_type = get_function_type(&builtin_type_void, {}, &zc.ast_allocator);

    Bytecode_Function_Handle fn_handle = bytecode_function_create(&bb, "main_fn", main_fn_type);
    Bytecode_Block_Handle block_handle = bytecode_append_block(&bb, fn_handle, "entry");

    auto print_fn_type = get_function_type(&builtin_type_void, {}, &zc.ast_allocator);
    auto print_fn_handle = bytecode_function_create(&bb, "print_42", print_fn_type);
    auto print_entry_handle = bytecode_append_block(&bb, print_fn_handle, "entry");

    bytecode_set_insert_point(&bb, fn_handle, block_handle);

    bytecode_emit_call(&bb, print_fn_handle, 0);
    bytecode_emit_return(&bb);

    bytecode_set_insert_point(&bb, print_fn_handle, print_entry_handle);
    bytecode_emit_print(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 42));
    bytecode_emit_return(&bb);

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_allocator(), &zc);
        filesystem_temp_file(&interp.std_out);

        auto program = bytecode_get_program(&bb);
        program.entry_handle = fn_handle;
        interpreter_start(&interp, program);

        assert_zodiac_stream(interp.std_out, "42\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);

    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Arguments_And_Return_Values(const MunitParameter params[], void* user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    // ADD
    Type *add_args[] = { &builtin_type_s64, &builtin_type_s64 };
    auto add_fn_type = get_function_type(&builtin_type_s64, add_args, &zc.ast_allocator);
    auto add_fn_handle = bytecode_function_create(&bb, "add", add_fn_type);
    {
        auto add_fn_entry_handle = bytecode_append_block(&bb, add_fn_handle, "entry");

        bytecode_set_insert_point(&bb, add_fn_handle, add_fn_entry_handle);
        auto a1 = bytecode_emit_load_argument(&bb, 0);
        auto a2 = bytecode_emit_load_argument(&bb, 1);
        auto add_result = bytecode_emit_add(&bb, a1, a2);
        bytecode_emit_print(&bb, add_result);
        bytecode_emit_return(&bb, add_result);
    }


    // Main
    Bytecode_Function_Handle main_fn_handle = -1;
    {
        Type *main_fn_type = get_function_type(&builtin_type_void, Array_Ref<Type*>(), &zc.ast_allocator);

        main_fn_handle = bytecode_function_create(&bb, "main_fn", main_fn_type);
        Bytecode_Block_Handle entry_block_handle = bytecode_append_block(&bb, main_fn_handle, "entry");

        bytecode_set_insert_point(&bb, main_fn_handle, entry_block_handle);

        bytecode_emit_push_arg(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 2));
        bytecode_emit_push_arg(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 40));

        auto result = bytecode_emit_call(&bb, add_fn_handle, 2);
        bytecode_emit_print(&bb, result);

        bytecode_emit_return(&bb);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {
        Interpreter interp = interpreter_create(c_alloc, &zc);

        filesystem_temp_file(&interp.std_out);

        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn_handle;
        interpreter_start(&interp, program);

        assert_zodiac_stream(interp.std_out, "42\n42\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Recursion_And_Jumps(const MunitParameter params[], void* user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    // recurse
    Type *param_types[] = { &builtin_type_s64 };
    auto recursive_fn_type = get_function_type(&builtin_type_s64, param_types, &zc.ast_allocator);
    auto recursive_fn_handle = bytecode_function_create(&bb, "recurse", recursive_fn_type);
    {
        auto add_fn_entry_handle = bytecode_append_block(&bb, recursive_fn_handle, "entry");
        auto recurse_block = bytecode_append_block(&bb, recursive_fn_handle, "recurse_block");
        auto return_block = bytecode_append_block(&bb, recursive_fn_handle, "return_block");

        bytecode_set_insert_point(&bb, recursive_fn_handle, add_fn_entry_handle);

        auto a1 = bytecode_emit_load_argument(&bb, 0);
        auto cond = bytecode_emit_gt(&bb, a1, bytecode_integer_literal(&bb, &builtin_type_s64, 0));
        bytecode_emit_print(&bb, cond);
        bytecode_emit_jmp_if(&bb, cond, recurse_block, return_block);

        bytecode_set_insert_point(&bb, recursive_fn_handle, recurse_block);
        auto next_value = bytecode_emit_sub(&bb, a1, bytecode_integer_literal(&bb, &builtin_type_s64, 1));
        bytecode_emit_push_arg(&bb, next_value);
        auto result_val = bytecode_emit_call(&bb, recursive_fn_handle, 1);
        bytecode_emit_return(&bb, result_val);


        bytecode_set_insert_point(&bb, recursive_fn_handle, return_block);
        bytecode_emit_return(&bb, a1);
    }

    // Main
    Bytecode_Function_Handle main_fn_handle = -1;
    {
        Type *main_fn_type = get_function_type(&builtin_type_void, Array_Ref<Type*>(), &zc.ast_allocator);

        main_fn_handle = bytecode_function_create(&bb, "main_fn", main_fn_type);
        Bytecode_Block_Handle entry_block_handle = bytecode_append_block(&bb, main_fn_handle, "entry");
        auto return_block = bytecode_append_block(&bb, main_fn_handle, "return_block");

        bytecode_set_insert_point(&bb, main_fn_handle, entry_block_handle);

        bytecode_emit_push_arg(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 2));

        auto result = bytecode_emit_call(&bb, recursive_fn_handle, 1);
        bytecode_emit_jmp(&bb, return_block);

        bytecode_set_insert_point(&bb, main_fn_handle, return_block);
        bytecode_emit_print(&bb, result);
        bytecode_emit_return(&bb);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {
        Interpreter interp = interpreter_create(c_alloc, &zc);

        filesystem_temp_file(&interp.std_out);

        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn_handle;
        interpreter_start(&interp, program);

        assert_zodiac_stream(interp.std_out, "true\ntrue\nfalse\n0\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Insert_And_Extract_Value(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto fn_type = get_function_type(&builtin_type_void, { }, &zc.ast_allocator);
    auto fn = bytecode_function_create(&bb, "insert_and_extract_value", fn_type);
    auto entry_block = bytecode_append_block(&bb, fn, "entry");

    bytecode_set_insert_point(&bb, fn, entry_block);

    Type *vec2_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec2_type = get_struct_type(&zc, vec2_mem_types, "Vec2", &zc.ast_allocator);
    munit_assert(vec2_type != nullptr);

    auto alloc = bytecode_emit_alloc(&bb, vec2_type, "struct_alloc");
    munit_assert(alloc.type == vec2_type);

    auto lit_42 = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
    auto new_struct_val = bytecode_emit_insert_value(&bb, {}, lit_42, vec2_type, 0);
    munit_assert(new_struct_val.type == vec2_type);

    auto lit_24 = bytecode_integer_literal(&bb, &builtin_type_s64, 24);
    new_struct_val = bytecode_emit_insert_value(&bb, new_struct_val, lit_24, vec2_type, 1);
    munit_assert(new_struct_val.type == vec2_type);

    bytecode_emit_store_alloc(&bb, new_struct_val, alloc);

    auto sv = bytecode_emit_load_alloc(&bb, alloc);
    auto x = bytecode_emit_extract_value(&bb, sv, 0);
    auto y = bytecode_emit_extract_value(&bb, sv, 1);

    bytecode_emit_print(&bb, x);
    bytecode_emit_print(&bb, y);

    bytecode_emit_return(&bb);

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        auto interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = fn;
        interpreter_start(&interp, program);

        assert_zodiac_stream(interp.std_out, "42\n24\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Extract_Struct_Value(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec2_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec2_type = get_struct_type(&zc, vec2_mem_types, "vec2", &zc.ast_allocator);

    Type *aabb_mem_types[] = { vec2_type, vec2_type };
    Type *aabb_type = get_struct_type(&zc, aabb_mem_types, "aabb", &zc.ast_allocator);

    auto insert_and_extract_value_fn_type = get_function_type(aabb_type, { }, &zc.ast_allocator);
    auto insert_and_extract_value_fn = bytecode_function_create(&bb, "insert_and_extract_value", insert_and_extract_value_fn_type);
    auto entry_block = bytecode_append_block(&bb, insert_and_extract_value_fn, "entry");

    bytecode_set_insert_point(&bb, insert_and_extract_value_fn, entry_block);
    {
        auto lit_1 = bytecode_integer_literal(&bb, &builtin_type_s64, 1);
        auto lit_2 = bytecode_integer_literal(&bb, &builtin_type_s64, 2);
        auto p1 = bytecode_emit_insert_value(&bb, {}, lit_1, vec2_type, 0);
        p1 = bytecode_emit_insert_value(&bb, p1, lit_2, vec2_type, 1);

        auto lit_3 = bytecode_integer_literal(&bb, &builtin_type_s64, 3);
        auto lit_4 = bytecode_integer_literal(&bb, &builtin_type_s64, 4);
        auto p2 = bytecode_emit_insert_value(&bb, {}, lit_3, vec2_type, 0);
        p2 = bytecode_emit_insert_value(&bb, p2, lit_4, vec2_type, 1);

        auto aabb = bytecode_emit_insert_value(&bb, {}, p1, aabb_type, 0);
        aabb = bytecode_emit_insert_value(&bb, aabb, p2, aabb_type, 1);

        bytecode_emit_return(&bb, aabb);
    }

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto result = bytecode_emit_call(&bb, insert_and_extract_value_fn, 0);
        auto p1 = bytecode_emit_extract_value(&bb, result, 0);
        auto x1 = bytecode_emit_extract_value(&bb, p1, 0);
        auto y1 = bytecode_emit_extract_value(&bb, p1, 1);
        auto p2 = bytecode_emit_extract_value(&bb, result, 1);
        auto x2 = bytecode_emit_extract_value(&bb, p2, 0);
        auto y2 = bytecode_emit_extract_value(&bb, p2, 1);
        bytecode_emit_print(&bb, x1);
        bytecode_emit_print(&bb, y1);
        bytecode_emit_print(&bb, x2);
        bytecode_emit_print(&bb, y2);
        auto sum = bytecode_emit_add(&bb, x1, y1);
        sum = bytecode_emit_add(&bb, sum, x2);
        sum = bytecode_emit_add(&bb, sum, y2);
        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Interpreter interp = interpreter_create(c_alloc, &zc);
    filesystem_temp_file(&interp.std_out);
    auto program = bytecode_get_program(&bb);
    program.entry_handle = main_fn;
    interpreter_start(&interp, program);

    assert_zodiac_stream(interp.std_out, "1\n2\n3\n4\n");

    munit_assert(filesystem_close(&interp.std_out));

    interpreter_free(&interp);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return MUNIT_OK;
}

file_local MunitResult Return_Struct(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec2_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec2_type = get_struct_type(&zc, vec2_mem_types, "vec2", &zc.ast_allocator);

    auto make_vec2_fn_type = get_function_type(vec2_type, { }, &zc.ast_allocator);
    auto make_vec2_fn = bytecode_function_create(&bb, "make_vec2", make_vec2_fn_type);
    auto entry_block = bytecode_append_block(&bb, make_vec2_fn, "entry");

    bytecode_set_insert_point(&bb, make_vec2_fn, entry_block);

    auto lit_42 = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
    auto new_struct_val = bytecode_emit_insert_value(&bb, {}, lit_42, vec2_type, 0);

    auto lit_24 = bytecode_integer_literal(&bb, &builtin_type_s64, 24);
    new_struct_val = bytecode_emit_insert_value(&bb, new_struct_val, lit_24, vec2_type, 1);

    bytecode_emit_return(&bb, new_struct_val);

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto result = bytecode_emit_call(&bb, make_vec2_fn, 0);
        auto x = bytecode_emit_extract_value(&bb, result, 0);
        auto y = bytecode_emit_extract_value(&bb, result, 1);
        bytecode_emit_print(&bb, x);
        bytecode_emit_print(&bb, y);
        auto sum = bytecode_emit_add(&bb, x, y);
        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);

        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);

        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 66);

        assert_zodiac_stream(interp.std_out, "42\n24\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Struct_Arguments(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec2_mem_types[] = { &builtin_type_r32, &builtin_type_r32 };
    Type *vec2_type = get_struct_type(&zc, vec2_mem_types, "vec2", &zc.ast_allocator);

    auto make_vec2_fn_type = get_function_type(vec2_type, { }, &zc.ast_allocator);
    auto make_vec2_fn = bytecode_function_create(&bb, "make_vec2", make_vec2_fn_type);
    auto entry_block = bytecode_append_block(&bb, make_vec2_fn, "entry");

    bytecode_set_insert_point(&bb, make_vec2_fn, entry_block);
    {
        auto lit_1 = bytecode_real_literal(&bb, &builtin_type_r32, 3, 3.f);
        auto lit_2 = bytecode_real_literal(&bb, &builtin_type_r32, 4, 4.f);
        auto p1 = bytecode_emit_insert_value(&bb, {}, lit_1, vec2_type, 0);
        p1 = bytecode_emit_insert_value(&bb, p1, lit_2, vec2_type, 1);

        bytecode_emit_return(&bb, p1);
    }

    Type *vec2_len_fn_arg_types[] = { vec2_type };
    auto vec2_len_fn_type = get_function_type(&builtin_type_r32, vec2_len_fn_arg_types, &zc.ast_allocator);
    auto vec2_len_fn = bytecode_function_create(&bb, "vec2_len", vec2_len_fn_type);
    auto vec2_len_entry_block = bytecode_append_block(&bb, vec2_len_fn, "entry");
    bytecode_set_insert_point(&bb, vec2_len_fn, vec2_len_entry_block);
    {
        auto vec = bytecode_emit_load_argument(&bb, 0);
        auto x = bytecode_emit_extract_value(&bb, vec, 0);
        auto y = bytecode_emit_extract_value(&bb, vec, 1);
        x = bytecode_emit_mul(&bb, x, x);
        y = bytecode_emit_mul(&bb, y, y);
        auto sum = bytecode_emit_add(&bb, x, y);
        auto result = bytecode_emit_sqrt(&bb, sum);

        // We are changing this value to verify the struct in the calling function doesn't change
        bytecode_emit_insert_value(&bb, vec, sum, vec.type,  0);

        bytecode_emit_return(&bb, result);
    }

    auto print_vec2_fn_type = get_function_type(&builtin_type_void, vec2_len_fn_arg_types, &zc.ast_allocator);
    auto print_vec2_fn = bytecode_function_create(&bb, "vec2_print", print_vec2_fn_type);
    auto print_vec2_entry_block = bytecode_append_block(&bb, print_vec2_fn, "entry");
    bytecode_set_insert_point(&bb, print_vec2_fn, print_vec2_entry_block);
    {
        auto vec = bytecode_emit_load_argument(&bb, 0);
        auto x = bytecode_emit_extract_value(&bb, vec, 0);
        auto y = bytecode_emit_extract_value(&bb, vec, 1);
        bytecode_emit_print(&bb, x);
        bytecode_emit_print(&bb, y);
        bytecode_emit_return(&bb);
    }

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto v = bytecode_emit_call(&bb, make_vec2_fn, 0);
        bytecode_emit_push_arg(&bb, v);
        bytecode_emit_call(&bb, print_vec2_fn, 1);
        bytecode_emit_push_arg(&bb, v);
        auto v_len = bytecode_emit_call(&bb, vec2_len_fn, 1);
        bytecode_emit_print(&bb, v_len);
        bytecode_emit_return(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 42));
    }


    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 42);

        assert_zodiac_stream(interp.std_out, "3.000000\n4.000000\n5.000000\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Basic_Pointers(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto int_alloc = bytecode_emit_alloc(&bb, &builtin_type_s64, "x");
        bytecode_emit_store_alloc(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 42), int_alloc);

        auto int_alloc_addr = bytecode_emit_address_of_alloc(&bb, int_alloc);
        auto int_value = bytecode_emit_load_pointer(&bb, int_alloc_addr);
        bytecode_emit_print(&bb, int_value);

        int_value = bytecode_emit_add(&bb, int_value, bytecode_integer_literal(&bb, &builtin_type_s64, 1));
        bytecode_emit_store_pointer(&bb, int_value, int_alloc_addr);

        bytecode_emit_return(&bb, bytecode_emit_load_alloc(&bb, int_alloc));
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 43);

        assert_zodiac_stream(interp.std_out, "42\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Struct_Pointers(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec_type = get_struct_type(&zc, vec_mem_types, "Vec2", &zc.ast_allocator);

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto struct_alloc = bytecode_emit_alloc(&bb, vec_type, "v");
        auto struct_val = bytecode_emit_insert_value(&bb, {}, bytecode_integer_literal(&bb, &builtin_type_s64, 42), vec_type, 0);
        struct_val = bytecode_emit_insert_value(&bb, struct_val, bytecode_integer_literal(&bb, &builtin_type_s64, 24), vec_type, 1);
        bytecode_emit_store_alloc(&bb, struct_val, struct_alloc);

        auto struct_ptr = bytecode_emit_address_of_alloc(&bb, struct_alloc);
        auto loaded_struct = bytecode_emit_load_pointer(&bb, struct_ptr);
        auto x = bytecode_emit_extract_value(&bb, loaded_struct, 0);
        auto y = bytecode_emit_extract_value(&bb, loaded_struct, 1);

        auto sum = bytecode_emit_add(&bb, x, y);

        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        auto progam = bytecode_get_program(&bb);
        progam.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, progam);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 66);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Invalid_Extract_Element(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec_type = get_struct_type(&zc, vec_mem_types, "Vec2", &zc.ast_allocator);

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto struct_alloc = bytecode_emit_alloc(&bb, vec_type, "v");
        auto inserted_struct_val = bytecode_emit_insert_value(&bb, struct_alloc, bytecode_integer_literal(&bb, &builtin_type_s64, 42), vec_type, 0);
        inserted_struct_val = bytecode_emit_insert_value(&bb, struct_alloc, bytecode_integer_literal(&bb, &builtin_type_s64, 24), vec_type, 1);

        auto struct_ptr = bytecode_emit_address_of_alloc(&bb, struct_alloc);
        auto loaded_struct = bytecode_emit_load_pointer(&bb, struct_ptr);
        auto x = bytecode_emit_extract_value(&bb, loaded_struct, 0);
        auto y = bytecode_emit_extract_value(&bb, loaded_struct, 1);

        auto sum = bytecode_emit_add(&bb, x, y);

        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    bytecode_validator_print_errors(&validator);

    munit_assert_false(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 1);

    Validation_Error &ve = validator.errors[0];
    auto err = &zc.errors[ve.error_handle];

    auto expected_msg = "The 'a' register of 'INSERT_VALUE' must be a temporary or <undef>";
    munit_assert_int64(err->message.length, ==, (s64)strlen(expected_msg));
    munit_assert_string_equal(err->message.data, expected_msg);

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return MUNIT_OK;
}

file_local MunitResult Simple_AGG_OFFSET_PTR(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec_type = get_struct_type(&zc, vec_mem_types, "Vec2", &zc.ast_allocator);

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto struct_alloc = bytecode_emit_alloc(&bb, vec_type, "v");
        auto lit_42 = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto lit_24 = bytecode_integer_literal(&bb, &builtin_type_s64, 24);
        auto struct_val = bytecode_emit_insert_value( &bb, {}, lit_42, vec_type, 0);
        struct_val = bytecode_emit_insert_value( &bb, struct_val, lit_24, vec_type, 1);
        bytecode_emit_store_alloc(&bb, struct_val, struct_alloc);

        auto x_ptr = bytecode_emit_aggregate_offset_pointer(&bb, struct_alloc, 0);
        auto y_ptr = bytecode_emit_aggregate_offset_pointer(&bb, struct_alloc, 1);

        auto x = bytecode_emit_load_pointer(&bb, x_ptr);
        auto y = bytecode_emit_load_pointer(&bb, y_ptr);

        auto sum = bytecode_emit_add(&bb, x, y);

        bytecode_emit_print(&bb, sum);

        auto struct_ptr = bytecode_emit_address_of_alloc(&bb, struct_alloc);
        x_ptr = bytecode_emit_aggregate_offset_pointer(&bb, struct_ptr, 0);
        y_ptr = bytecode_emit_aggregate_offset_pointer(&bb, struct_ptr, 1);

        x = bytecode_emit_load_pointer(&bb, x_ptr);
        y = bytecode_emit_load_pointer(&bb, y_ptr);
        auto sum2 = bytecode_emit_add(&bb, x, y);
        sum = bytecode_emit_add(&bb, sum, sum2);
        bytecode_emit_print(&bb, sum);

        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 132);

        assert_zodiac_stream(interp.std_out, "66\n132\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Nested_AGG_OFFSET_PTR(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec_type = get_struct_type(&zc, vec_mem_types, "Vec2", &zc.ast_allocator);

    Type *aabb_mem_types[] = { vec_type, vec_type };
    Type *aabb_type = get_struct_type(&zc, aabb_mem_types, "AABB", &zc.ast_allocator);

    auto main_fn_type = get_function_type(&builtin_type_s64, { }, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto aabb_alloc = bytecode_emit_alloc(&bb, aabb_type, "rect");

        auto p1_ptr = bytecode_emit_aggregate_offset_pointer(&bb, aabb_alloc, 0);
        auto p2_ptr = bytecode_emit_aggregate_offset_pointer(&bb, aabb_alloc, 1);

        auto x1_ptr = bytecode_emit_aggregate_offset_pointer(&bb, p1_ptr, 0);
        auto y1_ptr = bytecode_emit_aggregate_offset_pointer(&bb, p1_ptr, 1);

        auto x2_ptr = bytecode_emit_aggregate_offset_pointer(&bb, p2_ptr, 0);
        auto y2_ptr = bytecode_emit_aggregate_offset_pointer(&bb, p2_ptr, 1);

        bytecode_emit_store_pointer(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 11), x1_ptr);
        bytecode_emit_store_pointer(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 22), y1_ptr);

        bytecode_emit_store_pointer(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 33), x2_ptr);
        bytecode_emit_store_pointer(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 44), y2_ptr);

        auto aabb = bytecode_emit_load_alloc(&bb, aabb_alloc);
        auto p1 = bytecode_emit_extract_value(&bb, aabb, 0);
        auto p2 = bytecode_emit_extract_value(&bb, aabb, 1);

        auto x1 = bytecode_emit_extract_value(&bb, p1, 0);
        auto y1 = bytecode_emit_extract_value(&bb, p1, 1);

        auto x2 = bytecode_emit_extract_value(&bb, p2, 0);
        auto y2 = bytecode_emit_extract_value(&bb, p2, 1);

        bytecode_emit_print(&bb, x1);
        bytecode_emit_print(&bb, y1);
        bytecode_emit_print(&bb, x2);
        bytecode_emit_print(&bb, y2);

        bytecode_emit_return(&bb, x2);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 33);

        assert_zodiac_stream(interp.std_out, "11\n22\n33\n44\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Insert_And_Extract_Element(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto array_type = get_static_array_type(&builtin_type_s64, 5, &zc.ast_allocator);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto array_alloc = bytecode_emit_alloc(&bb, array_type, "array_alloc");

        Bytecode_Register array_val = {};

        for (s64 i = 0; i < array_type->static_array.count; i++) {
            auto elem_val = bytecode_integer_literal(&bb, &builtin_type_s64, i + 1);
            array_val = bytecode_emit_insert_element(&bb, array_val, elem_val, array_type, i);
        }
        bytecode_emit_store_alloc(&bb, array_val, array_alloc);

        auto sum = bytecode_integer_literal(&bb, &builtin_type_s64, 0);
        array_val = bytecode_emit_load_alloc(&bb, array_alloc);

        for (s64 i = array_type->static_array.count - 1; i >= 0; i--) {
            auto elem_val = bytecode_emit_extract_element(&bb, array_val, i);
            bytecode_emit_print(&bb, elem_val);
            sum = bytecode_emit_add(&bb, sum ,elem_val);
        }

        bytecode_emit_print(&bb, sum);

        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {


        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 15);

        assert_zodiac_stream(interp.std_out, "5\n4\n3\n2\n1\n15\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Simple_ARR_OFFSET_PTR(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto array_type = get_static_array_type(&builtin_type_s64, 5, &zc.ast_allocator);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto array_alloc = bytecode_emit_alloc(&bb, array_type, "array_alloc");

        Bytecode_Register array_val = {};

        for (s64 i = 0; i < array_type->static_array.count; i++) {
            auto elem_val = bytecode_integer_literal(&bb, &builtin_type_s64, i + 1);
            array_val = bytecode_emit_insert_element(&bb, array_val, elem_val, array_type, i);
        }
        bytecode_emit_store_alloc(&bb, array_val, array_alloc);

        auto sum = bytecode_integer_literal(&bb, &builtin_type_s64, 0);

        for (s64 i = array_type->static_array.count - 1; i >= 0; i--) {
            auto elem_ptr = bytecode_emit_array_offset_pointer(&bb, array_alloc, i);
            auto elem = bytecode_emit_load_pointer(&bb, elem_ptr);

            bytecode_emit_print(&bb, elem);
            sum = bytecode_emit_add(&bb, sum, elem);
        }

        auto array_ptr = bytecode_emit_address_of_alloc(&bb, array_alloc);

        for (s64 i = array_type->static_array.count - 1; i >= 0; i--) {
            auto elem_ptr = bytecode_emit_array_offset_pointer(&bb, array_ptr, i);
            auto elem = bytecode_emit_load_pointer(&bb, elem_ptr);

            bytecode_emit_print(&bb, elem);
            sum = bytecode_emit_add(&bb, sum, elem);
        }

        bytecode_emit_print(&bb, sum);

        bytecode_emit_return(&bb, sum);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 30);

        assert_zodiac_stream(interp.std_out, "5\n4\n3\n2\n1\n5\n4\n3\n2\n1\n30\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Calling_Function_Pointers(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *add_arg_types[] = { &builtin_type_s64, &builtin_type_s64 };
    auto add_fn_type = get_function_type(&builtin_type_s64, add_arg_types, &zc.ast_allocator);
    auto foreign_add_fn = bytecode_foreign_function_create(&bb, "foreign_add", add_fn_type);

    auto add_fn = bytecode_function_create(&bb, "add", add_fn_type);
    auto add_entry_block = bytecode_append_block(&bb, add_fn, "entry");
    bytecode_set_insert_point(&bb, add_fn, add_entry_block);
    {
        auto a = bytecode_emit_load_argument(&bb, 0);
        auto b = bytecode_emit_load_argument(&bb, 1);

        auto r = bytecode_emit_add(&bb, a, b);
        bytecode_emit_return(&bb, r);
    }

    Type *add32_arg_types[] = { &builtin_type_s32, &builtin_type_s32 };
    auto add32_fn_type = get_function_type(&builtin_type_s32, add32_arg_types, &zc.ast_allocator);
    auto add32_fn = bytecode_function_create(&bb, "add32", add32_fn_type);
    auto add32_entry_block = bytecode_append_block(&bb, add32_fn, "entry");
    bytecode_set_insert_point(&bb, add32_fn, add32_entry_block);
    {
        auto a = bytecode_emit_load_argument(&bb, 0);
        auto b = bytecode_emit_load_argument(&bb, 1);

        auto r = bytecode_emit_add(&bb, a, b);
        bytecode_emit_return(&bb, r);
    }

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto a = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto b = bytecode_integer_literal(&bb, &builtin_type_s64, 24);
        auto a32 = bytecode_integer_literal(&bb, &builtin_type_s32, 21);
        auto b32 = bytecode_integer_literal(&bb, &builtin_type_s32, 12);

        // Calling bytecode function
        bytecode_emit_push_arg(&bb, a);
        bytecode_emit_push_arg(&bb, b);
        auto r3 = bytecode_emit_call(&bb, add_fn, 2);
        bytecode_emit_print(&bb, r3);

        // Calling bytecode function trough pointer
        auto add_fn_ptr = bytecode_emit_address_of_function(&bb, add_fn);
        bytecode_emit_push_arg(&bb, a);
        bytecode_emit_push_arg(&bb, b);
        auto r4 = bytecode_emit_call_pointer(&bb, add_fn_ptr, 2);
        bytecode_emit_print(&bb, r4);

        // Calling foreign function trough ffi
        bytecode_emit_push_arg(&bb, a);
        bytecode_emit_push_arg(&bb, b);
        auto r = bytecode_emit_call(&bb, foreign_add_fn, 2);
        bytecode_emit_print(&bb, r);

         //Calling foreign function trough ffi via pointer
        auto foreign_add_fn_ptr = bytecode_emit_address_of_function(&bb, foreign_add_fn);
        bytecode_emit_push_arg(&bb, a);
        bytecode_emit_push_arg(&bb, b);
        auto r2 = bytecode_emit_call_pointer(&bb, foreign_add_fn_ptr, 2);
        bytecode_emit_print(&bb, r2);

        // Calling add32 trough bytecode
        bytecode_emit_push_arg(&bb, a32);
        bytecode_emit_push_arg(&bb, b32);
        auto r32 = bytecode_emit_call(&bb, add32_fn, 2);
        bytecode_emit_print(&bb, r32);

        // Calling add32 trough a pointer
        auto add32_fn_ptr = bytecode_emit_address_of_function(&bb, add32_fn);
        bytecode_emit_push_arg(&bb, a32);
        bytecode_emit_push_arg(&bb, b32);
        r32 = bytecode_emit_call_pointer(&bb, add32_fn_ptr, 2);
        bytecode_emit_print(&bb, r32);

        bytecode_emit_return(&bb, r);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 66);

        assert_zodiac_stream(interp.std_out, "66\n66\n66\n66\n33\n33\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}


file_local MunitResult BC_FN_PTR_Calls_With_Structs(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Type *vec2_mem_types[] = { &builtin_type_s64, &builtin_type_s64 };
    Type *vec2_type = get_struct_type(&zc, vec2_mem_types, "vec2", &zc.ast_allocator);
    Type *make_vec2_fn_type = get_function_type(vec2_type, vec2_mem_types, &zc.ast_allocator);
    auto make_vec2_fn = bytecode_function_create(&bb, "make_vec2", make_vec2_fn_type);
    auto make_vec2_entry_block = bytecode_append_block(&bb, make_vec2_fn, "entry");
    bytecode_set_insert_point(&bb, make_vec2_fn, make_vec2_entry_block);
    {
        auto a = bytecode_emit_load_argument(&bb, 0);
        auto b = bytecode_emit_load_argument(&bb, 1);

        auto result = bytecode_emit_insert_value(&bb, {}, a, vec2_type, 0);
        result = bytecode_emit_insert_value(&bb, result, b, vec2_type, 1);

        bytecode_emit_return(&bb, result);
    }

    Type *print_vec_arg_types[] = { vec2_type };
    Type *print_vec_fn_type = get_function_type(&builtin_type_void, print_vec_arg_types, &zc.ast_allocator);
    auto print_vec2_fn = bytecode_function_create(&bb, "print_vec2", print_vec_fn_type);
    auto print_vec2_entry_block = bytecode_append_block(&bb, print_vec2_fn, "entry");
    bytecode_set_insert_point(&bb, print_vec2_fn, print_vec2_entry_block);
    {
        auto vec = bytecode_emit_load_argument(&bb, 0);

        auto x = bytecode_emit_extract_value(&bb, vec, 0);
        auto y = bytecode_emit_extract_value(&bb, vec, 1);

        bytecode_emit_print(&bb, x);
        bytecode_emit_print(&bb, y);
        bytecode_emit_return(&bb);
    }

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto x = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto y = bytecode_integer_literal(&bb, &builtin_type_s64, 24);

        Bytecode_Register r = {};

        // Calling make_vec2 trough bytecode
        {
            bytecode_emit_push_arg(&bb, x);
            bytecode_emit_push_arg(&bb, y);

            auto v = bytecode_emit_call(&bb, make_vec2_fn, 2);

            bytecode_emit_push_arg(&bb, v);
            bytecode_emit_call(&bb, print_vec2_fn, 1);

            auto a = bytecode_emit_extract_value(&bb, v, 0);
            auto b = bytecode_emit_extract_value(&bb, v, 1);

            r = bytecode_emit_add(&bb, a, b);
            bytecode_emit_print(&bb, r);
        }

        // Calling make_vec2 trough pointer
        {
            bytecode_emit_push_arg(&bb, x);
            bytecode_emit_push_arg(&bb, y);

            auto make_vec2_fn_ptr = bytecode_emit_address_of_function(&bb, make_vec2_fn);

            auto v = bytecode_emit_call_pointer(&bb, make_vec2_fn_ptr, 2);

            auto print_vec2_fn_ptr = bytecode_emit_address_of_function(&bb, print_vec2_fn);

            bytecode_emit_push_arg(&bb, v);
            bytecode_emit_call_pointer(&bb, print_vec2_fn_ptr, 1);

            auto a = bytecode_emit_extract_value(&bb, v, 0);
            auto b = bytecode_emit_extract_value(&bb, v, 1);

            r = bytecode_emit_add(&bb, a, b);
            bytecode_emit_print(&bb, r);
         }

        assert(r.type && r.type == &builtin_type_s64);
        bytecode_emit_return(&bb, r);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        Interpreter interp = interpreter_create(c_alloc, &zc);
        filesystem_temp_file(&interp.std_out);
        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 66);

        assert_zodiac_stream(interp.std_out, "42\n24\n66\n42\n24\n66\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult BC_Callback_From_C(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    Interpreter interp = interpreter_create(c_alloc, &zc);
    filesystem_temp_file(&interp.std_out);

    auto void_ptr_ty = get_pointer_type(&builtin_type_void, &zc.ast_allocator);
    Type *runtime_set_stdout_arg_types[] = { void_ptr_ty };
    auto runtime_set_stdout_fn_type = get_function_type(&builtin_type_void, runtime_set_stdout_arg_types, &zc.ast_allocator);
    auto runtime_set_stdout_fn = bytecode_foreign_function_create(&bb, "runtime_set_stdout", runtime_set_stdout_fn_type);

    Type *add_arg_types[] = { &builtin_type_s64, &builtin_type_s64 };
    auto add_fn_type = get_function_type(&builtin_type_s64, add_arg_types, &zc.ast_allocator);
    auto foreign_add_fn = bytecode_foreign_function_create(&bb, "foreign_add", add_fn_type);

    auto add_fn = bytecode_function_create(&bb, "add", add_fn_type);
    auto add_entry_block = bytecode_append_block(&bb, add_fn, "entry");
    bytecode_set_insert_point(&bb, add_fn, add_entry_block);
    {
        auto a = bytecode_emit_load_argument(&bb, 0);
        auto b = bytecode_emit_load_argument(&bb, 1);

        auto r = bytecode_emit_add(&bb, a, b);
        bytecode_emit_return(&bb, r);
    }

    get_pointer_type(add_fn_type, &zc.ast_allocator);

    Type *call_binop_ptr_arg_types[] = { add_fn_type->pointer_to, &builtin_type_s64, &builtin_type_s64 };
    auto call_binop_ptr_type = get_function_type(&builtin_type_s64, call_binop_ptr_arg_types, &zc.ast_allocator);
    auto call_binop_ptr_fn = bytecode_foreign_function_create(&bb, "foreign_call_binop_ptr", call_binop_ptr_type);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        Bytecode_Register stdout_reg = {
            .kind = Bytecode_Register_Kind::TEMPORARY,
            .flags = BC_REGISTER_FLAG_LITERAL,
            .index = -1,
            .type = void_ptr_ty,
            .value = { .pointer = (uint8_t*)interp.std_out.handle }
        };
        bytecode_emit_push_arg(&bb, stdout_reg);
        bytecode_emit_call(&bb, runtime_set_stdout_fn, 1);

        auto a = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto b = bytecode_integer_literal(&bb, &builtin_type_s64, 24);

        // Passing and calling a foreign function to a c function
        auto foreign_add_addr = bytecode_emit_address_of_function(&bb, foreign_add_fn);
        bytecode_emit_push_arg(&bb, foreign_add_addr);
        bytecode_emit_push_arg(&bb, a);
        bytecode_emit_push_arg(&bb, b);
        auto r1 = bytecode_emit_call(&bb, call_binop_ptr_fn, 3);
        bytecode_emit_print(&bb, r1);

        // Passing and calling a bytecode function to a c function
        auto bc_add_addr = bytecode_emit_address_of_function(&bb, add_fn);
        bytecode_emit_push_arg(&bb, bc_add_addr);
        bytecode_emit_push_arg(&bb, a);
        bytecode_emit_push_arg(&bb, b);
        auto r2 = bytecode_emit_call(&bb, call_binop_ptr_fn, 3);
        bytecode_emit_print(&bb, r2);

        auto r = r2;
        bytecode_emit_return(&bb, r);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    MunitResult result = MUNIT_OK;

    if (!bytecode_valid) {
        bytecode_validator_print_errors(&validator);
        result = MUNIT_FAIL;

    } else {

        auto program = bytecode_get_program(&bb);
        program.entry_handle = main_fn;
        Interpreter_Register result_register = interpreter_start(&interp, program);
        munit_assert(result_register.type == &builtin_type_s64);
        munit_assert_int64(result_register.value.integer.s64, ==, 66);

        assert_zodiac_stream(interp.std_out, "Calling pointer!\n66\nCalling pointer!\n66\n");

        munit_assert(filesystem_close(&interp.std_out));

        interpreter_free(&interp);
    }

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return result;
}

file_local MunitResult Non_Return_Error_Simple(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto a = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto b = bytecode_integer_literal(&bb, &builtin_type_s64, 24);

        auto r = bytecode_emit_add(&bb, a, b);
        bytecode_emit_print(&bb, r);

        bytecode_emit_return(&bb, r);
    }

    auto return_bool_fn_type = get_function_type(&builtin_type_bool, {}, &zc.ast_allocator);
    auto return_bool = bytecode_function_create(&bb, "return_bool", return_bool_fn_type);
    auto return_bool_entry_block = bytecode_append_block(&bb, return_bool, "entry");
    bytecode_set_insert_point(&bb, return_bool, return_bool_entry_block);
    {
        auto b = bytecode_boolean_literal(&bb, &builtin_type_bool, true);
        bytecode_emit_return(&bb, b);
    }

    auto simple_fn_type = get_function_type(&builtin_type_void, {}, &zc.ast_allocator);
    auto simple_loop = bytecode_function_create(&bb, "simple_loop", simple_fn_type);
    auto simple_loop_entry_block = bytecode_append_block(&bb, simple_loop, "entry");
    auto simple_loop_b2 = bytecode_append_block(&bb, simple_loop, "b2");
    auto simple_loop_b3 = bytecode_append_block(&bb, simple_loop, "b3");
    {
        // This function will always keep jumping inside itself, so we expect an error.
        bytecode_set_insert_point(&bb, simple_loop, simple_loop_entry_block);
        bytecode_emit_jmp(&bb, simple_loop_b2);

        bytecode_set_insert_point(&bb, simple_loop, simple_loop_b2);
        bytecode_emit_jmp(&bb, simple_loop_b3);

        bytecode_set_insert_point(&bb, simple_loop, simple_loop_b3);
        bytecode_emit_jmp(&bb, simple_loop_b2);
    }

    auto simple2_loop = bytecode_function_create(&bb, "simple2_loop", simple_fn_type);
    auto simple2_loop_entry_block = bytecode_append_block(&bb, simple2_loop, "entry");
    auto simple2_loop_b2 = bytecode_append_block(&bb, simple2_loop, "b2");
    auto simple2_loop_b3 = bytecode_append_block(&bb, simple2_loop, "b3");
    auto simple2_return = bytecode_append_block(&bb, simple2_loop, "return");
    {
        // This function might return based on another function call, so expect no error
        bytecode_set_insert_point(&bb, simple2_loop, simple2_loop_entry_block);
        bytecode_emit_jmp(&bb, simple2_loop_b2);

        bytecode_set_insert_point(&bb, simple2_loop, simple2_loop_b2);
        bytecode_emit_jmp(&bb, simple2_loop_b3);

        bytecode_set_insert_point(&bb, simple2_loop, simple2_loop_b3);
        auto b = bytecode_emit_call(&bb, return_bool, 0);
        bytecode_emit_jmp_if(&bb, b, simple2_loop_b2, simple2_return);

        bytecode_set_insert_point(&bb, simple2_loop, simple2_return);
        bytecode_emit_return(&bb);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    munit_assert_false(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 1);

    Validation_Error &ve = validator.errors[0];
    auto err = &zc.errors[ve.error_handle];

    bytecode_validator_print_errors(&validator);

    const char *expected_err_msg = "Not all control paths return a value";
    munit_assert_int64(err->message.length, ==, (s64)strlen(expected_err_msg));
    munit_assert_string_equal(err->message.data, expected_err_msg);

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return MUNIT_OK;
}

file_local MunitResult Non_Return_Error_Indirect(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto a = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto b = bytecode_integer_literal(&bb, &builtin_type_s64, 24);

        auto r = bytecode_emit_add(&bb, a, b);
        bytecode_emit_print(&bb, r);

        bytecode_emit_return(&bb, r);
    }

    auto simple_fn_type = get_function_type(&builtin_type_void, {}, &zc.ast_allocator);
    auto indirect_loop = bytecode_function_create(&bb, "indirect_loop", simple_fn_type);
    auto indirect_loop_entry_block = bytecode_append_block(&bb, indirect_loop, "entry");
    auto indirect_loop_b2 = bytecode_append_block(&bb, indirect_loop, "b2");
    auto indirect_loop_b3 = bytecode_append_block(&bb, indirect_loop, "b3");
    auto indirect_loop_b4 = bytecode_append_block(&bb, indirect_loop, "b4");
    auto indirect_loop_b5 = bytecode_append_block(&bb, indirect_loop, "b5");
    auto indirect_loop_b6 = bytecode_append_block(&bb, indirect_loop, "b6");
    {
        bytecode_set_insert_point(&bb, indirect_loop, indirect_loop_entry_block);
        bytecode_emit_jmp(&bb, indirect_loop_b2);

        bytecode_set_insert_point(&bb, indirect_loop, indirect_loop_b2);
        bytecode_emit_jmp(&bb, indirect_loop_b3);

        bytecode_set_insert_point(&bb, indirect_loop, indirect_loop_b3);
        bytecode_emit_jmp(&bb, indirect_loop_b4);

        bytecode_set_insert_point(&bb, indirect_loop, indirect_loop_b4);
        bytecode_emit_jmp(&bb, indirect_loop_b5);

        bytecode_set_insert_point(&bb, indirect_loop, indirect_loop_b5);
        bytecode_emit_jmp(&bb, indirect_loop_b6);

        bytecode_set_insert_point(&bb, indirect_loop, indirect_loop_b6);
        bytecode_emit_jmp(&bb, indirect_loop_b2);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    bytecode_validator_print_errors(&validator);

    munit_assert_false(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 1);

    Validation_Error &ve = validator.errors[0];
    auto err = &zc.errors[ve.error_handle];

    auto expected_msg = "Not all control paths return a value";

    munit_assert_int64(err->message.length, ==, (s64)strlen(expected_msg));
    munit_assert_string_equal(err->message.data, expected_msg);

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return MUNIT_OK;
}

file_local MunitResult Non_Return_Flag(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto a = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
        auto b = bytecode_integer_literal(&bb, &builtin_type_s64, 24);

        auto r = bytecode_emit_add(&bb, a, b);
        bytecode_emit_print(&bb, r);

        bytecode_emit_return(&bb, r);
    }

    auto non_return_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto non_return_fn = bytecode_function_create(&bb, "non_return_fn", non_return_fn_type);
    auto non_return_entry_block = bytecode_append_block(&bb, non_return_fn, "entry");
    auto non_return_second_block = bytecode_append_block(&bb, non_return_fn, "second_block");
    bytecode_set_insert_point(&bb, non_return_fn, non_return_entry_block);
    {
        bytecode_emit_jmp(&bb, non_return_second_block);
    }
    bytecode_set_insert_point(&bb, non_return_fn, non_return_second_block);

    auto non_return2_fn = bytecode_function_create(&bb, "non_return2_fn", non_return_fn_type, BC_FUNCTION_FLAG_NORETURN);
    auto non_return2_entry_block = bytecode_append_block(&bb, non_return2_fn, "entry");
    auto non_return2_second_block = bytecode_append_block(&bb, non_return2_fn, "second_block");
    bytecode_set_insert_point(&bb, non_return2_fn, non_return2_entry_block);
    {
        bytecode_emit_jmp(&bb, non_return2_second_block);
    }
    bytecode_set_insert_point(&bb, non_return2_fn, non_return2_second_block);

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    bytecode_validator_print_errors(&validator);

    munit_assert_false(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 1);

    Validation_Error &ve = validator.errors[0];
    auto err = &zc.errors[ve.error_handle];

    auto expected_msg = "Not all control paths return a value";

    munit_assert_int64(err->message.length, ==, (s64)strlen(expected_msg));
    munit_assert_string_equal(err->message.data, expected_msg);

    munit_assert_int64(ve.instruction_handle.fn_index, ==, 1);
    munit_assert_int64(ve.instruction_handle.block_index, ==, 1);
    munit_assert_int64(ve.instruction_handle.instruction_index, ==, -1);

    bytecode_validator_free(&validator);
    bytecode_builder_free(&bb);
    zodiac_context_destroy(&zc);

    return MUNIT_OK;
}

file_local MunitResult Globals(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto global_var = bytecode_create_global(&bb, "global_var", &builtin_type_s64);
    auto global_var2 = bytecode_create_global(&bb, "global_var2", &builtin_type_s64, bytecode_integer_literal(&bb, &builtin_type_s64, 42));

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        bytecode_emit_store_global(&bb, bytecode_integer_literal(&bb, &builtin_type_s64, 24), global_var);
        auto global_val = bytecode_emit_load_global(&bb, global_var);
        bytecode_emit_print(&bb, global_val);

        auto global_val2 = bytecode_emit_load_global(&bb, global_var2);
        bytecode_emit_print(&bb, global_val2);


        bytecode_emit_return(&bb, global_val);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    bytecode_validator_print_errors(&validator);

    munit_assert(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 0);

    Interpreter interp = interpreter_create(c_alloc, &zc);
    filesystem_temp_file(&interp.std_out);
    auto program = bytecode_get_program(&bb);
    program.entry_handle = main_fn;
    Interpreter_Register result_register = interpreter_start(&interp, program);

    assert_zodiac_stream(interp.std_out, "24\n42\n");
    munit_assert(filesystem_close(&interp.std_out));

    munit_assert_ptr_equal(result_register.type, &builtin_type_s64);
    munit_assert_int64(result_register.value.integer.s64, ==, 24);

    interpreter_free(&interp);

    return MUNIT_OK;
}


file_local MunitResult Constants(const MunitParameter params[], void *user_data_or_fixture)
{
    auto c_alloc = c_allocator();

    Zodiac_Context zc;
    zodiac_context_create(&zc);
    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto global_const = bytecode_integer_literal(&bb, &builtin_type_s64, 42);
    munit_assert(global_const.flags & BC_REGISTER_FLAG_CONSTANT);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, &zc.ast_allocator);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");
    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        bytecode_emit_print(&bb, global_const);
        bytecode_emit_return(&bb, global_const);
    }

    print_bytecode(bb);

    Bytecode_Validator validator = {};
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    bytecode_validator_print_errors(&validator);

    munit_assert(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 0);

    Interpreter interp = interpreter_create(c_alloc, &zc);
    filesystem_temp_file(&interp.std_out);
    auto program = bytecode_get_program(&bb);
    program.entry_handle = main_fn;
    Interpreter_Register result_register = interpreter_start(&interp, program);

    assert_zodiac_stream(interp.std_out, "42\n");
    munit_assert(filesystem_close(&interp.std_out));

    munit_assert_ptr_equal(result_register.type, &builtin_type_s64);
    munit_assert_int64(result_register.value.integer.s64, ==, 42);

    interpreter_free(&interp);

    return MUNIT_OK;
}

file_local MunitResult String_Literals(const MunitParameter params[], void *user_data_or_fixture)
{
    Zodiac_Context zc;
    zodiac_context_create(&zc);

    auto c_alloc = c_allocator();
    auto aa = &zc.ast_allocator;

    Bytecode_Builder bb = bytecode_builder_create(c_alloc, &zc);

    auto main_fn_type = get_function_type(&builtin_type_s64, {}, aa);
    auto main_fn = bytecode_function_create(&bb, "main", main_fn_type);
    auto main_entry_block = bytecode_append_block(&bb, main_fn, "entry");


    bytecode_set_insert_point(&bb, main_fn, main_entry_block);
    {
        auto str_lit_val = bytecode_string_literal(&bb, "Hello, Zodiac!");
        bytecode_emit_print(&bb, str_lit_val);
        auto return_val = bytecode_integer_literal(&bb, &builtin_type_s64, 0);
        bytecode_emit_return(&bb, return_val);
    }

    print_bytecode(bb);

    Bytecode_Validator validator;
    bytecode_validator_init(&zc, c_allocator(), &validator, bb.functions, nullptr);
    bool bytecode_valid = validate_bytecode(&validator);

    bytecode_validator_print_errors(&validator);

    munit_assert(bytecode_valid);
    munit_assert_int64(validator.errors.count, ==, 0);

    Interpreter interp = interpreter_create(c_alloc, &zc);
    filesystem_temp_file(&interp.std_out);
    auto program = bytecode_get_program(&bb);
    program.entry_handle = main_fn;
    Interpreter_Register result_register = interpreter_start(&interp, program);

    assert_zodiac_stream(interp.std_out, "Hello, Zodiac!\n");
    munit_assert(filesystem_close(&interp.std_out));

    munit_assert_ptr_equal(result_register.type, &builtin_type_s64);
    munit_assert_int64(result_register.value.integer.s64, ==, 0);

    interpreter_free(&interp);

    return MUNIT_OK;
}

START_TESTS(bytecode_tests)
    DEFINE_TEST(Building_1),
    DEFINE_TEST(Simple_Function_Call),
    DEFINE_TEST(Arguments_And_Return_Values),
    DEFINE_TEST(Recursion_And_Jumps),
    DEFINE_TEST(Insert_And_Extract_Value),
    DEFINE_TEST(Extract_Struct_Value),
    DEFINE_TEST(Return_Struct),
    DEFINE_TEST(Struct_Arguments),
    DEFINE_TEST(Basic_Pointers),
    DEFINE_TEST(Struct_Pointers),
    DEFINE_TEST(Invalid_Extract_Element),
    DEFINE_TEST(Simple_AGG_OFFSET_PTR),
    DEFINE_TEST(Nested_AGG_OFFSET_PTR),
    DEFINE_TEST(Insert_And_Extract_Element),
    DEFINE_TEST(Simple_ARR_OFFSET_PTR),
    DEFINE_TEST(Calling_Function_Pointers),
    DEFINE_TEST(BC_FN_PTR_Calls_With_Structs),
    DEFINE_TEST(BC_Callback_From_C),
    DEFINE_TEST(Non_Return_Error_Simple),
    DEFINE_TEST(Non_Return_Error_Indirect),
    DEFINE_TEST(Non_Return_Flag),
    DEFINE_TEST(Globals),
    DEFINE_TEST(Constants),
    DEFINE_TEST(String_Literals),
END_TESTS()

} }
