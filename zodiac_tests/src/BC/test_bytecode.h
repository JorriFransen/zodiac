#pragma once

#include <munit/munit.h>

#include "bytecode/bytecode.h"

#include "defines.h"
#include "test_common.h"
#include "util/zstring.h"

#ifndef PRINT_BYTECODE_IN_TESTS
#define PRINT_BYTECODE_IN_TESTS 0
#endif // PRINT_BYTECODE_IN_TESTS

#ifndef BYTECODE_TESTS_VERBOSE
#define BYTECODE_TESTS_VERBOSE 1
#endif // BYTECODE_TESTS_VERBOSE

namespace Zodiac {

struct Zodiac_Context;

namespace Bytecode_Tests {

using namespace Bytecode;

void print_bytecode(const Bytecode_Builder &bb);
void init_test_context(Zodiac_Context *zc);
MunitResult execute_and_verify(String_Ref out_file_name, s64 return_code=0, String_Ref stdout_str="");

MunitResult Building_1(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Simple_Function_Call(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Arguments_And_Return_Values(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Recursion_And_Jumps(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Insert_And_Extract_Value(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Extract_Struct_Value(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Return_Struct(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Struct_Arguments(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Basic_Pointers(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Struct_Pointers(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Invalid_Extract_Element(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Simple_AGG_OFFSET_PTR(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Nested_AGG_OFFSET_PTR(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Insert_And_Extract_Element(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Simple_ARR_OFFSET_PTR(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Calling_Function_Pointers(const MunitParameter params[], void *user_data_or_fixture);
MunitResult BC_FN_PTR_Calls_With_Structs(const MunitParameter params[], void *user_data_or_fixture);
MunitResult BC_Callback_From_C(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Non_Return_Error_Simple(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Non_Return_Error_Indirect(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Non_Return_Flag(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Globals(const MunitParameter params[], void *user_data_or_fixture);
MunitResult Constants(const MunitParameter params[], void *user_data_or_fixture);
MunitResult String_Literals(const MunitParameter params[], void *user_data_or_fixture);

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
