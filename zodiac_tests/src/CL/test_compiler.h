#pragma once

#include <munit/munit.h>

#include "test_common.h"

#include "bytecode/bytecode.h"
#include "error.h"
#include "zodiac_context.h"

namespace Zodiac { namespace Compiler_Tests {

using namespace Bytecode;

struct Expected_Error
{
    Zodiac_Error_Kind kind;
    bool fatal;
    String_Ref message;
};

struct Expected_Results
{
    s64 exit_code = 0;

    String_Ref std_out = {}; // If this is set, expect the same at compile/runtime
    String_Ref compiletime_std_out = {};
    String_Ref runtime_std_out = {};

    Array_Ref<Expected_Error> resolve_errors = {};
};

struct Compile_Run_Results
{
    MunitResult result;
    Bytecode_Program program;

    Bytecode_Builder builder;
    Zodiac_Context context;
};

void free_compile_run_results(Compile_Run_Results *r);
Compile_Run_Results compile_and_run(String_Ref code_str, Expected_Results expected_results);

MunitResult Return_0(const MunitParameter params[], void* user_data_or_fixture);

MunitResult Return_1(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Infer_Void_Return(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Invalid_Return_Type(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Print(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Add_Int_Const(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Sub_Int_Const(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Mul_Int_Const(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Div_Int_Const(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Add_Int(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Sub_Int(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Mul_Int(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Binop_Div_Int(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Constant_With_Typespec(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Constant_Without_Typespec(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Modify_Global_Constant(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Variable_TS(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Variable_No_TS(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Variable_Assign(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Local_Constant(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Modify_Local_Constant(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Local_Variable(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Modify_Local_Variable(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Args_And_Return_Val(const MunitParameter params[], void* user_data_or_fixture);
MunitResult If_Statements(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Boolean_If_Statements(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Define_Struct_Type(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Struct_Offset_Ptr(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Nested_Struct_Offset_Ptr(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Run_Directive_Return_Void(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Run_Directive_Variable(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Global_Run_Directive_Constant(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Local_Run_Directives(const MunitParameter params[], void* user_data_or_fixture);
MunitResult Run_Directive_Global_Var_Types(const MunitParameter params[], void* user_data_or_fixture);

START_TESTS(compiler_tests)
    DEFINE_TEST(Return_0),
    DEFINE_TEST(Return_1),
    DEFINE_TEST(Infer_Void_Return),
    DEFINE_TEST(Invalid_Return_Type),
    DEFINE_TEST(Print),
    DEFINE_TEST(Binop_Add_Int_Const),
    DEFINE_TEST(Binop_Sub_Int_Const),
    DEFINE_TEST(Binop_Mul_Int_Const),
    DEFINE_TEST(Binop_Div_Int_Const),
    DEFINE_TEST(Binop_Add_Int),
    DEFINE_TEST(Binop_Sub_Int),
    DEFINE_TEST(Binop_Mul_Int),
    DEFINE_TEST(Binop_Div_Int),
    DEFINE_TEST(Global_Constant_With_Typespec),
    DEFINE_TEST(Global_Constant_Without_Typespec),
    DEFINE_TEST(Modify_Global_Constant),
    DEFINE_TEST(Global_Variable_TS),
    DEFINE_TEST(Global_Variable_No_TS),
    DEFINE_TEST(Global_Variable_Assign),
    DEFINE_TEST(Local_Constant),
    DEFINE_TEST(Modify_Local_Constant),
    DEFINE_TEST(Local_Variable),
    DEFINE_TEST(Modify_Local_Variable),
    DEFINE_TEST(Args_And_Return_Val),
    DEFINE_TEST(If_Statements),
    DEFINE_TEST(Boolean_If_Statements),
    DEFINE_TEST(Define_Struct_Type),
    DEFINE_TEST(Struct_Offset_Ptr),
    DEFINE_TEST(Nested_Struct_Offset_Ptr),
    DEFINE_TEST(Global_Run_Directive_Return_Void),
    DEFINE_TEST(Global_Run_Directive_Variable),
    DEFINE_TEST(Global_Run_Directive_Constant),
    DEFINE_TEST(Local_Run_Directives),
    DEFINE_TEST(Run_Directive_Global_Var_Types),
END_TESTS()

}}
