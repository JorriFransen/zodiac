#pragma once

#include <munit/munit.h>

#include "test_common.h"

#include "bytecode/bytecode.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "util/zstring.h"
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

    Array_Ref<Expected_Error> errors = {};
};

struct Compile_Run_Results
{
    MunitResult result;
    Bytecode_Program program;

    Zodiac_Context context;
};

void free_compile_run_results(Compile_Run_Results *r);
Compile_Run_Results compile_and_run(String_Ref code_str, Expected_Results expected_results, Zodiac_Options options = {});

#define COMPILER_TESTS \
    X(Return_0) \
    X(Return_1) \
    X(Infer_Void_Return) \
    X(Invalid_Return_Type) \
    X(Print) \
    X(Binop_Add_Int_Const) \
    X(Binop_Sub_Int_Const) \
    X(Binop_Mul_Int_Const) \
    X(Binop_Div_Int_Const) \
    X(Binop_Add_Int) \
    X(Binop_Sub_Int) \
    X(Binop_Mul_Int) \
    X(Binop_Div_Int) \
    X(Global_Constant_With_Typespec) \
    X(Global_Constant_Without_Typespec) \
    X(Modify_Global_Constant) \
    X(Global_Variable_TS) \
    X(Global_Variable_No_TS) \
    X(Global_Variable_Assign) \
    X(Local_Constant) \
    X(Modify_Local_Constant) \
    X(Local_Variable) \
    X(Modify_Local_Variable) \
    X(Args_And_Return_Val) \
    X(If_Statements) \
    X(Boolean_If_Statements) \
    X(Define_Struct_Type) \
    X(Struct_Offset_Ptr) \
    X(Nested_Struct_Offset_Ptr) \
    X(Static_Array_Basics) \
    X(Deref) \
    X(Global_Run_Directive_Return_Void) \
    X(Global_Run_Directive_Variable) \
    X(Global_Run_Directive_Constant) \
    X(Local_Run_Directives) \
    X(Run_Global_Var_Types) \
    X(Run_Global_Const_Types) \
    X(Run_Struct_Member_Types) \
    X(Run_Array_Element_Types) \
    X(Run_Const_Array_Element_Types) \
    X(Run_And_Pointer_To_Const) \
    X(Run_Expr_Const) \
    X(Run_Call_Arg_Const) \
    X(Run_Print_Arg_Const) \
    X(Run_Call_Arg_In_Block_Const) \
    X(Run_Print_Arg_In_Block_Const) \
    X(Run_Assignment_Is_Expression) \
    X(Run_Block_Only_Print_And_Call) \
    X(Run_Local_Unused) \
    X(Non_Constant_Compound) \
    X(Foreign_Function) \
    X(Strings) \
    X(While) \
    X(For) \
    X(Slice_Array_Locals) \
    X(Slice_Array_Globals) \
    X(Slice_Array_Extra) \
    X(Slice_Array_Arguments) \
    X(Slice_Aggregate_Index) \
    X(Slice_Lvalues) \
    X(Compound_Assignment) \
    X(Pointer_Equality) \
    X(Struct_Pointer_To_Self) \
    X(Spiderman_Struct) \
    X(More_Struct_Member_Pointers) \
    X(Defer_1) \
    X(Defer_2) \
    X(Defer_3) \
    X(Defer_4) \
    X(Defer_5) \
    X(Defer_6) \
    X(Defer_7) \
    X(Zero_Init_Locals) \
    X(Zero_Init_Globals) \
    X(Unsized_Int_To_Real) \
    X(Implicit_Pointer_To_Bool) \
    X(Implicit_Integer_To_Bool) \
    X(Enum_Implicit_Values) \
    X(Enum_Operations) \
    X(Enum_Mixed_Values) \
    X(Enum_Members_As_Values) \
    X(Switch_Int) \
    X(Switch_Int_Default) \
    X(Switch_Int_Falltrough) \
    X(Switch_Int_Falltrough_Last) \
    X(Switch_Int_Falltrough_Multi) \
    X(Switch_Int_Multi_Val) \
    X(Switch_Int_Multi_Val_Default) \
    X(Switch_Int_Multi_Val_Falltrough) \
    X(Switch_Int_Range) \
    X(Switch_Int_Range_Default) \
    X(Switch_Int_Range_Multi) \
    X(Switch_Enum_Incomplete) \
    X(Switch_Enum_Default) \
    X(Switch_Enum_Falltrough) \
    X(Switch_Enum_Falltrough_Last) \
    X(Switch_Enum_Falltrough_Multi) \
    X(Recurse_Self) \
    X(Recurse_Indirect) \
    X(Type_Info) \
    X(Type_Of) \
    X(Function_Pointers) \
    X(Any_Print) \
    X(Any_With_Storage) \
    X(Any_Without_Storage) \
    X(Vararg) \
    X(Vararg_Spreads) \
    X(Vararg_Spread_Illegal) \
    X(Break_Loop) \
    X(Break_Switch) \
    X(Break_Loop_Defer) \
    X(Break_Switch_Defer) \
    X(Continue_Loop) \
    X(Continue_Loop_Defer) \


// Function declarations
#define X(name) MunitResult name(const MunitParameter params[], void *user_data_or_fixture);
COMPILER_TESTS
#undef X

// Register the tests
#define X(name) DEFINE_TEST(name),
START_TESTS(compiler_tests)
    COMPILER_TESTS
END_TESTS()
#undef X

}}
