#pragma once

#include "util/zstring.h"
#include "test_common.h"
#include "CL/test_compiler.h"

#include <munit/munit.h>

namespace Zodiac { namespace Bug_Tests {

using namespace Compiler_Tests;

static MunitResult bool_to_string_Bool_Arg_LLVM(const MunitParameter params[], void *user_data_or_fixture)
{
    // This bug only happens on Linux. The register containing the boolean argument
    //  might have a value larger than 0xFF. This overflow happens with values
    //  originating from GEP instructions when the boolean is in the middle
    //  of a struct.
    //
    // The new default way of printing booleans is actually better than
    //  calling this foreign function, but it is a problem that should
    //  be fixed anyway.
    String_Ref code_string = R"CODE_STR(
        S :: struct {
            // _u64 : u64;
            // _s64 : s64;
            // _u32 : u32;
            // _s32 : s32;
            // _u16 : u16;
            // _s16 : s16;
            // _u8  :  u8;
            // _s8  :  s8;

            // _r64 : r64;
            // _r32 : r32;

            _bool1 : bool;
            _bool2 : bool;
        }
        main :: () -> s64 {
            print(true);
            print(false);
            // s : S = { 1, 2, 3, 4, 5, 6, 7, 8, 1.1, 2.2, false, true };
            s : S = { false, true };
            print(s._bool1);
            print(s._bool2);
            return 0;
        }
    )CODE_STR";

    // The default behaviour should work fine
    {
        Expected_Results expected = { .std_out = "true\nfalse\nfalse\ntrue", };
        Zodiac_Options options = {};

        auto result = compile_and_run(code_string, expected, options);
        defer { free_compile_run_results(&result); };

        munit_assert(result.result == MUNIT_OK);
    }

    // Note that the third print is incorrect in the runtime stdout
    Expected_Results expected = { .compiletime_std_out = "true\nfalse\nfalse\ntrue",
                                  .runtime_std_out     = "true\nfalse\ntrue\ntrue" };
    Zodiac_Options options = {};

    // We use this to call the support lib functions, for now it's
    //  the easiest way to call a foreign function.
    options.use_bool_to_string_for_PRINT_in_llvm = true;
    auto result = compile_and_run(code_string, expected, options);
    defer { free_compile_run_results(&result); };

    if (result.result == MUNIT_OK) {
        // Means the bug isn't fixed yet
        return MUNIT_SKIP;
    }

    // Fail if it is magically fixed....
    return MUNIT_FAIL;
}


START_TESTS(bug_tests)
    DEFINE_TEST(bool_to_string_Bool_Arg_LLVM),
END_TESTS()

}}

