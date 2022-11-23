
#include <munit/munit.h>

#include "test_atoms.h"
#include "test_strings.h"
#include "test_dynamic_array.h"
#include "test_freelist.h"
#include "test_dynamic_allocator.h"
#include "test_linear_allocator.h"
#include "test_pool_allocator.h"
#include "test_temp_allocator.h"
#include "test_dynamic_pool_allocator.h"

namespace Zodiac
{

static MunitSuite string_suite = {
    (char*)"String/",
    String_Tests::string_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite atom_suite = {
    (char*)"Atom/",
    Atom_Tests::atom_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite dynamic_array_suite = {
    (char*)"Dynamic_Array/",
    Dynamic_Array_Tests::dynamic_array_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite freelist_suite = {
    (char*)"Freelist/",
    Freelist_Tests::freelist_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite dynamic_allocator_suite = {
    (char*)"Dynamic_Alloc/",
    Dynamic_Allocator_Tests::dynamic_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite linear_allocator_suite = {
    (char*)"Linear_Alloc/",
    Linear_Allocator_Tests::linear_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite pool_allocator_suite = {
    (char*)"Pool_Alloc/",
    Pool_Allocator_Tests::pool_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite temporary_allocator_suite = {
    (char*)"Temp_Alloc/",
    Temp_Allocator_Tests::temp_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite dynamic_pool_allocator_suite = {
    (char*)"Dyn_Pool_Alloc/",
    Dynamic_Pool_Allocator_Tests::dynamic_pool_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite main_child_suites[] = {
    string_suite,
    atom_suite,
    dynamic_array_suite,
    freelist_suite,
    dynamic_allocator_suite,
    linear_allocator_suite,
    pool_allocator_suite,
    temporary_allocator_suite,
    dynamic_pool_allocator_suite,
    //bytecode_suite,
    {},
};

static MunitSuite main_suite = {
    nullptr,
    nullptr,
    main_child_suites,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

}

int main(int argc, char** argv) {

    if (!Zodiac::logging_system_initialize()) return 1;

    // Log nothting to stdout, this will mess up the test output.
    // Munit will display stderr when a test fails.
    Zodiac::File_Handle *err_file = Zodiac::filesystem_stderr_file();
    Zodiac::logging_system_set_stdout_file(err_file);

    if (!Zodiac::memory_system_initialize()) return 1;


    return munit_suite_main(&Zodiac::main_suite, nullptr, argc, argv);
}
