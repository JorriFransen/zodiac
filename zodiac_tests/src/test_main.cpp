
#include <munit/munit.h>

#include "test_atoms.h"
#include "test_strings.h"
#include "test_dynamic_array.h"
#include "test_freelist.h"
#include "test_dynamic_allocator.h"

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
static MunitSuite main_child_suites[] = {
    string_suite,
    atom_suite,
    dynamic_array_suite,
    freelist_suite,
    dynamic_allocator_suite,
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

    Zodiac::memory_system_initialize();

    return munit_suite_main(&Zodiac::main_suite, nullptr, argc, argv);
}
