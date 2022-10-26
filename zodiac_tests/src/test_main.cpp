
#include <munit/munit.h>

#include "test_atoms.h"

namespace Zodiac
{

static MunitSuite atom_suite = {
    (char*)"Atom/",
    atom_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite main_child_suites[] = {
    atom_suite,
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

    return munit_suite_main(&Zodiac::main_suite, nullptr, argc, argv);
}
