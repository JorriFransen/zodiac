#pragma once

#include <munit/munit.h>

#include "test_dynamic_array.h"
#include "test_freelist.h"

namespace Zodiac
{

static MunitSuite dynamic_array_suite = {
    (char *)"Dyn_Array/",
    Dynamic_Array_Tests::dynamic_array_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite freelist_suite = {
    (char *)"Freelist/",
    Freelist_Tests::freelist_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite containers_child_suites[] = {
    dynamic_array_suite,
    freelist_suite,
    {},
};

static MunitSuite containers_suite = {
    (char *)"CRT/",
    nullptr,
    containers_child_suites,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

}
