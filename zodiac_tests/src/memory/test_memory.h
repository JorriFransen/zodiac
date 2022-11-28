#pragma once

#include <munit/munit.h>

#include "test_dynamic_allocator.h"
#include "test_linear_allocator.h"
#include "test_pool_allocator.h"
#include "test_temp_allocator.h"
#include "test_stack_allocator.h"

namespace Zodiac
{

static MunitSuite dynamic_allocator_suite = {
    (char *)"Dyn_Alloc/",
    Dynamic_Allocator_Tests::dynamic_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite linear_allocator_suite = {
    (char *)"Lin_Alloc/",
    Linear_Allocator_Tests::linear_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite temporary_allocator_suite = {
    (char *)"Temp_Alloc/",
    Temp_Allocator_Tests::temp_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite pool_allocator_suite = {
    (char *)"Dyn_Pool_Alloc/",
    Pool_Allocator_Tests::pool_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite stack_allocator_suite = {
    (char *)"Stack_Alloc/",
    Stack_Allocator_Tests::stack_allocator_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite memory_child_suites[] = {
    dynamic_allocator_suite,
    linear_allocator_suite,
    pool_allocator_suite,
    temporary_allocator_suite,
    stack_allocator_suite,
};

static MunitSuite memory_suite = {
    (char *)"Mem/",
    nullptr,
    memory_child_suites,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

}
