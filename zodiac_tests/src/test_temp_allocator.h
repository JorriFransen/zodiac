
#pragma once

#include <test_common.h>

#include <memory/temporary_allocator.h>

namespace Zodiac { namespace Temp_Allocator_Tests {


static MunitResult Create_Free(const MunitParameter params[], void *user_data_or_fixture)
{

    return MUNIT_OK;
}


START_TESTS(temp_allocator_tests)
    DEFINE_TEST(Create_Free),
END_TESTS()

}}
