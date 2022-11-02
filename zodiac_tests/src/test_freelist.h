#include <test_common.h>

#include <containers/freelist.h>

namespace Zodiac { namespace Freelist_Tests {

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = KIBIBYTE(1);
    freelist_create(c_allocator(), total_size, &freelist);
    
    munit_assert_uint64(freelist.total_size, ==, total_size);
    munit_assert_uint64(freelist.max_entries, >=, 20);
    munit_assert_ptr_not_null(freelist.head);
    munit_assert_ptr_not_null(freelist.nodes);
    munit_assert_ptr_equal(freelist.head, &freelist.nodes[0]);
    munit_assert_ptr_equal(freelist.backing_allocator, c_allocator());

    for (i64 i = 1; i < freelist.max_entries; i++) {
        munit_assert_uint64(freelist.nodes[i].offset, ==, ZODIAC_FREELIST_INVALID_OFFSET);
        munit_assert_uint64(freelist.nodes[i].size, ==, ZODIAC_FREELIST_INVALID_SIZE);
    }

    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size);

    freelist_free(&freelist);

    munit_assert_uint64(freelist.total_size, ==, 0);
    munit_assert_uint64(freelist.max_entries, ==, 0);
    munit_assert_ptr_null(freelist.nodes);
    munit_assert_ptr_null(freelist.head);
    munit_assert_ptr_null(freelist.backing_allocator);

    return MUNIT_OK;
}

static MunitResult Allocate_And_Free_Block(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = 512;
    auto alloc_size = 64;
    freelist_create(c_allocator(), total_size, &freelist);

    u64 offset = ZODIAC_FREELIST_INVALID_OFFSET;
    bool result = freelist_allocate_block(&freelist, alloc_size, &offset);

    munit_assert(result);
    munit_assert_uint64(offset, ==, 0);

    auto free_space = freelist_free_space(&freelist);
    munit_assert_uint64(free_space, ==, total_size - alloc_size);

    result = freelist_free_block(&freelist, alloc_size, offset);

    freelist_free(&freelist);

    return MUNIT_OK;
}
 
START_TESTS(freelist_tests)
    DEFINE_TEST(Create_And_Free),
    DEFINE_TEST(Allocate_And_Free_Block),
END_TESTS()

}}