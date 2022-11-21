#include <test_common.h>

#include <containers/freelist.h>
#include <logger.h>
#include <memory/zmemory.h>

namespace Zodiac { namespace Freelist_Tests {

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = 512;
    u64 mem_required;

    freelist_create(total_size, &mem_required, &freelist, nullptr);
    munit_assert_uint64(mem_required, >, 0);

    void *memory = zallocate(mem_required);
    munit_assert_ptr_not_null(memory);

    freelist_create(total_size, &mem_required, &freelist, memory);


    munit_assert_uint64(freelist.total_size, ==, total_size);
    munit_assert_uint64(freelist.max_entries, >=, 20);
    munit_assert_ptr_not_null(freelist.head);
    munit_assert_ptr_not_null(freelist.nodes);
    munit_assert_ptr_equal(freelist.head, &freelist.nodes[0]);

    for (i64 i = 1; i < freelist.max_entries; i++) {
        munit_assert_uint64(freelist.nodes[i].offset, ==, ZODIAC_FREELIST_INVALID_OFFSET);
        munit_assert_uint64(freelist.nodes[i].size, ==, ZODIAC_FREELIST_INVALID_SIZE);
    }

    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size);

    freelist_destroy(&freelist);

    munit_assert_uint64(freelist.total_size, ==, 0);
    munit_assert_uint64(freelist.max_entries, ==, 0);
    munit_assert_ptr_null(freelist.nodes);
    munit_assert_ptr_null(freelist.head);

    zfree(memory);

    return MUNIT_OK;
}

static MunitResult Alloc_And_Free_Block(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = 512;
    u64 alloc_size = 64;
    u64 mem_required;

    {
        freelist_create(total_size, &mem_required, &freelist, nullptr);
        munit_assert_uint64(mem_required, >, 0);

        void *memory = zallocate(mem_required);
        munit_assert_ptr_not_null(memory);

        freelist_create(total_size, &mem_required, &freelist, memory);


        u64 offset = ZODIAC_FREELIST_INVALID_OFFSET;
        bool result = freelist_allocate_block(&freelist, alloc_size, &offset);

        munit_assert(result);
        munit_assert_uint64(offset, ==, 0);

        auto free_space = freelist_free_space(&freelist);
        munit_assert_uint64(free_space, ==, total_size - alloc_size);

        result = freelist_free_block(&freelist, alloc_size, offset);

        freelist_destroy(&freelist);
    }

    {
        freelist_create(total_size, &mem_required, &freelist, nullptr);
        munit_assert_uint64(mem_required, >, 0);

        void *memory = zallocate(mem_required);
        munit_assert_ptr_not_null(memory);

        freelist_create(total_size, &mem_required, &freelist, memory);

        u64 offset = ZODIAC_FREELIST_INVALID_OFFSET;
        bool result = freelist_allocate_block(&freelist, alloc_size, &offset);

        munit_assert(result);
        munit_assert_uint64(offset, ==, 0);

        auto free_space = freelist_free_space(&freelist);
        munit_assert_uint64(free_space, ==, total_size - alloc_size);

        result = freelist_free_block(&freelist, alloc_size, offset);

        freelist_destroy(&freelist);
    }

    return MUNIT_OK;
}

static MunitResult Alloc_And_Free_Multi(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = 512;
    u64 mem_required;

    freelist_create(total_size, &mem_required, &freelist, nullptr);
    munit_assert_uint64(mem_required, >, 0);

    void *memory = zallocate(mem_required);
    munit_assert_ptr_not_null(memory);

    freelist_create(total_size, &mem_required, &freelist, memory);



    auto alloc_size = 64;

    u64 offset = ZODIAC_FREELIST_INVALID_OFFSET;
    bool result = freelist_allocate_block(&freelist, alloc_size, &offset);

    munit_assert(result);
    munit_assert_uint64(offset, ==, 0);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - alloc_size);

    u64 offset2 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size, &offset2);

    munit_assert(result);
    munit_assert_uint64(offset2, ==, alloc_size);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size) * 2);


    u64 offset3 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size, &offset3);

    munit_assert(result);
    munit_assert_uint64(offset3, ==, alloc_size * 2);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size) * 3);


    result = freelist_free_block(&freelist, alloc_size, offset2);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size) * 2);

    u64 offset4 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size, &offset4);

    munit_assert(result);
    munit_assert_uint64(offset4, ==, offset2);
    munit_assert_uint64(offset4, ==, alloc_size);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size) * 3);


    result = freelist_free_block(&freelist, alloc_size, offset);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size) * 2);


    result = freelist_free_block(&freelist, alloc_size, offset3);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size));


    result = freelist_free_block(&freelist, alloc_size, offset4);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size);

    munit_assert_ptr_not_null(freelist.head);
    munit_assert_uint64(freelist.head->offset, ==, 0);
    munit_assert_uint64(freelist.head->size, ==, total_size);
    munit_assert_ptr_null(freelist.head->next);


    freelist_destroy(&freelist);

    zfree(memory);

    return MUNIT_OK;
}
static MunitResult Alloc_And_Free_Multi_Sized(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = 512;
    u64 mem_required;

    freelist_create(total_size, &mem_required, &freelist, nullptr);
    munit_assert_uint64(mem_required, >, 0);

    void *memory = zallocate(mem_required);
    munit_assert_ptr_not_null(memory);

    freelist_create(total_size, &mem_required, &freelist, memory);


    u64 alloc_size = 64;
    u64 offset = ZODIAC_FREELIST_INVALID_OFFSET;
    bool result = freelist_allocate_block(&freelist, alloc_size, &offset);

    munit_assert(result);
    munit_assert_uint64(offset, ==, 0);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - alloc_size);


    u64 alloc_size2 = 32;
    u64 offset2 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size2, &offset2);

    munit_assert(result);
    munit_assert_uint64(offset2, ==, alloc_size);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size + alloc_size2));


    u64 alloc_size3 = 64;
    u64 offset3 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size3, &offset3);

    munit_assert(result);
    munit_assert_uint64(offset3, ==, alloc_size + alloc_size2);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size + alloc_size2 + alloc_size3));


    // Free the middle block
    result = freelist_free_block(&freelist, alloc_size2, offset2);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size + alloc_size3));


    // This is bigger than the previously freed block
    u64 alloc_size4 = 64;
    u64 offset4 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size4, &offset4);

    munit_assert(result);
    munit_assert_uint64(offset4, ==, alloc_size + alloc_size2 + alloc_size3);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size + alloc_size3 + alloc_size4));


    result = freelist_free_block(&freelist, alloc_size, offset);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - (alloc_size3 + alloc_size4));


    result = freelist_free_block(&freelist, alloc_size3, offset3);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - alloc_size4);


    result = freelist_free_block(&freelist, alloc_size4, offset4);
    munit_assert(result);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size);


    munit_assert_ptr_not_null(freelist.head);
    munit_assert_uint64(freelist.head->offset, ==, 0);
    munit_assert_uint64(freelist.head->size, ==, total_size);
    munit_assert_ptr_null(freelist.head->next);


    freelist_destroy(&freelist);

    zfree(memory);

    return MUNIT_OK;
}

static MunitResult Alloc_Full(const MunitParameter params[], void *user_data_or_fixture)
{
    Freelist freelist;
    auto total_size = 512;
    u64 mem_required;

    freelist_create(total_size, &mem_required, &freelist, nullptr);
    munit_assert_uint64(mem_required, >, 0);

    void *memory = zallocate(mem_required);
    munit_assert_ptr_not_null(memory);

    freelist_create(total_size, &mem_required, &freelist, memory);

    u64 alloc_size = 512;
    u64 offset = ZODIAC_FREELIST_INVALID_OFFSET;
    bool result = freelist_allocate_block(&freelist, alloc_size, &offset);

    munit_assert(result);
    munit_assert_uint64(offset, ==, 0);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - alloc_size);


    u64 alloc_size2 = 64;
    u64 offset2 = ZODIAC_FREELIST_INVALID_OFFSET;
    ZINFO("The following warning is intentional!..");
    result = freelist_allocate_block(&freelist, alloc_size2, &offset2);
    munit_assert_false(result);


    freelist_free_block(&freelist, alloc_size, offset);

    offset2 = ZODIAC_FREELIST_INVALID_OFFSET;
    result = freelist_allocate_block(&freelist, alloc_size2, &offset2);
    munit_assert(result);
    munit_assert_uint64(offset2, ==, 0);
    munit_assert_uint64(freelist_free_space(&freelist), ==, total_size - alloc_size2);

    freelist_destroy(&freelist);

    zfree(memory);

    return MUNIT_OK;
}

START_TESTS(freelist_tests)
    DEFINE_TEST(Create_And_Free),
    DEFINE_TEST(Alloc_And_Free_Block),
    DEFINE_TEST(Alloc_And_Free_Multi),
    DEFINE_TEST(Alloc_And_Free_Multi_Sized),
    DEFINE_TEST(Alloc_Full),
END_TESTS()

}}
