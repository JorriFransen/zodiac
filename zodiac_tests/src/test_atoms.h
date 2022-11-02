
#include <test_common.h>

#include <atom.h>

namespace Zodiac { namespace Atom_Tests {

static MunitResult Single_Match(const MunitParameter params[], void *user_data_or_fixture)
{
    auto allocator = c_allocator();

    Atom_Table at;
    atom_table_init(allocator, &at);

    auto cstr = "test string";
    Atom atom = atom_get(&at, cstr);
    Atom atom2 = atom_get(&at, cstr);

    munit_assert_true(atom == atom2);

    munit_assert(atom == atom_get(&at, "test string"));

    munit_assert_ptr_equal(atom.data, atom2.data);

    atom_table_free(&at);

    return MUNIT_OK;
}

static MunitResult Multiple_Match(const MunitParameter params[], void *user_data_or_fixture)
{
    auto allocator = c_allocator();

    Atom_Table at;
    atom_table_init(allocator, &at);

    Atom a1 = atom_get(&at, "a1");
    Atom a2 = atom_get(&at, "a2");
    Atom a3 = atom_get(&at, "a3");
    Atom a4 = atom_get(&at, "a4");
    Atom a5 = atom_get(&at, "a5");

    munit_assert_true(a1 == atom_get(&at, "a1"));
    munit_assert_true(a2 == atom_get(&at, "a2"));
    munit_assert_true(a3 == atom_get(&at, "a3"));
    munit_assert_true(a4 == atom_get(&at, "a4"));
    munit_assert_true(a5 == atom_get(&at, "a5"));

    munit_assert_false(a1 == a2);
    munit_assert_false(a1 == a3);
    munit_assert_false(a1 == a4);
    munit_assert_false(a1 == a5);

    munit_assert_false(a2 == a1);
    munit_assert_false(a2 == a3);
    munit_assert_false(a2 == a4);
    munit_assert_false(a2 == a5);

    munit_assert_false(a3 == a1);
    munit_assert_false(a3 == a2);
    munit_assert_false(a3 == a4);
    munit_assert_false(a3 == a5);

    munit_assert_false(a4 == a1);
    munit_assert_false(a4 == a2);
    munit_assert_false(a4 == a3);
    munit_assert_false(a4 == a5);

    munit_assert_false(a5 == a1);
    munit_assert_false(a5 == a2);
    munit_assert_false(a5 == a3);
    munit_assert_false(a5 == a4);

    Atom a11 = atom_get(&at, "a11");
    munit_assert_false(a1 == a11);

    Atom z1 = atom_get(&at, "z1");
    munit_assert_false(a1 == z1);

    atom_table_free(&at);

    return MUNIT_OK;
}

static MunitResult Growing(const MunitParameter params[], void *user_data_or_fixture)
{
    auto allocator = c_allocator();

    Atom_Table at;
    atom_table_init(allocator, &at, 2);

    munit_assert_int64(at.capacity, ==, 2);
    Atom a1 = atom_get(&at, "a1");
    Atom a2 = atom_get(&at, "a2");
    munit_assert_int64(at.capacity, ==, 2);
    Atom a3 = atom_get(&at, "a3");
    munit_assert_int64(at.capacity, ==, 4);
    Atom a4 = atom_get(&at, "a4");
    munit_assert_int64(at.capacity, ==, 4);
    Atom a5 = atom_get(&at, "a5");
    munit_assert_int64(at.capacity, ==, 8);

    munit_assert_true(a1 == atom_get(&at, "a1"));
    munit_assert_true(a2 == atom_get(&at, "a2"));
    munit_assert_true(a3 == atom_get(&at, "a3"));
    munit_assert_true(a4 == atom_get(&at, "a4"));
    munit_assert_true(a5 == atom_get(&at, "a5"));

    munit_assert_false(a1 == a2);
    munit_assert_false(a1 == a3);
    munit_assert_false(a1 == a4);
    munit_assert_false(a1 == a5);

    munit_assert_false(a2 == a1);
    munit_assert_false(a2 == a3);
    munit_assert_false(a2 == a4);
    munit_assert_false(a2 == a5);

    munit_assert_false(a3 == a1);
    munit_assert_false(a3 == a2);
    munit_assert_false(a3 == a4);
    munit_assert_false(a3 == a5);

    munit_assert_false(a4 == a1);
    munit_assert_false(a4 == a2);
    munit_assert_false(a4 == a3);
    munit_assert_false(a4 == a5);

    munit_assert_false(a5 == a1);
    munit_assert_false(a5 == a2);
    munit_assert_false(a5 == a3);
    munit_assert_false(a5 == a4);

    atom_table_free(&at);

    return MUNIT_OK;
}

START_TESTS(atom_tests)
    DEFINE_TEST(Single_Match),
    DEFINE_TEST(Multiple_Match),
    DEFINE_TEST(Growing),
END_TESTS()

}}

