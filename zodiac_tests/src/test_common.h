#pragma once


#define START_TESTS(name) \
    static MunitTest (name)[] = {

#define DEFINE_NAMED_TEST(name, fn) { \
    (char*)(name), \
    (fn), \
    nullptr, nullptr, \
    MUNIT_TEST_OPTION_NONE, \
    nullptr, \
}

#define DEFINE_TEST(name) DEFINE_NAMED_TEST(#name, name)

#define END_TESTS() \
    { nullptr, nullptr, nullptr, nullptr, MUNIT_TEST_OPTION_NONE, nullptr } \
    };
