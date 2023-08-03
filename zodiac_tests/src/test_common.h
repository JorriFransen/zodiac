#pragma once

#ifndef PRINT_BYTECODE_IN_TESTS
#define PRINT_BYTECODE_IN_TESTS 0
#endif // PRINT_BYTECODE_IN_TESTS

#ifndef BYTECODE_TESTS_VERBOSE
#define BYTECODE_TESTS_VERBOSE 1
#endif // BYTECODE_TESTS_VERBOSE

#if PRINT_BYTECODE_IN_TESTS
#include "bytecode/bytecode.h"
#endif // PRINT_BYTECODE_IN_TESTS

namespace Zodiac { namespace Bytecode {

struct Bytecode_Builder; 

void print_bytecode(const Bytecode_Builder *bb);

}}

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

#define assert_zodiac_stream(stream, expected_string) { \
    String es = {}; \
    if (expected_string.length) { \
        es = string_append(temp_allocator_allocator(), (expected_string), "\n"); \
    } \
    filesystem_flush(&stream); \
    u64 length; \
    filesystem_size(&stream, &length); \
    const auto _buf_size = 1024; \
    munit_assert((s64)_buf_size > length); \
    char _buf[_buf_size]; \
    u64 read_length; \
    bool read_res = filesystem_read(&stream, length, (u8 *)_buf, &read_length); \
    munit_assert_int(read_length, ==, length); \
    _buf[length] = '\0'; \
    munit_assert_int((int)strlen(_buf), ==, es.length); \
    if (read_length) munit_assert_string_equal(_buf, es.data); \
}

