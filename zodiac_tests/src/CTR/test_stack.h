#pragma once

#include "containers/stack.h"

#include "test_common.h"

#include <munit/munit.h>

namespace Zodiac { namespace Stack_Tests {

static MunitResult Create_And_Free(const MunitParameter params[], void *user_data_or_fixture)
{
    // Default capacity
    {
        Stack<s64> stack;
        stack_init(c_allocator(), &stack);

        munit_assert_ptr_not_null(stack.buffer);
        munit_assert_int(stack.sp, ==, 0);
        munit_assert_int(stack.capacity, ==, ZODIAC_STACK_DEFAULT_CAP);
        munit_assert_ptr_equal(stack.allocator, c_allocator());

        stack_free(&stack);
        munit_assert_ptr_null(stack.buffer);
        munit_assert_int(stack.sp, ==, -1);
        munit_assert_int(stack.capacity, ==, -1);
        munit_assert_ptr_null(stack.allocator);
    }

    // Custom capacity
    {
        const auto CAPACITY = 42;
        Stack<s64> stack;
        stack_init(c_allocator(), &stack, CAPACITY);

        munit_assert_ptr_not_null(stack.buffer);
        munit_assert_int(stack.sp, ==, 0);
        munit_assert_int(stack.capacity, ==, CAPACITY);
        munit_assert_ptr_equal(stack.allocator, c_allocator());

        stack_free(&stack);
        munit_assert_ptr_null(stack.buffer);
        munit_assert_int(stack.sp, ==, -1);
        munit_assert_int(stack.capacity, ==, -1);
        munit_assert_ptr_null(stack.allocator);
    }

    // Custom capacity
    {
        const auto CAPACITY = 0;
        Stack<s64> stack;
        stack_init(c_allocator(), &stack, CAPACITY);

        munit_assert_ptr_null(stack.buffer);
        munit_assert_int(stack.sp, ==, -1);
        munit_assert_int(stack.capacity, ==, CAPACITY);
        munit_assert_ptr_equal(stack.allocator, c_allocator());

        stack_free(&stack);
        munit_assert_ptr_null(stack.buffer);
        munit_assert_int(stack.sp, ==, -1);
        munit_assert_int(stack.capacity, ==, -1);
        munit_assert_ptr_null(stack.allocator);
    }


    return MUNIT_OK;
}

static MunitResult Push_Pop(const MunitParameter params[], void *user_data_or_fixture)
{
    Stack<int> stack;
    stack_init(c_allocator(), &stack, 4);

    stack_push(&stack, 1); // 1
    stack_push(&stack, 2); // 1, 2

    auto r1 = stack_pop(&stack); // 1
    munit_assert_int(2, ==, r1);

    stack_push(&stack, 3); // 1, 3
    stack_push(&stack, 4); // 1, 3, 4
    stack_push(&stack, 5); // 1, 3, 4, 5

    auto r2 = stack_pop(&stack); // 1, 3, 4
    auto r3 = stack_pop(&stack); // 1, 3
    auto r4 = stack_pop(&stack); // 1
    auto r5 = stack_pop(&stack); // <empty>

    munit_assert_int(5, ==, r2);
    munit_assert_int(4, ==, r3);
    munit_assert_int(3, ==, r4);
    munit_assert_int(1, ==, r5);

    stack_free(&stack);

    return MUNIT_OK;
}

static MunitResult Peek(const MunitParameter params[], void *user_data_or_fixture)
{
    Stack<int> stack;
    stack_init(c_allocator(), &stack, 4);

    stack_push(&stack, 1); // 1
    stack_push(&stack, 2); // 1, 2
    stack_push(&stack, 3); // 1, 2, 3
    stack_push(&stack, 4); // 1, 2, 3, 4

    munit_assert_int(1, ==, stack_peek(&stack, 3));
    munit_assert_int(2, ==, stack_peek(&stack, 2));
    munit_assert_int(3, ==, stack_peek(&stack, 1));
    munit_assert_int(4, ==, stack_peek(&stack, 0));

    stack_free(&stack);

    return MUNIT_OK;
}

static MunitResult Peek_Ptr(const MunitParameter params[], void *user_data_or_fixture)
{
    Stack<int> stack;
    stack_init(c_allocator(), &stack, 4);

    stack_push(&stack, 1); // 1
    stack_push(&stack, 2); // 1, 2
    stack_push(&stack, 3); // 1, 2, 3
    stack_push(&stack, 4); // 1, 2, 3, 4

    munit_assert_int(1, ==, *stack_peek_ptr(&stack, 3));
    munit_assert_int(2, ==, *stack_peek_ptr(&stack, 2));
    munit_assert_int(3, ==, *stack_peek_ptr(&stack, 1));
    munit_assert_int(4, ==, *stack_peek_ptr(&stack, 0));

    stack_free(&stack);

    return MUNIT_OK;
}

static MunitResult Top(const MunitParameter params[], void *user_data_or_fixture)
{
    Stack<int> stack;
    stack_init(c_allocator(), &stack, 4);

    stack_push(&stack, 1); // 1
    stack_push(&stack, 2); // 1, 2
    stack_push(&stack, 3); // 1, 2, 3
    stack_push(&stack, 4); // 1, 2, 3, 4

    munit_assert_int(4, ==, stack_top(&stack));
    stack_pop(&stack);
    munit_assert_int(3, ==, stack_top(&stack));
    stack_pop(&stack);
    munit_assert_int(2, ==, stack_top(&stack));
    stack_pop(&stack);
    munit_assert_int(1, ==, stack_top(&stack));
    stack_pop(&stack);

    stack_free(&stack);

    return MUNIT_OK;
}

static MunitResult Top_Ptr(const MunitParameter params[], void *user_data_or_fixture)
{
    Stack<int> stack;
    stack_init(c_allocator(), &stack, 4);

    stack_push(&stack, 1); // 1
    stack_push(&stack, 2); // 1, 2
    stack_push(&stack, 3); // 1, 2, 3
    stack_push(&stack, 4); // 1, 2, 3, 4

    munit_assert_int(4, ==, *stack_top_ptr(&stack));
    stack_pop(&stack);
    munit_assert_int(3, ==, *stack_top_ptr(&stack));
    stack_pop(&stack);
    munit_assert_int(2, ==, *stack_top_ptr(&stack));
    stack_pop(&stack);
    munit_assert_int(1, ==, *stack_top_ptr(&stack));
    stack_pop(&stack);

    stack_free(&stack);

    return MUNIT_OK;
}

static MunitResult Count(const MunitParameter params[], void *user_data_or_fixture)
{
    Stack<int> stack;
    stack_init(c_allocator(), &stack, 4);

    stack_push(&stack, 1); // 1
    munit_assert_int(1, ==, stack_count(&stack));
    stack_push(&stack, 2); // 1, 2
    munit_assert_int(2, ==, stack_count(&stack));
    stack_push(&stack, 3); // 1, 2, 3
    munit_assert_int(3, ==, stack_count(&stack));
    stack_push(&stack, 4); // 1, 2, 3, 4
    munit_assert_int(4, ==, stack_count(&stack));

    stack_free(&stack);

    return MUNIT_OK;
}

static MunitResult Grow(const MunitParameter params[], void *user_data_or_fixture)
{
    // Start with capacity = 0
    {
        Stack<int> stack;
        stack_init(c_allocator(), &stack, 0);

        munit_assert_int(stack.sp, ==, -1);
        munit_assert_int(stack.capacity, ==, 0);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 1);
        munit_assert_int(stack.capacity, ==, 1);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 2);
        munit_assert_int(stack.capacity, ==, 2);

        stack_pop(&stack);
        munit_assert_int(stack.sp, ==, 1);
        munit_assert_int(stack.capacity, ==, 2);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 2);
        munit_assert_int(stack.capacity, ==, 2);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 3);
        munit_assert_int(stack.capacity, ==, 4);

        stack_free(&stack);
    }

    // Start with capacity = 1
    {
        Stack<int> stack;
        stack_init(c_allocator(), &stack, 1);

        munit_assert_int(stack.sp, ==, 0);
        munit_assert_int(stack.capacity, ==, 1);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 1);
        munit_assert_int(stack.capacity, ==, 1);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 2);
        munit_assert_int(stack.capacity, ==, 2);

        stack_pop(&stack);
        munit_assert_int(stack.sp, ==, 1);
        munit_assert_int(stack.capacity, ==, 2);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 2);
        munit_assert_int(stack.capacity, ==, 2);

        stack_push(&stack, 42);
        munit_assert_int(stack.sp, ==, 3);
        munit_assert_int(stack.capacity, ==, 4);

        stack_free(&stack);
    }

    // Default capacity
    {
        Stack<int> stack;
        stack_init(c_allocator(), &stack);

        munit_assert_int(stack.sp, ==, 0);
        munit_assert_int(stack.capacity, ==, ZODIAC_STACK_DEFAULT_CAP);

        for (int i = 0; i < ZODIAC_STACK_DEFAULT_CAP; i++) {
            stack_push(&stack, i);
        }

        munit_assert_int(stack.sp, ==, 8);
        munit_assert_int(stack.capacity, ==, ZODIAC_STACK_DEFAULT_CAP);

        stack_push(&stack, 42);

        munit_assert_int(stack.sp, ==, 9);
        munit_assert_int(stack.capacity, ==, ZODIAC_STACK_DEFAULT_CAP * 2);

        munit_assert_int(42, ==, stack_pop(&stack));

        for (int i = ZODIAC_STACK_DEFAULT_CAP - 1; i >= 0; i--) {
            munit_assert_int(i, ==, stack_pop(&stack));
        }

        munit_assert_int(stack.sp, ==, 0);
        munit_assert_int(stack.capacity, ==, ZODIAC_STACK_DEFAULT_CAP * 2);

        stack_free(&stack);
    }

    return MUNIT_OK;
}

START_TESTS(stack_tests)
    DEFINE_TEST(Create_And_Free),
    DEFINE_TEST(Push_Pop),
    DEFINE_TEST(Peek),
    DEFINE_TEST(Peek_Ptr),
    DEFINE_TEST(Top),
    DEFINE_TEST(Top_Ptr),
    DEFINE_TEST(Count),
    DEFINE_TEST(Grow),
END_TESTS()
}}
