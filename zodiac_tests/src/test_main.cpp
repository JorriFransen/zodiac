#include <stdio.h>

#include <munit/munit.h>


int main() {
    printf("test main\n");
    
    munit_assert_int(1, ==, 2);
    return 0;
}