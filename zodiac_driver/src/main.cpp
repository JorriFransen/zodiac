
#include <stdio.h>

#include <allocator.h>
#include <llvm_test.h>

using namespace Zodiac;

int main() {
    printf("Hello world!\n");

    auto ca = c_allocator();

    llvm_test();

    return 0;
}   
