#include <stdio.h>

#include <dummy/dummy.h>

int main() {
    printf("add(1, 2) = %i\n", add(1, 2));
    do_llvm_stuff();

    return 0;
}   
