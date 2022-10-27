#include "llvm_test.h"

#include <stdio.h>

#include <llvm/Support/Host.h>

void llvm_test() {
    printf("llvm_test\n");

    auto tt = llvm::sys::getDefaultTargetTriple();

    printf("Default target triple: %s\n", tt.c_str());
}