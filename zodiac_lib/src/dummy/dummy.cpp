
#include "dummy.h"

#include <stdio.h>
#include <llvm/Support/Host.h>

int add(int a, int b) {
    return a + b;
}

void do_llvm_stuff() {
    printf("Doing some llvm stuff...\n");
    
    auto _tt = llvm::sys::getDefaultTargetTriple();
    
    printf("Target triple: %s\n", _tt.c_str());
}