
#include <stdio.h>

#include <allocator.h>
#include <atom.h>
#include <llvm_test.h>

using namespace Zodiac;

int main() {
    printf("Hello world!\n");

    auto ca = c_allocator();

    llvm_test();
    
    Atom_Table at;
    atom_table_init(c_allocator(), &at);
    
    auto match = atom_get(&at, "a") == atom_get(&at, "a");
        
    atom_table_free(&at);
    
    return 0;
}   
