#include "test_common.h"

#if PRINT_BYTECODE_IN_TESTS
#include "bytecode/printer.h"
#endif // PRINT_BYTECODE_IN_TESTS
 
namespace Zodiac { namespace Bytecode {

void print_bytecode(const Bytecode_Builder *bb) {

#if PRINT_BYTECODE_IN_TESTS

    printf("\n\n");

    // Assume this uses printf
    bytecode_print(bb, temp_allocator_allocator());
#endif // PRINT_BYTECODE_IN_TESTS

}

}}
