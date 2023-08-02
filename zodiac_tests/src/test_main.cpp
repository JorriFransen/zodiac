
#include <munit/munit.h>

#include "memory/zmemory.h"
#include "platform/filesystem.h"
#include "util/logger.h"

#include "BC/test_bytecode.h"
#include "CTR/test_containers.h"
#include "MEM/test_memory.h"
#include "CL/test_compiler.h"

#include "test_atoms.h"
#include "test_lexer.h"
#include "test_string_builder.h"
#include "test_strings.h"


namespace Zodiac
{

static MunitSuite string_suite = {
    (char *)"String/",
    String_Tests::string_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite atom_suite = {
    (char *)"Atom/",
    Atom_Tests::atom_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite string_builder_suite = {
    (char *)"String_Builder/",
    String_Builder_Tests::string_builder_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite lexer_suite = {
    (char *)"Lex/",
    Lexer_Tests::lexer_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite bytecode_suite = {
    (char *)"BC/",
    Bytecode_Tests::bytecode_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite compiler_suite = {
    (char *)"CL/",
    Compiler_Tests::compiler_tests,
    nullptr,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

static MunitSuite main_child_suites[] = {
    string_suite,
    atom_suite,
    string_builder_suite,
    containers_suite,
    memory_suite,
    lexer_suite,
    bytecode_suite,
    compiler_suite,
    {},
};

static MunitSuite main_suite = {
    nullptr,
    nullptr,
    main_child_suites,
    1,
    MUNIT_SUITE_OPTION_NONE,
};

}

int main(int argc, char** argv) {

    if (!Zodiac::logging_system_initialize()) return 1;

    // Log nothing to stdout, this will mess up the test output.
    // Munit will display stderr when a test fails.
    Zodiac::File_Handle err_file;
    Zodiac::filesystem_stderr_file(&err_file);
    Zodiac::logging_system_set_stdout_file(err_file);

    if (!Zodiac::memory_system_initialize()) return 1;


    return munit_suite_main(&Zodiac::main_suite, nullptr, argc, argv);
}
