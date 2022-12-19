#include "asserts.h"
#include "lexer.h"
#include "logger.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "parser.h"
#include "platform/filesystem.h"
#include "resolve.h"
#include "zodiac_context.h"
#include "zstring.h"

using namespace Zodiac;

namespace Zodiac { struct AST_File; }

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    // TODO: CLEANUP: Used in the resolver
    ctx = &c;

    Lexer lexer;
    lexer_create(&c, &lexer);
    String stream = {};

    auto filename = "tests/test.zc";
    bool read_result = filesystem_read_entire_file(&dynamic_allocator, filename, &stream);
    assert(read_result);
    if (!read_result) {
        return 1;
    }

    lexer_init_stream(&lexer, stream, filename);

    Parser parser;
    parser_create(&c, &lexer, &parser);
    AST_File *file = parse_file(&parser);
    if (parser.error) return 1;;
    assert(file);

    // ast_print_file(file);

    resolve_test(&c, file);

    free(&dynamic_allocator, stream.data);

    return 0;
}

