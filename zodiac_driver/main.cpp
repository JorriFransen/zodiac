#include "command_line_arguments.h"
#include "defines.h"
#include "memory/zmemory.h"
#include "util/logger.h"
#include "zodiac_context.h"

using namespace Zodiac;

int main(int argc, const char **argv) {

    if (!logging_system_initialize()) return 1;
    if (!memory_system_initialize()) return 1;

    Zodiac_Options options = {};
    parse_command_line_options(&options, argc, argv);

    Zodiac_Context c;
    zodiac_context_create(options, &c);

    if (!zodiac_context_compile(&c)) {
        return -1;
    }

    zodiac_context_destroy(&c);

    return 0;
}

