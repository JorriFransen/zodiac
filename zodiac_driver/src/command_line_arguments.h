#pragma once

#include "defines.h"

#include <argp.h>

#define ZODIAC_ARGP_PROGRAM_VERSION "zodiac " ZODIAC_VERSION
#define ZODIAC_ARGP_PROGRAM_DOC "Zodiac compiler"
#define ZODIAC_ARGP_ARGS_DOC "INFILE"

namespace Zodiac
{
struct Zodiac_Options;

error_t parse_opt(int key, char *arg, argp_state *state);
void parse_command_line_options(Zodiac_Options *opts, int argc, const char **argv);

}
