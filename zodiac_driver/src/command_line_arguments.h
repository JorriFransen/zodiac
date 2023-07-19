#pragma once

#define CXXOPTS_NO_EXCEPTIONS
#include <cxxopts.hpp>

namespace Zodiac
{

struct Zodiac_Options;

void parse_command_line_options(Zodiac_Options *opts, int argc, const char **argv);

}
