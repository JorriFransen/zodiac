#include "command_line_arguments.h"

#include "memory/allocator.h"
#include "platform/filesystem.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/zstring.h"
#include "zodiac_context.h"

#include <cxxopts.hpp>

#include <memory>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>

namespace Zodiac
{

void parse_command_line_options(Zodiac_Options *opts, int argc, const char **argv)
{
    assert(opts);

    auto ca = c_allocator();

    cxxopts::Options cxxoptions("zodiac", "zodiac compiler");

#define STRDEF(v) cxxopts::value<std::string>()->default_value(v)
#define BDEF(v) cxxopts::value<bool>()->default_value(#v)

    cxxoptions.add_options()
        ("input_file_name", "Input file name", STRDEF(""))
        ("o,output_file_name", "Output file name", STRDEF("a.out"))

        ("n,no_binary", "Do not emit binary", BDEF(false))

        ("a,print_ast", "Print ast", BDEF(false))
        ("p,print_bytecode", "Print generated bytecode", BDEF(false))
        ("l,print_llvm_ir", "Print generated llvm ir", BDEF(false))
        ("v,verbose", "Verbose output", BDEF(false))
        ("h,help", "Print help", BDEF(false))
        ;

#undef STRDEF
#undef BDEF

    cxxoptions.parse_positional({"input_file_name"});
    cxxoptions.positional_help("[input_file_name]");

    auto result = cxxoptions.parse(argc, argv);

    auto unmatched = result.unmatched();
    if (unmatched.size()) {
        printf("%s\n", cxxoptions.help().c_str());
        ZFATAL("Unmatched option: '%s'", unmatched.begin()->c_str());
    }

    if (result.count("help")) {
        printf("%s\n", cxxoptions.help().c_str());
        exit(0);
    }

    opts->input_file_name = string_copy(ca, result["input_file_name"].as<std::string>());
    opts->output_file_name = string_copy(ca, result["output_file_name"].as<std::string>());

    opts->dont_emit_binary = result["no_binary"].as<bool>();

    opts->print_ast = result["print_ast"].as<bool>();
    opts->print_bytecode = result["print_bytecode"].as<bool>();
    opts->print_llvm_ir = result["print_llvm_ir"].as<bool>();
    opts->verbose = result["verbose"].as<bool>();

    if (string_empty(opts->input_file_name)) {
        printf("%s\n", cxxoptions.help().c_str());
        ZFATAL("Input file required");
    }

    if (!filesystem_exists(opts->input_file_name)) {
        ZFATAL("Input file '%.*s' does not exist", (int)opts->input_file_name.length, opts->input_file_name.data);
    }

    if (opts->dont_emit_binary && opts->print_llvm_ir) {
        printf("%s\n", cxxoptions.help().c_str());
        ZFATAL("Conflicting options 'no_binary' and 'print_llvm_ir'");
    }

    if (opts->verbose) {
        logging_system_set_max_level(Log_Level::TRACE);
    }
}

}
