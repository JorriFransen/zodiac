#pragma once

#include "bytecode/bytecode.h"
#include "util/string_builder.h"

namespace Zodiac { namespace Bytecode {

ZAPI void bytecode_print(const Bytecode_Builder *builder, Allocator *allocator);
ZAPI void bytecode_print(const Bytecode_Builder *builder, String_Builder *sb);
ZAPI void bytecode_print_global(const Bytecode_Builder *builder, Bytecode_Function *fn, const Bytecode_Global *glob, String_Builder *sb);
ZAPI void bytecode_print_function(const Bytecode_Builder *builder, const Bytecode_Function *function, String_Builder *sb);
ZAPI void bytecode_print_block(const Bytecode_Builder *builder, const Bytecode_Function *fn, const Bytecode_Block *block, String_Builder *sb, int indent = 0);
ZAPI void bytecode_print_instruction(const Bytecode_Builder *builder, const Bytecode_Function *fn, const Bytecode_Instruction *instruction, String_Builder *sb, int indent = 0);
ZAPI void bytecode_print_register(const Bytecode_Builder *builder, const Bytecode_Function *fn, const Bytecode_Register &reg, String_Builder *sb);
ZAPI void bytecode_print_indent(int indent, String_Builder *sb);

}}
