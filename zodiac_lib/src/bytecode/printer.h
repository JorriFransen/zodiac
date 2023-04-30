#pragma once

#include "bytecode/bytecode.h"
#include "util/string_builder.h"

namespace Zodiac { namespace Bytecode {

ZAPI void bytecode_print(Bytecode_Builder *builder, Allocator *allocator);
ZAPI void bytecode_print(Bytecode_Builder *builder, String_Builder *sb);
ZAPI void bytecode_print_global(Bytecode_Builder *builder, Bytecode_Function *fn, Bytecode_Global *glob, String_Builder *sb);
ZAPI void bytecode_print_function(Bytecode_Builder *builder, Bytecode_Function *function, String_Builder *sb);
ZAPI void bytecode_print_block(Bytecode_Builder *builder, Bytecode_Function *fn, Bytecode_Block *block, String_Builder *sb, int indent = 0);
ZAPI void bytecode_print_instruction(Bytecode_Builder *builder, Bytecode_Function *fn, Bytecode_Instruction *instruction, String_Builder *sb, int indent = 0);
ZAPI void bytecode_print_register(Bytecode_Builder *builder, Bytecode_Function *fn, const Bytecode_Register &reg, String_Builder *sb);
ZAPI void bytecode_print_indent(int indent, String_Builder *sb);

}}
