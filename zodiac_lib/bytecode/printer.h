#pragma once

#include "defines.h"

namespace Zodiac {

struct Allocator;
struct String_Builder;

namespace Bytecode {

struct Bytecode_Block;
struct Bytecode_Builder;
struct Bytecode_Function;
struct Bytecode_Global;
struct Bytecode_Instruction;
struct Bytecode_Register;

ZAPI void bytecode_print(const Bytecode_Builder *builder, Allocator *allocator);
ZAPI void bytecode_print(const Bytecode_Builder *builder, String_Builder *sb);
ZAPI void bytecode_print_global(const Bytecode_Builder *builder, Bytecode_Function *fn, const Bytecode_Global *glob, String_Builder *sb);
ZAPI void bytecode_print_function(const Bytecode_Builder *builder, const Bytecode_Function *function, String_Builder *sb);
ZAPI void bytecode_print_block(const Bytecode_Builder *builder, const Bytecode_Function *fn, const Bytecode_Block *block, String_Builder *sb, int indent = 0);
ZAPI void bytecode_print_instruction(const Bytecode_Builder *builder, const Bytecode_Function *fn, const Bytecode_Instruction *instruction, String_Builder *sb, int indent = 0);
ZAPI void bytecode_print_register(const Bytecode_Builder *builder, const Bytecode_Function *fn, const Bytecode_Register &reg, String_Builder *sb);
ZAPI void bytecode_print_indent(int indent, String_Builder *sb);

}}
