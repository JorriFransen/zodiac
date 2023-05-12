#pragma once

#include "bytecode/bytecode.h"
#include "containers/stack.h"
#include "containers/dynamic_array.h"
#include "source_pos.h"

namespace Zodiac {

struct Allocator;

namespace Bytecode {

struct Bytecode_Visitor;

typedef bool (*Bytecode_Visit_Function_FN)(Bytecode_Visitor *visitor, Bytecode_Function_Handle fn_handle);
typedef bool (*Bytecode_Visit_Block_FN)(Bytecode_Visitor *visitor, Bytecode_Block *block);
typedef bool (*Bytecode_Visit_Instruction_FN)(Bytecode_Visitor *visitor, Bytecode_Instruction *instruction);

struct Bytecode_Visitor
{
    Allocator *allocator = nullptr;

    void *user_data = nullptr;

    Array_Ref<Bytecode_Function> functions = {};

    Bytecode_Function *current_function = nullptr;
    Bytecode_Instruction_Handle current_pos = {};

    Bytecode_Visit_Function_FN visit_function = nullptr;
    Bytecode_Visit_Block_FN visit_block = nullptr;
    Bytecode_Visit_Instruction_FN visit_instruction = nullptr;

    Stack<Bytecode_Register> arg_stack = {};

    Hash_Table<Bytecode_Instruction_Handle, Source_Pos> *instruction_locations = nullptr;
};

ZAPI void bytecode_visitor_init(Allocator *allocator, Bytecode_Visitor *visitor, void *user_data, const Array_Ref<Bytecode_Function> &functions, Hash_Table<Bytecode_Instruction_Handle, Source_Pos> *instruction_locations);

ZAPI void bytecode_visitor_free(Bytecode_Visitor *visitor);

ZAPI bool visit_bytecode(Bytecode_Visitor *visitor);
ZAPI bool visit_function(Bytecode_Visitor *visitor, Bytecode_Function_Handle fn_handle);
ZAPI bool visit_block(Bytecode_Visitor *visitor, Bytecode_Block *block);

ZAPI void visit_instruction_post(Bytecode_Visitor *visitor, Bytecode_Instruction *instruction);
}}
