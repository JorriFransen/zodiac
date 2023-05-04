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

// TODO: Hash table
struct Inst_Loc__
{
    Bytecode_Instruction_Handle inst_handle;
    Source_Pos pos;
};

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

    // TODO: Hash table
    Dynamic_Array<Inst_Loc__> *instruction_locations = nullptr;
    // Hash_Table<Bytecode_Instruction_Handle, Source_Pos> *instruction_locations = nullptr;
};

void bytecode_visitor_init(Allocator *allocator, Bytecode_Visitor *visitor, void *user_data, const Array_Ref<Bytecode_Function> &functions, Dynamic_Array<Inst_Loc__> *instruction_locations);

void bytecode_visitor_free(Bytecode_Visitor *visitor);

bool visit_bytecode(Bytecode_Visitor *visitor);
bool visit_function(Bytecode_Visitor *visitor, Bytecode_Function_Handle fn_handle);
bool visit_block(Bytecode_Visitor *visitor, Bytecode_Block *block);

void visit_instruction_post(Bytecode_Visitor *visitor, Bytecode_Instruction *instruction);
}}
