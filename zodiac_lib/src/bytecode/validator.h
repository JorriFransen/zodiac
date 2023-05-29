#pragma once

#include "bytecode/bytecode.h"
#include "bytecode/visitor.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "error.h"
#include "util/zstring.h"

namespace Zodiac {

struct Allocator;
struct Zodiac_Context;
struct Source_Pos;

template <typename Key_Type, typename Value_Type>
struct Hash_Table;

namespace Bytecode {

struct Validation_Error
{
    Bytecode_Instruction_Handle instruction_handle = {};
    Error_Handle error_handle = -1;
};

struct Graph_Node
{
    Graph_Node *a = nullptr;
    Graph_Node *b = nullptr;

    Bytecode_Block *block = nullptr;

    bool returns = false;
};

struct Bytecode_Validator
{
    Zodiac_Context *context = nullptr;
    Allocator *allocator = nullptr;
    Bytecode_Visitor visitor = {};
    Dynamic_Array<Validation_Error> errors = {};
};

ZAPI void bytecode_validator_init(Zodiac_Context *context, Allocator *allocator, Bytecode_Validator *validator, Array_Ref<Bytecode_Function> functions, Hash_Table<Bytecode_Instruction_Handle, Source_Pos> *instruction_locations);

ZAPI void bytecode_validator_free(Bytecode_Validator *validator);

ZAPI void bytecode_validator_print_errors(Bytecode_Validator *validator);

ZAPI void bytecode_validator_report_error(Bytecode_Validator *validator, const char *fmt, ...);
ZAPI void bytecode_validator_report_error(Bytecode_Validator *validator, Bytecode_Instruction_Handle location, const char *fmt, ...);

ZAPI bool validate_bytecode(Bytecode_Validator *validator);

ZAPI bool validate_function(Bytecode_Visitor *visitor, Bytecode_Function_Handle fn_handle);
ZAPI bool validate_function(Bytecode_Validator *validator, Bytecode_Function_Handle fn_handle);

ZAPI bool validate_instruction(Bytecode_Visitor *visitor, Bytecode_Instruction *instruction);
ZAPI bool validate_instruction(Bytecode_Validator *validator, Bytecode_Instruction *instruction);

ZAPI Array_Ref<Graph_Node> validator_build_block_graph(Bytecode_Function *func);
ZAPI String block_graph_to_dot(Array_Ref<Graph_Node> nodes, Bytecode_Function *func, Allocator *allocator);
}}
