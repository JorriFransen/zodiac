#pragma once

#include "defines.h"

namespace Zodiac { namespace Bytecode {

typedef s64 Bytecode_Function_Handle;
typedef s64 Bytecode_Global_Handle;

typedef s64 Bytecode_Block_Handle;
typedef s64 Bytecode_Phi_Args_Handle;
typedef s64 Bytecode_Global_Handle;

struct Bytecode_Switch_Handle
{
    s64 index;
};

struct Bytecode_Instruction_Handle
{
    Bytecode_Function_Handle fn_index = -1;
    Bytecode_Block_Handle block_index = -1;
    s64 instruction_index = -1;
};


} }
