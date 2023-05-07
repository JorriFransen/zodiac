#pragma once

#include "defines.h"
#include "util/zstring.h"

namespace Zodiac
{

struct Source_Pos
{
    String_Ref name;
    u64 line;
    u64 index_in_line;
};

struct Source_Range
{
    Source_Pos start;
    Source_Pos end;
};

}
