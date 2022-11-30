#pragma once

#include <atom.h>

namespace Zodiac
{

struct Zodiac_Context
{
    Atom_Table atoms;
};

ZAPI void zodiac_context_create(Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

}
