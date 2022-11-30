#pragma once

#include <atom.h>
#include <memory/allocator.h>

namespace Zodiac
{

struct Zodiac_Context
{
    Atom_Table atoms;
    Allocator *expression_allocator;
};

ZAPI void zodiac_context_create(Allocator *expression_allocator, Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

}
