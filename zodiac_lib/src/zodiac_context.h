#pragma once

#include <atom.h>
#include <memory/allocator.h>
#include <memory/temporary_allocator.h>

namespace Zodiac
{

struct Zodiac_Context
{
    Atom_Table atoms;

    Allocator *ast_allocator;

    Temporary_Allocator temp_allocator_state;
    Allocator temp_allocator;
};

ZAPI void zodiac_context_create(Allocator *ast_allocator, Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

}
