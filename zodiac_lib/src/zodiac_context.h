#pragma once

#include <ast.h>
#include <lexer.h>
#include <atom.h>
#include <memory/allocator.h>
#include <memory/temporary_allocator.h>

namespace Zodiac
{

struct Zodiac_Context
{
    Atom_Table atoms;

    Linear_Allocator ast_allocator_state;
    Allocator ast_allocator;

    Temporary_Allocator temp_allocator_state;
    Allocator temp_allocator;

    Temporary_Allocator resolve_error_allocator_state;
    Allocator resolve_error_allocator;
};

ZAPI void zodiac_context_create(Zodiac_Context *out_context);
ZAPI void zodiac_context_destroy(Zodiac_Context *context);

}
