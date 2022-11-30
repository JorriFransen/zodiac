#include "zodiac_context.h"

#include <lexer.h>

namespace Zodiac
{

void zodiac_context_create(Allocator *expr_allocator, Zodiac_Context *out_context)
{
    assert(expr_allocator && out_context);

    atom_table_init(&out_context->atoms);

    out_context->expression_allocator = expr_allocator;

    zodiac_register_keywords(&out_context->atoms);
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    atom_table_free(&context->atoms);
}

}
