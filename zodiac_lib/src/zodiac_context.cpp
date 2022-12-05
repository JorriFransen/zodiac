#include "zodiac_context.h"

#include <lexer.h>

namespace Zodiac
{

void zodiac_context_create(Allocator *ast_allocator, Zodiac_Context *out_context)
{
    assert(ast_allocator && out_context);

    atom_table_init(&out_context->atoms);

    out_context->ast_allocator = ast_allocator;

    temporary_allocator_create(KIBIBYTE(16), nullptr, &out_context->temp_allocator_state);
    out_context->temp_allocator = temporary_allocator_allocator(&out_context->temp_allocator_state);

    zodiac_register_keywords(&out_context->atoms);
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    atom_table_free(&context->atoms);
}

}
