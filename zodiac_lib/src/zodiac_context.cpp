#include "zodiac_context.h"
#include "lexer.h"

namespace Zodiac
{

void zodiac_context_create(Zodiac_Context *out_context)
{
    assert(out_context);

    atom_table_init(&out_context->atoms);

    linear_allocator_create(MEBIBYTE(1), nullptr, &out_context->ast_allocator_state);
    out_context->ast_allocator = linear_allocator_allocator(&out_context->ast_allocator_state);

    temporary_allocator_create(KIBIBYTE(64), nullptr, &out_context->temp_allocator_state);
    out_context->temp_allocator = temporary_allocator_allocator(&out_context->temp_allocator_state);

    temporary_allocator_create(KIBIBYTE(8), nullptr, &out_context->resolve_error_allocator_state);
    out_context->resolve_error_allocator = temporary_allocator_allocator(&out_context->resolve_error_allocator_state);

    zodiac_register_keywords(&out_context->atoms);
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    atom_table_free(&context->atoms);
}

}
