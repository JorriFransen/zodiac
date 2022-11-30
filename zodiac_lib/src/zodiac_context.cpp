#include "zodiac_context.h"

namespace Zodiac
{

void zodiac_context_create(Zodiac_Context *out_context)
{
    atom_table_init(&out_context->atoms);
}

void zodiac_context_destroy(Zodiac_Context *context)
{
    atom_table_free(&context->atoms);
}

}
