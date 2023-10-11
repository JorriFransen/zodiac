#include "type_info.h"

#include "type.h"
#include "zodiac_context.h"

namespace Zodiac
{

void add_type_info(Zodiac_Context *ctx, Type *type)
{
    assert(type->info_index == -1);

    auto allocator = &ctx->ast_allocator;

    Type_Info *result = nullptr;

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;
        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            auto int_info = alloc<Type_Info_Int>(allocator);
            result = &int_info->base;

            init_type_info_base(result, Type_Info_Kind::INTEGER, type->bit_size);
            int_info->sign = type->integer.sign;
            break;
        }

        case Type_Kind::FLOAT: {
            result = alloc<Type_Info>(allocator);
            init_type_info_base(result, Type_Info_Kind::REAL, type->bit_size);
            break;
        }

        case Type_Kind::BOOLEAN: assert(false); break;
        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::STRUCTURE: assert(false); break;
        case Type_Kind::ENUM: assert(false); break;
        case Type_Kind::STATIC_ARRAY: assert(false); break;
        case Type_Kind::SLICE: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
    }

    auto index = ctx->type_infos.count;

    assert(result);
    dynamic_array_append(&ctx->type_infos, result);
    type->info_index = index;
}

void init_type_info_base(Type_Info *ti, Type_Info_Kind kind, s64 bit_size)
{
    assert(bit_size % 8 == 0);

    ti->kind = kind;
    ti->byte_size = bit_size / 8;
}

}
