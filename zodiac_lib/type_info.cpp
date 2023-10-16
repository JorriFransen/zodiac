#include "type_info.h"

#include "containers/dynamic_array.h"
#include "memory/allocator.h"
#include "type.h"
#include "util/asserts.h"
#include "zodiac_context.h"

namespace Zodiac
{

Type_Info *add_type_info(Zodiac_Context *ctx, Type *type)
{
    if (type->info_index >= 0) {
        return ctx->type_infos[type->info_index];
    }

    auto allocator = &ctx->ast_allocator;

    Type_Info *result = nullptr;

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: {
            result = alloc<Type_Info>(allocator);
            init_type_info_base(result, Type_Info_Kind::VOID, type->bit_size);
            break;
        }

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

        case Type_Kind::BOOLEAN: {
            result = alloc<Type_Info>(allocator);
            init_type_info_base(result, Type_Info_Kind::BOOL, type->bit_size);
            break;
        }

        case Type_Kind::POINTER: {
            auto pointer_info = alloc<Type_Info_Pointer>(allocator);
            result = &pointer_info->base;

            init_type_info_base(result, Type_Info_Kind::POINTER, type->bit_size);
            pointer_info->pointer_to = add_type_info(ctx, type->pointer.base);
            break;
        }

        case Type_Kind::STRUCTURE: {
            auto structure_info = alloc<Type_Info_Struct>(allocator);
            result = &structure_info->base;

            init_type_info_base(result, Type_Info_Kind::STRUCT, type->bit_size);

            auto index = ctx->type_infos.count;
            dynamic_array_append(&ctx->type_infos, result);
            dynamic_array_append(&ctx->type_info_types, type);
            assert(type->info_index == -1);
            type->info_index = index;

            auto members = alloc_array<Type_Info_Struct_Member>(allocator, type->structure.member_types.count);

            s64 offset = 0;

            for (s64 i = 0; i < type->structure.member_types.count; i++) {
                members[i].name = string_copy(allocator, type->structure.member_names[i]);
                members[i].type = add_type_info(ctx, type->structure.member_types[i]);
                members[i].offset = offset;

                offset += members[i].type->byte_size;
            }

            structure_info->name = string_copy(&ctx->bytecode_allocator, type->structure.name);
            structure_info->members = members;
            structure_info->member_count = type->structure.member_types.count;
            break;
        }

        case Type_Kind::ENUM: {

            auto enum_info = alloc<Type_Info_Enum>(allocator);
            result = &enum_info->base;

            auto members = alloc_array<Type_Info_Enum_Member>(allocator, type->enumeration.members.count);

            for (s64 i = 0; i < type->enumeration.members.count; i++) {
                members[i].name = string_copy(allocator, type->enumeration.members[i].name);
                members[i].value  = type->enumeration.members[i].value;
            }

            init_type_info_base(result, Type_Info_Kind::ENUM, type->bit_size);
            enum_info->name = string_copy(&ctx->bytecode_allocator, type->enumeration.name);
            enum_info->integer_type = add_type_info(ctx, type->enumeration.integer_type);
            enum_info->members = members;
            enum_info->member_count = type->enumeration.members.count;
            break;
        }

        case Type_Kind::STATIC_ARRAY: {

            auto static_array_info = alloc<Type_Info_Static_Array>(allocator);
            result = &static_array_info->base;

            init_type_info_base(result, Type_Info_Kind::STATIC_ARRAY, type->bit_size);
            static_array_info->element_type = add_type_info(ctx, type->static_array.element_type);
            static_array_info->length = type->static_array.count;

            break;
        }

        case Type_Kind::SLICE: {
            auto slice_info = alloc<Type_Info_Slice>(allocator);
            result = &slice_info->base;

            init_type_info_base(result, Type_Info_Kind::SLICE, type->bit_size);
            slice_info->element_type = add_type_info(ctx, type->slice.element_type);
            break;
        }

        case Type_Kind::FUNCTION: {
            auto func_info = alloc<Type_Info_Function>(allocator);
            result = &func_info->base;

            init_type_info_base(result, Type_Info_Kind::FUNCTION, type->bit_size);

            auto param_count = type->function.parameter_types.count;

            Type_Info **params = nullptr;
            if (param_count) {
                params = alloc_array<Type_Info *>(allocator, param_count);

                for (s64 i = 0; i < param_count; i++) {
                    params[i] = add_type_info(ctx, type->function.parameter_types[i]);
                }
            }

            func_info->param_types = params;
            func_info->param_count = param_count;
            func_info->return_type = add_type_info(ctx, type->function.return_type);
        }
    }

    assert(result);

    if (type->kind != Type_Kind::STRUCTURE) {
        auto index = ctx->type_infos.count;
        dynamic_array_append(&ctx->type_infos, result);
        dynamic_array_append(&ctx->type_info_types, type);
        assert(type->info_index == -1);
        type->info_index = index;
    } else {
        assert(type->info_index >= 0);
    }

    return result;
}

void init_type_info_base(Type_Info *ti, Type_Info_Kind kind, s64 bit_size)
{
    assert(bit_size % 8 == 0);

    ti->kind = kind;
    ti->byte_size = bit_size / 8;
}

}
