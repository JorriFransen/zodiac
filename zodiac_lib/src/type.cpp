#include "type.h"

#include "ast.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"
#include "util/asserts.h"
#include "util/string_builder.h"

namespace Zodiac
{

bool type_system_initialized = false;

Type builtin_type_unsized_integer;
Type builtin_type_void;
Type builtin_type_boolean;
Type builtin_type_s64;
Type builtin_type_s32;

Dynamic_Array<Type *> function_types;

bool type_system_initialize()
{
    assert(!type_system_initialized);

    dynamic_array_create(&dynamic_allocator, &function_types);

    create_type(&builtin_type_unsized_integer, Type_Kind::UNSIZED_INTEGER, 0, TYPE_FLAG_INT);
    create_type(&builtin_type_void, Type_Kind::VOID, 0);
    create_type(&builtin_type_boolean, Type_Kind::BOOLEAN, 8);

    create_integer_type(&builtin_type_s64, 64, true);
    create_integer_type(&builtin_type_s32, 32, true);

    type_system_initialized = true;
    return true;
}

void create_type(Type *type, Type_Kind kind, u64 bit_size, Type_Flags flags/*=TYPE_FLAG_NONE*/)
{
    assert(type);
    assert(bit_size % 8 == 0);

    type->kind = kind;
    type->bit_size = bit_size;
    type->flags = flags;
}

void create_integer_type(Type *type, u64 bit_size, bool sign)
{
    assert(type);
    assert(bit_size % 8 == 0);

    create_type(type, Type_Kind::INTEGER, bit_size, TYPE_FLAG_INT);
    type->integer.sign = sign;
}

void create_float_type(Type *type, u64 bit_size)
{
    assert(type);
    assert(bit_size % 8 == 0);

    create_type(type, Type_Kind::FLOAT, bit_size);
}

void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types)
{
    assert(type);

    create_type(type, Type_Kind::FUNCTION, 64);

    type->function.return_type = return_type;
    type->function.parameter_types = param_types;
    type->function.is_vararg = false;
}

Type *get_function_type(Type *return_type, Array_Ref<Type *> parameter_types, Allocator *allocator)
{
    assert(return_type);
    assert(allocator);

    for (u64 i = 0; i < function_types.count; i++) {

        auto ex_type = function_types[i];
        if (ex_type->function.return_type != return_type) continue;
        if (ex_type->function.parameter_types.count != parameter_types.count) continue;

        bool param_match = true;
        for (u64 j = 0; j < parameter_types.count; j++) {
            if (ex_type->function.parameter_types[j] != parameter_types[j]) {
                param_match = false;
                break;
            }
        }

        if (param_match) return ex_type;
    }

    auto params_copy = dynamic_array_copy(parameter_types, allocator);

    Type *result = alloc<Type>(allocator);
    create_function_type(result, return_type, params_copy);

    return result;
}

Type *decl_type(AST_Declaration *decl)
{
    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);
        case AST_Declaration_Kind::VARIABLE: assert(false);

        case AST_Declaration_Kind::CONSTANT_VARIABLE: {
            assert(decl->variable.resolved_type);
            return decl->variable.resolved_type;
            break;
        }

        case AST_Declaration_Kind::FUNCTION: assert(false);
        case AST_Declaration_Kind::STRUCT: assert(false);
        case AST_Declaration_Kind::UNION: assert(false);
    }

    assert(false);
    return nullptr;
}

bool valid_static_type_conversion(Type *from, Type *to)
{
    assert(from);
    assert(to);

    if (from == to) {
        return true;
    }

    switch (from->kind) {

        case Type_Kind::INVALID: assert(false);
        case Type_Kind::VOID: assert(false);

        case Type_Kind::UNSIZED_INTEGER: {
            // TODO: Pass in the size of the literal, and take it into account
            if (to->kind == Type_Kind::INTEGER) return true;
            assert(false);
            break;
        }

        case Type_Kind::INTEGER: assert(false);
        case Type_Kind::FLOAT: assert(false);
        case Type_Kind::BOOLEAN: assert(false);
        case Type_Kind::POINTER: assert(false);
        case Type_Kind::STRUCTURE: assert(false);
        case Type_Kind::STATIC_ARRAY: assert(false);
        case Type_Kind::FUNCTION: assert(false);
    }

    assert(false);
    return false;
}

void type_to_string(Type *type, String_Builder *sb)
{
    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: string_builder_append(sb, "void"); break;

        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            char sign_char = type->integer.sign ? 's' : 'u';
            string_builder_append(sb, "%c%d", sign_char, type->bit_size);
            break;
        }

        case Type_Kind::FLOAT: assert(false); break;

        case Type_Kind::BOOLEAN: string_builder_append(sb, "bool"); break;

        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::STRUCTURE: assert(false); break;
        case Type_Kind::STATIC_ARRAY: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
    }
}

String type_to_string(Allocator *allocator, Type *type)
{
    String_Builder sb;
    string_builder_create(&sb, allocator);

    type_to_string(type, &sb);
    String result = string_builder_to_string(&sb);

    string_builder_destroy(&sb);

    return result;
}

}
