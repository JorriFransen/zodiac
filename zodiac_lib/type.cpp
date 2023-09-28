#include "type.h"

#include "ast.h"
#include "memory/allocator.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "scope.h"
#include "util/asserts.h"
#include "util/string_builder.h"
#include "zodiac_context.h"

#include "resolve.h"

namespace Zodiac
{

// Builtin type atoms
#define ZODIAC_NUMERIC_TYPE_DEF(type, size) Atom atom_##type##size;
#define ZODIAC_NAME_TYPE_DEF(name) Atom atom_##name;
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_NAME_TYPE_DEF
#undef ZODIAC_NUMERIC_TYPE_DEF

bool type_system_initialized = false;

Type builtin_type_unsized_integer;
Type builtin_type_void;
Type builtin_type_bool;

Type builtin_type_u64;
Type builtin_type_s64;
Type builtin_type_u32;
Type builtin_type_s32;
Type builtin_type_u16;
Type builtin_type_s16;
Type builtin_type_u8;
Type builtin_type_s8;

Type builtin_type_r64;
Type builtin_type_r32;

s64 pointer_size;
Dynamic_Array<Type *> function_types;
Dynamic_Array<Type *> struct_types;
Dynamic_Array<Type *> static_array_types;
Dynamic_Array<Type *> slice_types;

bool type_system_initialize(Zodiac_Context *ctx)
{
    assert(ctx);
    assert(!type_system_initialized);

    auto at = &ctx->atoms;

    // Initialize atoms
#define ZODIAC_NUMERIC_TYPE_DEF(sign, size) atom_##sign##size = atom_get(at, #sign#size);
#define ZODIAC_NAME_TYPE_DEF(name) atom_##name = atom_get(at, #name);
ZODIAC_BUILTIN_TYPES
#undef ZODIAC_NAME_TYPE_DEF
#undef ZODIAC_NUMERIC_TYPE_DEF

    pointer_size = 64;

    dynamic_array_create(&dynamic_allocator, &function_types);
    dynamic_array_create(&dynamic_allocator, &struct_types);
    dynamic_array_create(&dynamic_allocator, &static_array_types);
    dynamic_array_create(&dynamic_allocator, &slice_types);

    create_type(&builtin_type_unsized_integer, Type_Kind::UNSIZED_INTEGER, 0, TYPE_FLAG_INT);
    create_type(&builtin_type_void, Type_Kind::VOID, 0);
    create_type(&builtin_type_bool, Type_Kind::BOOLEAN, 8);

    create_integer_type(&builtin_type_u64, 64, false);
    create_integer_type(&builtin_type_s64, 64, true);
    create_integer_type(&builtin_type_u32, 32, false);
    create_integer_type(&builtin_type_s32, 32, true);
    create_integer_type(&builtin_type_u16, 16, false);
    create_integer_type(&builtin_type_s16, 16, true);
    create_integer_type(&builtin_type_u8, 8, false);
    create_integer_type(&builtin_type_s8, 8, true);

    create_integer_type(&builtin_type_u8, 8, false);

    create_float_type(&builtin_type_r64, 64);
    create_float_type(&builtin_type_r32, 32);

    Dynamic_Array<Type *> string_member_types;
    dynamic_array_create(&ctx->ast_allocator, &string_member_types, 2);
    dynamic_array_append(&string_member_types, get_pointer_type(&builtin_type_u8, &ctx->ast_allocator));
    dynamic_array_append(&string_member_types, &builtin_type_s64);

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
    type->pointer_to = nullptr;
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

void create_pointer_type(Type *type, Type *base_type)
{
    assert(type);
    assert(base_type);
    assert(!base_type->pointer_to);

    create_type(type, Type_Kind::POINTER, pointer_size);

    type->pointer.base = base_type;

    base_type->pointer_to = type;
}

void create_struct_type(Type *type, Dynamic_Array<Type *> member_types, Atom name)
{
    assert(type);

    u64 bit_size = 0;
    for (u64 i = 0; i < member_types.count; i++) {
        //TODO: Alignment
        bit_size += member_types[i]->bit_size;
    }

    assert(bit_size % 8 == 0);

    create_type(type, Type_Kind::STRUCTURE, bit_size, TYPE_FLAG_AGGREGATE);

    type->structure.name = name;
    type->structure.member_types = member_types;
}

void create_static_array_type(Type *type, Type *element_type, u64 count)
{
    assert(type);
    assert(element_type);

    assert(element_type->bit_size > 0);
    assert(element_type->bit_size % 8 == 0);

    //TODO: Alignment
    auto bit_size = count * element_type->bit_size;
    assert(bit_size % 8 == 0);

    create_type(type, Type_Kind::STATIC_ARRAY, bit_size, TYPE_FLAG_NONE);

    type->static_array.element_type = element_type;
    type->static_array.count = count;
}

void create_slice_type(Type *type, Type *element_type, Type *struct_type)
{
    assert(struct_type->kind == Type_Kind::STRUCTURE);
    assert(struct_type->structure.member_types[0]->kind == Type_Kind::POINTER);
    assert(struct_type->structure.member_types[0]->pointer.base == element_type);



    create_type(type, Type_Kind::SLICE, struct_type->bit_size, TYPE_FLAG_NONE);

    type->slice.element_type = element_type;
    type->slice.struct_type = struct_type;
}

void create_function_type(Type *type, Type *return_type, Dynamic_Array<Type *> param_types, bool vararg/*false*/)
{
    assert(type);

    create_type(type, Type_Kind::FUNCTION, 64);

    type->function.return_type = return_type;
    type->function.parameter_types = param_types;
    type->function.is_vararg = vararg;
}

Type *get_pointer_type(Type *base, Allocator *allocator)
{
    assert(base);
    assert(allocator);

    if (base->pointer_to) return base->pointer_to;

    auto result = alloc<Type>(allocator);
    create_pointer_type(result, base);

    return result;
}

Type *get_struct_type(Zodiac_Context *zc, Array_Ref<Type *> member_types, const char *cstr_name, Allocator *allocator)
{
    assert(zc);
    assert(cstr_name);
    assert(allocator);

    Atom name_atom = atom_get(&zc->atoms, cstr_name);
    return get_struct_type(member_types, name_atom, allocator);
}

Type *get_struct_type(Array_Ref<Type *> member_types, Atom name, Allocator *allocator)
{
    assert(member_types.count);
    assert(allocator);

    for (s64 si = 0; si < struct_types.count; si++) {

        auto ex_type = struct_types[si];

        if (ex_type->structure.member_types.count != member_types.count) continue;
        if (ex_type->structure.name != name)  continue;

        bool member_match = true;
        for (s64 mi = 0; mi < ex_type->structure.member_types.count; mi++) {
            if (ex_type->structure.member_types[mi] != member_types[mi]) {
                member_match = false;
                break;
            }
        }

        if (member_match && ex_type->structure.name == name) {
            return ex_type;
        }
    }

    auto result = alloc<Type>(allocator);
    auto members_copy = dynamic_array_copy(member_types, allocator);

    create_struct_type(result, members_copy, name);

    dynamic_array_append(&struct_types, result);

    return result;
}

Type *finalize_struct_type(Type *unfinished, Array_Ref<Type *> member_types, Allocator *allocator)
{
    assert(unfinished->kind == Type_Kind::STRUCTURE);
    assert(unfinished->flags & TYPE_FLAG_UNFINISHED_STRUCT_TYPE);

    auto members_copy = dynamic_array_copy(member_types, allocator);

    auto pointer_to = unfinished->pointer_to;
    create_struct_type(unfinished, members_copy, unfinished->structure.name);
    unfinished->pointer_to = pointer_to;

    unfinished->flags &= ~TYPE_FLAG_UNFINISHED_STRUCT_TYPE;

    dynamic_array_append(&struct_types, unfinished);

    return unfinished;
}

Type *get_static_array_type(Type *element_type, u64 count, Allocator *allocator)
{
    assert(element_type);
    assert(allocator);

    for (u64 i = 0; i < static_array_types.count; i++) {

        auto sat = static_array_types[i];
        if (sat->static_array.element_type == element_type && sat->static_array.count == count) {
            return sat;
        }
    }

    Type *result = alloc<Type>(allocator);
    create_static_array_type(result, element_type, count);
    dynamic_array_append(&static_array_types, result);

    return result;
}

Type *get_slice_type(Zodiac_Context *ctx, Type *element_type, Allocator *allocator)
{
    for (u64 i = 0; i < slice_types.count; i++) {

        auto sat = slice_types[i];
        if (sat->slice.element_type == element_type) {
            return sat;
        }
    }

    Type *members[] = { get_pointer_type(element_type, allocator),
                        &builtin_type_s64 };

    auto mark = temporary_allocator_get_mark(temp_allocator());
    auto name_ = string_format(temp_allocator_allocator(), "slice.%s", temp_type_string(element_type).data);
    auto name = atom_get(&ctx->atoms, name_);
    temporary_allocator_reset(temp_allocator(), mark);

    Type *struct_type = get_struct_type(members, name, allocator);
    struct_type->flags |= TYPE_FLAG_SLICE_STRUCT;

    auto global_scope = ctx->resolver->global_scope;
    auto sym = add_typed_symbol(ctx, global_scope, Symbol_Kind::TYPE, SYM_FLAG_BUILTIN, name, nullptr);
    sym->aggregate.scope = scope_new(&ctx->ast_allocator, Scope_Kind::AGGREGATE, global_scope);
    sym->builtin_type = struct_type;

    Atom data_name = atom_get(&ctx->atoms, "data");
    auto data_sym = scope_add_symbol(ctx, sym->aggregate.scope, Symbol_Kind::MEMBER, Symbol_State::TYPED, SYM_FLAG_BUILTIN, data_name, nullptr);
    data_sym->builtin_type = members[0];

    Atom length_name = atom_get(&ctx->atoms, "length");
    auto length_sym = scope_add_symbol(ctx, sym->aggregate.scope, Symbol_Kind::MEMBER, Symbol_State::TYPED, SYM_FLAG_BUILTIN, length_name, nullptr);
    length_sym->builtin_type = members[1];

    Type *result = alloc<Type>(allocator);
    create_slice_type(result, element_type, struct_type);
    dynamic_array_append(&slice_types, result);

    return result;
}

Type *get_string_type(Zodiac_Context *ctx)
{
    if (ctx->builtin_string_type) {
        return ctx->builtin_string_type;
    }

    for (s64 i = 0; i < struct_types.count; i++) {
        auto st = struct_types[i];
        if (st->structure.name == atom_String) {
            auto u8_ptr_type = get_pointer_type(&builtin_type_u8, &ctx->ast_allocator);
            assert(st->structure.member_types[0] == u8_ptr_type);
            assert(st->structure.member_types[1] == &builtin_type_s64);

            ctx->builtin_string_type = st;
            return st;
        }
    }

    assert_msg(false, "Builting string type could not be found");
}

Type *get_function_type(Type *return_type, Array_Ref<Type *> parameter_types, Allocator *allocator, bool vararg/*=false*/)
{
    assert(return_type);
    assert(allocator);

    for (u64 i = 0; i < function_types.count; i++) {

        auto ex_type = function_types[i];
        if (ex_type->function.return_type != return_type) continue;
        if (ex_type->function.parameter_types.count != parameter_types.count) continue;
        if (ex_type->function.is_vararg != vararg) continue;

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
    create_function_type(result, return_type, params_copy, vararg);
    dynamic_array_append(&function_types, result);

    return result;
}

Type *sym_decl_type(Symbol *sym)
{
    assert(sym);
    assert(sym->decl);

    auto decl = sym->decl;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false);

        case AST_Declaration_Kind::VARIABLE:
        case AST_Declaration_Kind::CONSTANT_VARIABLE:
        case AST_Declaration_Kind::PARAMETER:
        case AST_Declaration_Kind::FIELD: {
            assert(decl->variable.resolved_type);

            if (decl->variable.resolved_type->kind == Type_Kind::UNSIZED_INTEGER) {
                return &builtin_type_s64;
            }

            return decl->variable.resolved_type;
        }

        case AST_Declaration_Kind::FUNCTION: {
            assert(decl->function.type && decl->function.type->kind == Type_Kind::FUNCTION);
            return decl->function.type;
        }

        case AST_Declaration_Kind::STRUCT:
        case AST_Declaration_Kind::UNION: {
            assert(decl->aggregate.resolved_type);
            return decl->aggregate.resolved_type;
            break;
        }

        case AST_Declaration_Kind::RUN_DIRECTIVE: assert(false); break;
        case AST_Declaration_Kind::IMPORT_DIRECTIVE: assert(false); break;
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

        case Type_Kind::INTEGER: {

            if (to->kind == Type_Kind::INTEGER) {
                if (from->integer.sign == to->integer.sign &&
                    from->bit_size < to->bit_size) {

                    return true;

                } else {
                    assert_msg(false, "Not implemented!");
                }

            } else if (to->kind == Type_Kind::POINTER) {
                assert(!from->integer.sign);
                assert(from->bit_size <= to->bit_size);
                return true;
            } else {
                assert_msg(false, "Not implemented!");
            }
            break;
        }

        case Type_Kind::FLOAT: return false;

        case Type_Kind::BOOLEAN: assert(false);

        case Type_Kind::POINTER: {
            return false;
            break;
        }

        case Type_Kind::STRUCTURE: return false;

        case Type_Kind::STATIC_ARRAY: {
            if (to->kind == Type_Kind::SLICE && from->static_array.element_type == to->static_array.element_type) return true;
            return false;
        }

        case Type_Kind::SLICE: assert(false);
        case Type_Kind::FUNCTION: assert(false);
    }

    assert(false);
    return false;
}

void type_to_string(Type *type, String_Builder *sb)
{
    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: {
            string_builder_append(sb, "void");
            break;
        }

        case Type_Kind::UNSIZED_INTEGER: {
            string_builder_append(sb, "UNSIZED_INTEGER");
            break;
        }

        case Type_Kind::INTEGER: {
            char sign_char = type->integer.sign ? 's' : 'u';
            string_builder_append(sb, "%c%d", sign_char, type->bit_size);
            break;
        }

        case Type_Kind::FLOAT: {
            string_builder_append(sb, "r%d", type->bit_size);
            break;
        }

        case Type_Kind::BOOLEAN: {
            string_builder_append(sb, "bool");
            break;
        }

        case Type_Kind::POINTER: {
            string_builder_append(sb, "*");
            type_to_string(type->pointer.base, sb);
            break;
        }

        case Type_Kind::STRUCTURE: {
            string_builder_append(sb, "%.*s", (int)type->structure.name.length, type->structure.name.data);
            break;
        }

        case Type_Kind::STATIC_ARRAY: {
            string_builder_append(sb, "[%lu]", type->static_array.count);
            type_to_string(type->static_array.element_type, sb);
            break;
        }

        case Type_Kind::SLICE: {
            string_builder_append(sb, "[]");
            type_to_string(type->static_array.element_type, sb);
            break;
        }

        case Type_Kind::FUNCTION: {
            string_builder_append(sb, "fn_type");
            break;
        }
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

String temp_type_string(Type *type)
{
    assert(type);

    return type_to_string(temp_allocator_allocator(), type);
}

}
