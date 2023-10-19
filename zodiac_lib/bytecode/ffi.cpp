#include "ffi.h"

#include "bytecode/bytecode.h"
#include "common.h"
#include "containers/stack.h"
#include "interpreter.h"
#include "memory/temporary_allocator.h"
#include "type.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "util/string_builder.h"
#include "zodiac_context.h"

#include <dyncall_args.h>
#include <dyncall_callback.h>
#include <dyncall.h>
#include <dyncall_types.h>
#include <dyncall_value.h>
#include <dynload.h>

namespace Zodiac { namespace Bytecode {

FFI_Context ffi_create(Allocator *allocator, Zodiac_Context *zc, bool link_c, FFI_Callback_Handler callback_handler)
{
    FFI_Context result = {};

    result.allocator = allocator;

    dynamic_array_create(allocator, &result.libs);

    result.dc_vm = dcNewCallVM(4096);
    dcMode(result.dc_vm, DC_CALL_C_DEFAULT);
    dcReset(result.dc_vm);

    DLLib *this_exe_lib = dlLoadLibrary(nullptr);
    assert(this_exe_lib);
    dynamic_array_append(&result.libs, this_exe_lib);

#if ZPLATFORM_WINDOWS
    DLLib *kernel32_lib = dlLoadLibrary("kernel32.dll");
    assert(kernel32_lib);
    dynamic_array_append(&result.libs, kernel32_lib);

    DLLib *ucrtbase_lib = dlLoadLibrary("ucrtbase.dll");
    assert(ucrtbase_lib);
    dynamic_array_append(&result.libs, ucrtbase_lib);

    if (link_c) {
        DLLib* msvcrt_lib = dlLoadLibrary("msvcrt.dll");
        assert(msvcrt_lib);
        dynamic_array_append(&result.libs, msvcrt_lib);
    }

#endif


    DLLib *rt_support_lib = nullptr;

#ifdef ZPLATFORM_WINDOWS
    rt_support_lib = dlLoadLibrary(zc->support_dll_dynamic_path.data);
#else
    rt_support_lib = dlLoadLibrary(zc->support_lib_dynamic_path.data);
#endif
    assert(rt_support_lib);
    dynamic_array_append(&result.libs, rt_support_lib);

    hash_table_create(allocator, &result.callbacks);

    assert(callback_handler);
    result.callback_handler = callback_handler;

    return result;
}

void ffi_free(FFI_Context *ffi)
{
    for (s64 i = 0; i < ffi->libs.count; i++) {
        dlFreeLibrary(ffi->libs[i]);
    }

    dynamic_array_free(&ffi->libs);

    hash_table_free(&ffi->callbacks);

    dcFree(ffi->dc_vm);
}

FFI_Handle ffi_load_function(FFI_Context *ffi, const String_Ref &fn_name)
{
    DCpointer symbol = nullptr;
    for (s64 i = 0; i < ffi->libs.count; i++) {
        auto sym = dlFindSymbol(ffi->libs[i], fn_name.data);
        if (sym) {
            symbol = sym;
            break;
        }
    }

    if (symbol) {
        return symbol;
    }

    ZWARN("Did not find function '%s' in any loaded library", fn_name.data);
    return nullptr;
}

void ffi_call(FFI_Context *ffi, void *fn_ptr, void *return_val_ptr,
              Type *return_type)
{
    switch (return_type->kind) {

        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
        case Type_Kind::BOOLEAN: assert(false); break;
        case Type_Kind::STRUCTURE: assert(false); break;
        case Type_Kind::ENUM: assert(false); break;
        case Type_Kind::STATIC_ARRAY: assert(false); break;
        case Type_Kind::SLICE: assert(false); break;

        case Type_Kind::VOID: {
            assert(!return_val_ptr);
            dcCallVoid(ffi->dc_vm, fn_ptr);
            break;
        }

        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            if (return_type->integer.sign) {
                switch (return_type->bit_size) {
                    default: assert(false); break;
                    case 8: assert(false); break;
                    case 16: assert(false); break;

                    case 32: {
                        assert(sizeof(int) == 4);
                        assert(sizeof(DCint) == 4);
                        s32 result = dcCallInt(ffi->dc_vm, fn_ptr);
                        if (return_val_ptr) *(s32*)return_val_ptr = result;
                        break;
                    }

                    case 64: {
                        assert(sizeof(DClonglong) == 8);
                        s64 result = dcCallLongLong(ffi->dc_vm, fn_ptr);
                        if (return_val_ptr) *(s64*)return_val_ptr = result;
                        break;
                    }

                }
            } else {
                switch (return_type->bit_size) {
                    default: assert(false); break;
                    case 8: assert(false); break;
                    case 16: assert(false); break;

                    case 32: {
                        assert(sizeof(int) == 4);
                        assert(sizeof(DCint) == 4);
                        u32 result = dcCallInt(ffi->dc_vm, fn_ptr);
                        if (return_val_ptr) *(u32*)return_val_ptr = result;
                        break;
                    }

                    case 64: {
                        assert(sizeof(DClonglong) == 8);
                        u64 result = dcCallLongLong(ffi->dc_vm, fn_ptr);
                        if (return_val_ptr) *(u64*)return_val_ptr = result;
                        break;
                    }

                }
            }
            break;
        }

        case Type_Kind::FLOAT: {
            if (return_type->bit_size == 32) {
                float result = dcCallFloat(ffi->dc_vm, fn_ptr);
                if (return_val_ptr) *(float*)return_val_ptr = result;
            } else {
                double result = dcCallDouble(ffi->dc_vm, fn_ptr);
                if (return_val_ptr) *(double*)return_val_ptr = result;
            }
            break;
        }

        case Type_Kind::POINTER: {
            void *result = dcCallPointer(ffi->dc_vm, fn_ptr);
            if (return_val_ptr) *(void**)return_val_ptr = result;
            break;
        }
    }
}

void ffi_reset(FFI_Context *ffi)
{
    dcMode(ffi->dc_vm, DC_CALL_C_DEFAULT);
    dcReset(ffi->dc_vm);
}

void ffi_push_arg(FFI_Context *ffi, void *arg_ptr, Type *type)
{
    switch (type->kind) {
        default: assert(false); break;

        case Type_Kind::INTEGER: {
            switch (type->bit_size) {
                default: assert(false); break;
                case 32: {
                     assert(sizeof(DCint) == 4);
                     dcArgInt(ffi->dc_vm, *((DCint *)arg_ptr));
                     break;
                 }

                case 64: {
                     assert(sizeof(DClonglong) == 8);
                     dcArgLongLong(ffi->dc_vm, *((DClonglong *)arg_ptr));
                     break;
                 }
            }
            break;
        }

        case Type_Kind::POINTER:
        case Type_Kind::FUNCTION: {
            dcArgPointer(ffi->dc_vm, *(void**)arg_ptr);
            break;
        }

        case Type_Kind::FLOAT: {
            if (type->bit_size == 32) {
                dcArgFloat(ffi->dc_vm, *(float*)arg_ptr);
            } else {
                assert(type->bit_size == 64);
                dcArgDouble(ffi->dc_vm, *(double*)arg_ptr);
            }
            break;
        }
    }
}

FFI_Handle ffi_create_callback(FFI_Context *ffi, FFI_Function_User_Data func_data, Type *fn_type)
{
    assert(fn_type->kind == Type_Kind::FUNCTION);

#ifndef NDEBUG
    FFI_Handle ex_fn_callback = nullptr;
    bool found = hash_table_find_key(&ffi->callbacks, func_data.handle, &ex_fn_callback);
    assert(!found);

    assert(ex_fn_callback == nullptr);
#endif

    auto sig = ffi_dcb_func_sig(fn_type);
    DCCallback *callback = dcbNewCallback(sig.data, dcb_callback_handler, func_data.interp);

    hash_table_add(&ffi->callbacks, (FFI_Handle)callback, func_data.handle);

    return (FFI_Handle)callback;
}

s64 ffi_find_callback(FFI_Context *ffi, FFI_Handle ffi_handle)
{
    s64 handle = -1;
    bool found = hash_table_find(&ffi->callbacks, ffi_handle, &handle);
    if (found) return handle;

    return -1;
}


char ffi_dcb_type_sig_char(Type *type, bool is_arg/*= false*/)
{
    char result = 0;

    switch (type->kind) {

        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: result = 'v'; break;

        case Type_Kind::UNSIZED_INTEGER: assert(false); break;

        case Type_Kind::INTEGER: {
            if (type->integer.sign) {
                switch (type->bit_size) {
                    default: assert(false); break;
                    case 8:  result = 'c'; break;
                    case 16: result = 's'; break;
                    case 32: result = 'i'; break;
                    case 64: result = 'l'; break;
                }
            } else {
                switch (type->bit_size) {
                    default: assert(false); break;
                    case 8:  result = 'C'; break;
                    case 16: result = 'S'; break;
                    case 32: result = 'I'; break;
                    case 64: result = 'L'; break;
                }
            }

            break;
        }

        case Type_Kind::FLOAT: {
            if (type == &builtin_type_r32) {
                result = 'f';
            } else if (type == &builtin_type_r64) {
                result = 'd';
            } else { assert(false); }
            break;
        }

        case Type_Kind::POINTER: {
            if (type->pointer.base == &builtin_type_u8) {
                result = 'Z';
            } else {
                result = 'p';
            }
            break;
        }

        case Type_Kind::FUNCTION: assert(false); break;
        case Type_Kind::BOOLEAN: assert(false); break;

        case Type_Kind::STRUCTURE: {
            // We can't actually do this with dyncall, so make it void for return types,
            // pointers for arguments
            if (is_arg) {
                result = 'p';
            } else {
                result = 'v';
            }
            break;
        }

        case Type_Kind::ENUM: assert(false); break;
        case Type_Kind::STATIC_ARRAY: assert(false); break;
        case Type_Kind::SLICE: assert(false); break;
    }

    return result;
}

String ffi_dcb_func_sig(Type *func_type)
{
    assert(func_type->kind == Type_Kind::FUNCTION);

    auto ta = temp_allocator();
    auto taa = temp_allocator_allocator();
    temporary_allocator_reset(ta);

    String_Builder _sb; auto sb = &_sb;
    string_builder_create(sb, taa);

    for (s64 i = 0; i < func_type->function.parameter_types.count; i++) {
        auto param_type = func_type->function.parameter_types[i];

        char c = ffi_dcb_type_sig_char(param_type, true);
        assert(c != 'v' && "[FFI] We do not support void parameters (probably from struct param)");

        string_builder_append(sb, "%c", c);
    }

    string_builder_append(sb, ")");

    char c = ffi_dcb_type_sig_char(func_type->function.return_type);
    string_builder_append(sb, "%c", c);

    return string_builder_to_string(sb);
}

char dcb_callback_handler(DCCallback *cb, DCArgs *args, DCValue *result, void *userdata)
{
    assert(userdata);
    auto interp = static_cast<Interpreter *>(userdata);

    s64 fn_handle = -1;
    bool found = hash_table_find(&interp->ffi.callbacks, (FFI_Handle)cb, &fn_handle);
    assert(found);

    assert(fn_handle >= 0 && fn_handle < interp->functions.count);
    assert_msg(found, "Did not find dcb callback handler");

    auto bc_func = &interp->functions[fn_handle];

    auto &param_types = bc_func->type->function.parameter_types;

    for (s64 i = 0; i < param_types.count; i++) {
        auto param_type = param_types[i];

        Interpreter_Register arg_reg = {
            .type = param_type,
        };

        switch(param_type->kind) {
            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::VOID: assert(false); break;

            case Type_Kind::UNSIZED_INTEGER: assert(false); break;

            case Type_Kind::INTEGER: {
                switch (param_type->bit_size) {
                    default: assert(false); break;
                    case 8: {
                        auto arg_val = dcbArgUChar(args);
                        assert(sizeof(arg_val) == (param_type->bit_size) / 8);
                        arg_reg.value.integer.u8 = arg_val;
                        break;
                    }
                    case 16: {
                        auto arg_val = dcbArgUShort(args);
                        assert(sizeof(arg_val) == (param_type->bit_size) / 8);
                        arg_reg.value.integer.u16 = arg_val;
                        break;
                    }
                    case 32: {
                        auto arg_val = dcbArgInt(args);
                        assert(sizeof(arg_val) == (param_type->bit_size) / 8);
                        arg_reg.value.integer.u32 = arg_val;
                        break;
                    }

                    case 64: {
                        auto arg_val = dcbArgULongLong(args);
                        assert(sizeof(arg_val) == (param_type->bit_size) / 8);
                        arg_reg.value.integer.u64 = arg_val;
                        break;
                    }
                }

                break;
            }

            case Type_Kind::FLOAT: assert(false); break;
            case Type_Kind::POINTER: assert(false); break;
            case Type_Kind::FUNCTION: assert(false); break;
            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::STRUCTURE: assert(false); break;
            case Type_Kind::ENUM: assert(false); break;
            case Type_Kind::STATIC_ARRAY: assert(false); break;
            case Type_Kind::SLICE: assert(false); break;
        }

        assert(arg_reg.type);

        stack_push(&interp->arg_stack, arg_reg);
    }

    assert(interp->ffi.callback_handler);
    Interpreter_Register *return_value_reg = interp->ffi.callback_handler(interp, fn_handle);

    auto return_type = bc_func->type->function.return_type;

    if (return_value_reg) {
        stack_pop(&interp->frames);
        interp->used_register_count -= 1;

        switch (return_type->kind) {
            case Type_Kind::INVALID: assert(false); break;
            case Type_Kind::VOID: assert(false); break;

            case Type_Kind::UNSIZED_INTEGER: assert(false); break;

            case Type_Kind::INTEGER: {
                switch (return_type->bit_size) {
                    default: assert(false); break;
                    case 8: {
                        assert(sizeof(result->C) == sizeof(return_value_reg->value.integer.u8));
                        result->C = return_value_reg->value.integer.u8;
                        return 'C';
                        break;
                    }
                    case 16: {
                        assert(sizeof(result->S) == sizeof(return_value_reg->value.integer.u16));
                        result->S = return_value_reg->value.integer.u16;
                        return 'S';
                        break;
                    }
                    case 32: {
                        assert(sizeof(result->I) == sizeof(return_value_reg->value.integer.u32));
                        result->I = return_value_reg->value.integer.u32;
                        return 'I';
                        break;
                    }
                    case 64: {
                        assert(sizeof(result->L) == sizeof(return_value_reg->value.integer.u64));
                        result->L = return_value_reg->value.integer.u64;
                        return 'L';
                        break;
                    }
                }
                break;
            }

            case Type_Kind::FLOAT: assert(false); break;
            case Type_Kind::POINTER: assert(false); break;
            case Type_Kind::FUNCTION: assert(false); break;
            case Type_Kind::BOOLEAN: assert(false); break;
            case Type_Kind::STRUCTURE: assert(false); break;
            case Type_Kind::ENUM: assert(false); break;
            case Type_Kind::STATIC_ARRAY: assert(false); break;
            case Type_Kind::SLICE: assert(false); break;
        }
    }

    assert(false);
    return 'v';
}

}}
