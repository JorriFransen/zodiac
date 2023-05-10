#pragma once

#include "defines.h"
#include "containers/dynamic_array.h"
#include "util/zstring.h"

#include <dyncall_args.h>
#include <dyncall_callback.h>
#include <dyncall.h>
#include <dyncall_types.h>
#include <dyncall_value.h>
#include <dynload.h>

namespace Zodiac {

struct Allocator;
struct Type;
struct Zodiac_Context;

namespace Bytecode {

struct Interpreter;
struct Interpreter_Register;
typedef DCpointer FFI_Handle;
typedef Interpreter_Register * (*FFI_Callback_Handler)(Interpreter *interp, s64 fn_handle);

struct FFI_Callback
{
    FFI_Handle handle;
    s64 bc_handle;
};

struct FFI_Context
{
    Allocator *allocator = nullptr;

    DCCallVM *dc_vm = nullptr;

    Dynamic_Array<DLLib *> libs = {};

    // TODO: Hash table
    // Hash_Table<FFI_Handle, s64> callbacks = {};

    Dynamic_Array<FFI_Callback> callbacks;
    FFI_Callback_Handler callback_handler = nullptr;
};

struct FFI_Function_User_Data
{
    Interpreter *interp = nullptr;
    s64 handle = -1;
};

FFI_Context ffi_create(Allocator *allocator, Zodiac_Context *zc, bool link_c, FFI_Callback_Handler callback_handler);

void ffi_free(FFI_Context *ffi);

/////////////////////////////////////////////////////////////////
///// Symbol loading interface //////////////////////////////////
/////////////////////////////////////////////////////////////////
FFI_Handle ffi_load_function(FFI_Context *ffi, const String_Ref &fn_name);

/////////////////////////////////////////////////////////////////
///// Call interface ////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
void ffi_call(FFI_Context *ffi, void *fn_ptr, void *return_val_ptr,
              Type *return_type);
void ffi_reset(FFI_Context *ffi);
void ffi_push_arg(FFI_Context *ffi, void *arg_ptr, Type *type);

/////////////////////////////////////////////////////////////////
///// Callback interface ////////////////////////////////////////
/////////////////////////////////////////////////////////////////
FFI_Handle ffi_create_callback(FFI_Context *ffi, FFI_Function_User_Data func_data, Type *fn_type);
s64 ffi_find_callback(FFI_Context *ffi, FFI_Handle ffi_handle);
char ffi_dcb_type_sig_char(Type *type, bool is_arg = false);
String ffi_dcb_func_sig(Type *func_type);

char dcb_callback_handler(DCCallback *cb, DCArgs *args, DCValue *result, void *userdata);

}}
