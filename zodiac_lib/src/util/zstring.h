#pragma once

#include <stdarg.h>
#include <string>
#include <string.h>

#include "asserts.h"
#include "atom.h"
#include "common.h"
#include "defines.h"

namespace Zodiac
{

struct Allocator;

#define ZSTRING_FORMAT_STACK_BUFFER_SIZE 32000

struct String
{
    char* data = nullptr;
    u64 length = 0;

    String() {}
    String(char* data, u64 length) : data(data), length(length) {}
    String(Allocator* allocator, char *cstr, u64 length) { init(allocator, cstr, length); }
    String(Allocator* allocator, const char *cstr, u64 length) { init(allocator, (char *)cstr, length); }

    void init(Allocator * allocator, char *cstr, u64 length);

#ifdef _WIN32
    String(Allocator* allocator, wchar_t *wstr, u64 length) { init(allocator, wstr, length); }
    String(Allocator* allocator, const wchar_t *wstr, u64 length) { init(allocator, (wchar_t *)wstr, length); }
    void init(Allocator * allocator, wchar_t *wstr, u64 length);
#endif

    char &operator[](u64 index)
    {
        assert(index >= 0 && index <= length);
        return data[index];
    }

};

struct String_Ref
{
    const char *data = nullptr;
    u64 length = 0;

    String_Ref() : data(nullptr), length(0) {}

    String_Ref(const char *cstr) : data(cstr), length((u64)strlen(cstr)) {}
    String_Ref(const char *cstr, u64 length) : data(cstr), length(length) {}
    String_Ref(const std::string &std_str) : data(std_str.c_str()), length((u64)std_str.length()) {}
    String_Ref(const String &zstr) : data(zstr.data), length(zstr.length) {}
    String_Ref(const Atom &atom) : data(atom.data), length(atom.length) {}

    const char &operator[](u64 index) const {
        assert(index < this->length);
        return data[index];
    }

    bool operator==(const String_Ref &rhs) const {
        return data == rhs.data && length == rhs.length;
    }
};

ZAPI String string_copy(Allocator *allocator, const String_Ref &original);
ZAPI String string_copy(Allocator *allocator, const char *cstr, u64 length);
ZAPI String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b);

ZAPI bool string_contains(const String_Ref &string, const String_Ref &sub_string);
ZAPI bool string_starts_with(const String_Ref &string, const String_Ref &start);
ZAPI bool string_ends_with(const String_Ref &string, const String_Ref &end);
ZAPI bool string_equal(const String_Ref &a, const String_Ref &b);

ZAPI const String string_format(Allocator *allocator, const String_Ref fmt, ...);
ZAPI const String string_format(Allocator *allocator, const String_Ref fmt, va_list args);

ZAPI i32 string_format(char *dest, const String_Ref fmt, ...);
ZAPI i32 string_format(char *dest, const String_Ref fmt, va_list args);

ZAPI i64 string_to_i64(const String_Ref &string, u64 base = 10);
ZAPI Real_Value string_to_real(const String_Ref &string);
ZAPI float string_to_float(const String_Ref &string);
ZAPI double string_to_double(const String_Ref &string);

}