#pragma once

#include <stdarg.h>
#include <string>
#include <string.h>

#include "asserts.h"
#include "common.h"
#include "defines.h"

namespace Zodiac
{

struct Allocator;
struct Atom;

#define ZSTRING_FORMAT_STACK_BUFFER_SIZE 32000

struct String
{
    char* data = nullptr;
    s64 length = 0;

    String() {}
    String(char* data, s64 length) : data(data), length(length) {}
    String(Allocator* allocator, char *cstr, s64 length) { init(allocator, cstr, length); }
    String(Allocator* allocator, const char *cstr, s64 length) { init(allocator, (char *)cstr, length); }
    String(Allocator *allocator, const char *cstr) { init(allocator, (char *)cstr, strlen(cstr)); }

    void init(Allocator * allocator, char *cstr, s64 length);

#ifdef ZPLATFORM_WINDOWS
    String(Allocator* allocator, wchar_t *wstr, s64 length) { init(allocator, wstr, length); }
    String(Allocator* allocator, const wchar_t *wstr, s64 length) { init(allocator, (wchar_t *)wstr, length); }
    void init(Allocator * allocator, wchar_t *wstr, s64 length);
#endif //ZPLATFORM_WINDOWS

    char &operator[](s64 index)
    {
        assert(index >= 0 && index <= length);
        return data[index];
    }

};

struct String_Ref
{
    const char *data = nullptr;
    s64 length = 0;

    String_Ref() : data(nullptr), length(0) {}

    String_Ref(const char *cstr) : data(cstr), length((s64)strlen(cstr)) {}
    String_Ref(const char *cstr, s64 length) : data(cstr), length(length) {}
    String_Ref(const std::string &std_str) : data(std_str.c_str()), length((s64)std_str.length()) {}
    String_Ref(const String &zstr) : data(zstr.data), length(zstr.length) {}
    String_Ref(const Atom *atom);

    const char &operator[](s64 index) const {
        assert(index < this->length);
        return data[index];
    }

    bool operator==(const String_Ref &rhs) const {
        return data == rhs.data && length == rhs.length;
    }
};

#ifdef ZPLATFORM_WINDOWS

struct Wide_String_Ref
{
    const wchar_t *data;
    s64 length;

    Wide_String_Ref() : data(nullptr), length(0) {}
    Wide_String_Ref(const wchar_t *wcstr) : data(wcstr), length(wcslen(wcstr)) {}
};

struct Wide_String
{
    wchar_t *data;
    s64 length;

    Wide_String() : data(nullptr), length(0) {}
    Wide_String(wchar_t *data, s64 length) : data(data), length(length) {}
    Wide_String(Allocator *allocator, const String_Ref str_ref);
};

#endif //ZPLATFORM_WINDOWS

ZAPI String string_copy(Allocator *allocator, const String_Ref &original);
ZAPI String string_copy(Allocator *allocator, const char *cstr, s64 length);

ZAPI String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b);

#ifdef ZPLATFORM_WINDOWS
ZAPI Wide_String string_append(Allocator *allocator, const Wide_String_Ref &a, const Wide_String_Ref &b);
#endif //ZPLATFORM_WINDOWS

ZAPI bool string_contains(const String_Ref &string, const String_Ref &sub_string);
ZAPI bool string_starts_with(const String_Ref &string, const String_Ref &start);
ZAPI bool string_ends_with(const String_Ref &string, const String_Ref &end);
ZAPI bool string_equal(const String_Ref &a, const String_Ref &b);

ZAPI const String string_format(Allocator *allocator, const String_Ref fmt, ...);
ZAPI const String string_format(Allocator *allocator, const String_Ref fmt, va_list args);

ZAPI i32 string_format(char *dest, const String_Ref fmt, ...);
ZAPI i32 string_format(char *dest, const String_Ref fmt, va_list args);

ZAPI s64 string_to_s64(const String_Ref &string, s64 base = 10);
ZAPI Real_Value string_to_real(const String_Ref &string);
ZAPI float string_to_float(const String_Ref &string);
ZAPI double string_to_double(const String_Ref &string);

}
