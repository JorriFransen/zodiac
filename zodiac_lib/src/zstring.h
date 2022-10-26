#pragma once

#include <allocator.h>
#include <atom.h>
#include <common.h>
#include <defines.h>

// We need both of these because we are using c++ std::strings, and strlen for c style strings
#include <string>
#include <string.h>

namespace Zodiac
{

struct String
{
    char* data = nullptr;
    i64 length = 0;

    String() {}
    String(char* data, i64 length) : data(data), length(length) {}
    String(Allocator* allocator, char *cstr, i64 length) { __init__(allocator, cstr, length); }
    String(Allocator* allocator, const char *cstr, i64 length) { __init__(allocator, (char *)cstr, length); }

    void __init__(Allocator * allocator, char *cstr, i64 length);

#ifdef _WIN32
    String(Allocator* allocator, wchar_t *wstr, i64 length) { __init__(allocator, wstr, length); }
    String(Allocator* allocator, const wchar_t *wstr, i64 length) { __init__(allocator, (wchar_t *)wstr, length); }
    void __init__(Allocator * allocator, wchar_t *wstr, i64 length);
#endif

    char &operator[](i64 index)
    {
        assert(index >= 0 && index <= length);
        return data[index];
    }

};

struct String_Ref
{
    const char *data = nullptr;
    i64 length = 0;

    String_Ref() : data(nullptr), length(0) {}

    String_Ref(const char *cstr) : data(cstr), length(strlen(cstr)) {}
    String_Ref(const char *cstr, i64 length) : data(cstr), length(length) {}
    String_Ref(const std::string &std_str) : data(std_str.c_str()), length(std_str.length()) {}
    String_Ref(const String &zstr) : data(zstr.data), length(zstr.length) {}
    String_Ref(const Atom &atom) : data(atom.data), length(atom.length) {}

    const char &operator[](i64 index) const {
        assert(index < this->length);
        return data[index];
    }

    bool operator==(const String_Ref &rhs) const {
        return data == rhs.data && length == rhs.length;
    }
};

ZAPI String string_copy(Allocator *allocator, const String_Ref &original);
ZAPI String string_copy(Allocator *allocator, const char *cstr, i64 length);
ZAPI String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b);

ZAPI bool string_contains(const String_Ref &string, const String_Ref &sub_string);
ZAPI bool string_starts_with(const String_Ref &string, const String_Ref &start);
ZAPI bool string_ends_with(const String_Ref &string, const String_Ref &end);
ZAPI bool string_equal(const String_Ref &a, const String_Ref &b);

ZAPI const String string_format(Allocator *allocator, const char *fmt, ...);
ZAPI const String string_format(Allocator *allocator, const char *fmt, va_list args);

ZAPI i64 string_to_s64(const String_Ref &string, u64 base = 10);
ZAPI Real_Value string_to_real(const String_Ref &string);
ZAPI float string_to_float(const String_Ref &string);
ZAPI double string_to_double(const String_Ref &string);

}