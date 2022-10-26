#pragma once

#include <allocator.h>
#include <atom.h>
#include <common.h>

#include <cassert>
#include <inttypes.h>
#include <string>
#include <string.h>

namespace Zodiac
{

struct String
{
    char* data = nullptr;
    int64_t length = 0;

    String() {}
    String(char* data, int64_t length) : data(data), length(length) {}
    String(Allocator* allocator, char *cstr, int64_t length) { __init__(allocator, cstr, length); }
    String(Allocator* allocator, const char *cstr, int64_t length) { __init__(allocator, (char *)cstr, length); }

    void __init__(Allocator * allocator, char *cstr, int64_t length);

#ifdef _WIN32
    String(Allocator* allocator, wchar_t *wstr, int64_t length) { __init__(allocator, wstr, length); }
    String(Allocator* allocator, const wchar_t *wstr, int64_t length) { __init__(allocator, (wchar_t *)wstr, length); }
    void __init__(Allocator * allocator, wchar_t *wstr, int64_t length);
#endif

    char &operator[](int64_t index)
    {
        assert(index >= 0 && index <= length);
        return data[index];
    }

};

struct String_Ref
{
    const char *data = nullptr;
    int64_t length = 0;

    String_Ref() : data(nullptr), length(0) {}

    String_Ref(const char *cstr) : data(cstr), length(strlen(cstr)) {}
    String_Ref(const char *cstr, int64_t length) : data(cstr), length(length) {}
    String_Ref(const std::string &std_str) : data(std_str.c_str()), length(std_str.length()) {}
    String_Ref(const String &zstr) : data(zstr.data), length(zstr.length) {}
    String_Ref(const Atom &atom) : data(atom.data), length(atom.length) {}

    const char &operator[](int64_t index) const {
        assert(index < this->length);
        return data[index];
    }

    bool operator==(const String_Ref &rhs) const {
        return data == rhs.data && length == rhs.length;
    }
};

String string_copy(Allocator *allocator, const String_Ref &original);
String string_copy(Allocator *allocator, const char *cstr, int64_t length);
String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b);

bool string_contains(const String_Ref &string, const String_Ref &sub_string);
bool string_starts_with(const String_Ref &string, const String_Ref &start);
bool string_ends_with(const String_Ref &string, const String_Ref &end);
bool string_equal(const String_Ref &a, const String_Ref &b);

const String string_format(Allocator *allocator, const char *fmt, ...);
const String string_format(Allocator *allocator, const char *fmt, va_list args);

int64_t string_to_s64(const String_Ref &string, uint64_t base = 10);
Real_Value string_to_real(const String_Ref &string);
float string_to_float(const String_Ref &string);
double string_to_double(const String_Ref &string);

}