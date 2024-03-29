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

template <typename T> struct Array_Ref;

#define ZSTRING_FORMAT_STACK_BUFFER_SIZE 32000

ZAPI ZINLINE size_t zstrlen(const char *cstr)
{
    return strlen(cstr);
}

struct String
{
    char* data = nullptr;
    s64 length = 0;

    ZAPI char &operator[](s64 index)
    {
        assert(index >= 0 && index <= length);
        return data[index];
    }

};

struct String_Ref
{
    const char *data = nullptr;
    s64 length = 0;

    ZAPI String_Ref() : data(nullptr), length(0) {}

    ZAPI String_Ref(const char *cstr) : data(cstr) { length = cstr ? strlen(cstr) : 0; }
    ZAPI String_Ref(const char *cstr, s64 length) : data(cstr), length(length) {}
    ZAPI String_Ref(const std::string &std_str) : data(std_str.c_str()), length((s64)std_str.length()) {}
    ZAPI String_Ref(const String &zstr) : data(zstr.data), length(zstr.length) {}
    ZAPI String_Ref(const Atom &atom);

    ZAPI const char &operator[](s64 index) const {
        assert(index < this->length);
        return data[index];
    }

    ZAPI bool operator==(const String_Ref &rhs) const {
        return data == rhs.data && length == rhs.length;
    }
};

ZAPI String string_create(char *cstr, s64 length);
ZAPI String string_create(String_Ref ref);
ZAPI String string_create(wchar_t* wstr, s64 _length);
ZAPI String string_create(Allocator *allocator, char *cstr, s64 length);
ZAPI String string_create(Allocator *allocator, char *cstr);


#ifdef ZPLATFORM_WINDOWS

struct Wide_String_Ref
{
    const wchar_t *data;
    s64 length;

    ZAPI Wide_String_Ref() : data(nullptr), length(0) {}
    ZAPI Wide_String_Ref(const wchar_t *wcstr) : data(wcstr), length(wcslen(wcstr)) {}
};

struct Wide_String
{
    wchar_t *data;
    s64 length;

    ZAPI Wide_String() : data(nullptr), length(0) {}
    ZAPI Wide_String(wchar_t *data, s64 length) : data(data), length(length) {}
    ZAPI Wide_String(Allocator *allocator, const String_Ref str_ref);
};

ZAPI String string_create(Allocator* allocator, wchar_t* wstr, s64 _length);
ZAPI String string_create(Allocator* allocator, Wide_String_Ref ref);

#endif //ZPLATFORM_WINDOWS

ZAPI String string_copy(Allocator *allocator, const String_Ref &original);
ZAPI String string_copy(Allocator *allocator, const char *cstr, s64 length);

ZAPI String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b);
ZAPI String string_append(Allocator *allocator, Array_Ref<String_Ref> strings, const String_Ref &separator = "");

#ifdef ZPLATFORM_WINDOWS
ZAPI Wide_String string_append(Allocator *allocator, const Wide_String_Ref &a, const Wide_String_Ref &b);
#endif //ZPLATFORM_WINDOWS

ZAPI bool string_empty(const String_Ref &string);
ZAPI bool string_contains(const String_Ref &string, const String_Ref &sub_string);
ZAPI bool string_starts_with(const String_Ref &string, const String_Ref &start);
ZAPI bool string_ends_with(const String_Ref &string, const String_Ref &end);
ZAPI bool string_equal(const String_Ref &a, const String_Ref &b);
ZAPI s64 string_index_of(const String_Ref &a, char c);
ZAPI s64 string_last_index_of(const String_Ref &a, char c);

ZAPI const String string_format(Allocator *allocator, const String_Ref fmt, ...);
ZAPI const String string_format_va_list(Allocator *allocator, const String_Ref fmt, va_list args);

ZAPI i32 string_format(char *dest, const String_Ref fmt, ...);
ZAPI i32 string_format(char *dest, const String_Ref fmt, va_list args);

ZAPI s64 string_to_s64(const String_Ref &string, s64 base = 10);
ZAPI Real_Value string_to_real(const String_Ref &string);
ZAPI float string_to_float(const String_Ref &string);
ZAPI double string_to_double(const String_Ref &string);

ZAPI s64 is_escape_character(char c);

ZAPI String convert_special_characters_to_escape_characters(Allocator *allocator, const String_Ref str);
ZAPI String convert_escape_characters_to_special_characters(Allocator *allocator, const String_Ref str, const char **err_char = nullptr);

}
