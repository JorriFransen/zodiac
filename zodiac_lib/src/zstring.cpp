#include "zstring.h"

#include <string.h>
#include <common.h>

#ifdef _WIN32
#include <windows.h>
#endif

#include <stdarg.h>

namespace Zodiac
{


void String::__init__(Allocator * allocator, char *cstr, i64 len)
{
    assert(len >= 0);

    this->data = alloc_array<char>(allocator, len + 1);
    this->length = len;

    memcpy(this->data, cstr, len);

    this->data[len] = '\0';
}

#ifdef _WIN32

void String::__init__(Allocator* allocator, wchar_t* wstr, i64 _length)
{
    int required_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wstr, (int)_length, nullptr, 0, nullptr, nullptr);
    assert(required_size);

    this->data = alloc_array<char>(allocator, ((i64)required_size) + 1);
    this->length = required_size;

    auto written_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wstr, (int)_length, this->data, required_size + 1, nullptr, nullptr);

    assert(written_size == required_size);
    if (written_size == required_size) {
        this->data[required_size] = '\0';
    } else {
        assert(false);
        exit(42);
    }
}

#endif

String string_copy(Allocator *allocator, const String_Ref &original)
{
    String result(alloc_array<char>(allocator, original.length + 1), original.length);

    memcpy(result.data, original.data, original.length);
    result.data[original.length] = '\0';

    return result;
}

String string_copy(Allocator *allocator, const char *cstr, i64 length)
{
    return string_copy(allocator, String_Ref(cstr, length));
}

String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b)
{
    auto new_length = a.length + b.length;

    String result(alloc_array<char>(allocator, new_length + 1), new_length);

    memcpy(result.data, a.data, a.length);
    memcpy(result.data + a.length, b.data, b.length);
    result.data[new_length] = '\0';

    return result;
}

bool string_contains(const String_Ref &str, const String_Ref &sub_str)
{
    if (sub_str.length > str.length) return false;
    if (str == sub_str) return true;

    for (i64 str_i = 0; str_i < str.length; str_i++) {

        auto rem_length = str.length - str_i;
        if (sub_str.length > rem_length) return false;

        bool match = true;
        for (i64 substr_i = 0; substr_i < sub_str.length; substr_i++) {
            if (str[str_i + substr_i] != sub_str[substr_i]) {
                match = false;
                break;
            }
        }

        if (match) return true;
    }

    return false;
}

bool string_starts_with(const String_Ref &string, const String_Ref &start)
{
    if (!(string.length > start.length)) {
        return false;
    }

    for (i64 i = 0; i < start.length; i++) {
        if (string[i] != start[i]) return false;
    }

    return true;
}

bool string_ends_with(const String_Ref &string, const String_Ref &end)
{
    assert(end.length <= string.length);

    auto offset = string.length - end.length;

    for (i64 i = 0; i < end.length; i++) {
        if (end[i] != string[offset + i]) return false;
    }

    return true;
}

bool string_equal(const String_Ref &a, const String_Ref &b)

{
    if (a.length != b.length) return false;

    for (i64 i = 0; i < a.length; i++) {
        if (a[i] != b[i]) return false;
    }

    return true;
}

const String string_format(Allocator* allocator, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = string_format(allocator, fmt, args);

    va_end(args);

    return result;
}

const String string_format(Allocator *allocator, const char *fmt, va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);

    auto size = vsnprintf(nullptr, 0, fmt, args_copy);

    va_end(args_copy);

    char *buf = alloc_array<char>(allocator, size + 1);
    assert(buf);

    auto written_size = vsnprintf(buf, size + 1, fmt, args);
    assert(written_size <= size);
    zodiac_assert_fatal(written_size <= size, "Written size does not match the expected size");

    return String(buf, size);
}

static u64 _digit_value(char c)
{
    if (c >= '0' && c <= '9')
    {
        return ((u64)c - '0');
    }
    else if (c >= 'a'  && c <= 'f')
    {
        return ((u64)c - 'a') + 10;
    }
    else if (c >= 'A' && c <= 'F')
    {
        return ((u64)c - 'A') + 10;
    }
    else assert(false);

    assert(false);
    return 0;
}

i64 string_to_s64(const String_Ref &string, u64 base /*= 10*/)
{
    i64 result = 0;

    i64 start_index = 0;
    bool negate = false;

    if (string.data[0] == '-') {
        negate = true;
        start_index = 1;
    }

    for (i64 i = start_index; i < string.length; i++) {
        result *= base;
        i64 digit_value = _digit_value(string.data[i]);
        result += digit_value;
    }

    if (negate) {
        return -result;
    }
    return result;
}

Real_Value string_to_real(const String_Ref &string)
{
    return { .r32 = string_to_float(string), .r64 = string_to_double(string) };
}

float string_to_float(const String_Ref &string)
{
    char *end_ptr;
    float result = strtof(string.data, &end_ptr);
    if (result == 0.0 && end_ptr != string.data + string.length) {
        assert(false);
    }

    return result;
}

double string_to_double(const String_Ref &string)
{
    char *end_ptr;
    double result = strtod(string.data, &end_ptr);
    if (result == 0.0 && end_ptr != string.data + string.length) {
        assert(false);
    }

    return result;
}

}
