#include "zstring.h"

#include <common.h>
#include <memory/zmemory.h>

#ifdef _WIN32
#include <Windows.h>
#endif

namespace Zodiac
{


void String::init(Allocator * allocator, char *cstr, u64 len)
{
    assert(len >= 0);

    this->data = alloc_array<char>(allocator, len + 1);
    this->length = len;

    zmemcpy(this->data, cstr, (size_t)len);

    this->data[len] = '\0';
}

#ifdef _WIN32

void String::init(Allocator* allocator, wchar_t* wstr, u64 _length)
{
    int required_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wstr, (int)_length, nullptr, 0, nullptr, nullptr);
    assert(required_size);

    this->data = alloc_array<char>(allocator, ((u64)required_size) + 1);
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

    zmemcpy(result.data, original.data, (size_t)original.length);
    result.data[original.length] = '\0';

    return result;
}

String string_copy(Allocator *allocator, const char *cstr, u64 length)
{
    return string_copy(allocator, String_Ref(cstr, length));
}

String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b)
{
    auto new_length = a.length + b.length;

    String result(alloc_array<char>(allocator, new_length + 1), new_length);

    zmemcpy(result.data, a.data, (size_t)a.length);
    zmemcpy(result.data + a.length, b.data, (size_t)b.length);
    result.data[new_length] = '\0';

    return result;
}

bool string_contains(const String_Ref &str, const String_Ref &sub_str)
{
    if (sub_str.length > str.length) return false;
    if (str == sub_str) return true;

    for (u64 str_i = 0; str_i < str.length; str_i++) {

        auto rem_length = str.length - str_i;
        if (sub_str.length > rem_length) return false;

        bool match = true;
        for (u64 substr_i = 0; substr_i < sub_str.length; substr_i++) {
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

    return zmemcmp(string.data, start.data, start.length) == 0;
}

bool string_ends_with(const String_Ref &string, const String_Ref &end)
{
    assert(end.length <= string.length);

    auto offset = string.length - end.length;
    return zmemcmp(string.data + offset, end.data, end.length) == 0;
}

bool string_equal(const String_Ref &a, const String_Ref &b)

{
    if (a.length != b.length) return false;

    return zmemcmp(a.data, b.data, a.length) == 0;
}

const String string_format(Allocator* allocator, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = string_format(allocator, fmt, args);

    va_end(args);

    return result;
}

const String string_format(Allocator *allocator, const String_Ref fmt, va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);

    assert_msg(fmt.data[fmt.length] == '\0', "Null terminated string expected");
    auto size = vsnprintf(nullptr, 0, fmt.data, args_copy);

    va_end(args_copy);

    char *buf = alloc_array<char>(allocator, size + 1);
    assert(buf);

    auto written_size = vsnprintf(buf, (size_t)size + 1, fmt.data, args);
    assert(written_size <= size);

    assert_msg(written_size <= size, "Written size does not match the expected size")

    return String(buf, size);
}

i32 string_format(char *dest, const String_Ref fmt, ...)
{
    assert(dest && fmt.data);

    va_list args;
    va_start(args, fmt);

    char out_message[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    auto out_length = string_format(out_message, fmt, args);
    assert(out_length < ZSTRING_FORMAT_STACK_BUFFER_SIZE);

    va_end(args);

    zmemcpy(dest, out_message, out_length + 1);

    return out_length;
}

i32 string_format(char *dest, const String_Ref fmt, va_list args)
{
    assert(dest && fmt.data);

    char buffer[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    assert_msg(fmt.data[fmt.length] == '\0', "Null terminated string expected");
    auto written_size = vsnprintf(buffer, ZSTRING_FORMAT_STACK_BUFFER_SIZE, fmt.data, args);
    buffer[written_size] = '\0';
    zmemcpy(dest, buffer, written_size + 1);

    return written_size;
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

i64 string_to_i64(const String_Ref &string, u64 base /*= 10*/)
{
    i64 result = 0;

    u64 start_index = 0;
    bool negate = false;

    if (string.data[0] == '-') {
        negate = true;
        start_index = 1;
    }

    for (u64 i = start_index; i < string.length; i++) {
        result *= base;
        i64 digit_value = (i64)_digit_value(string.data[i]);
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
    r32 result = strtof(string.data, &end_ptr);
    if (result == 0.0f && end_ptr != string.data + string.length) {
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
