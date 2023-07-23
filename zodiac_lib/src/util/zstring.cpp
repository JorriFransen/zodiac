#include "zstring.h"

#include <stdio.h>
#include <stdlib.h>

#include "asserts.h"
#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "memory/zmemory.h"

#ifdef _WIN32
#include <Windows.h>
#endif

namespace Zodiac
{


void String::init(Allocator * allocator, char *cstr, s64 len)
{
    assert(len >= 0);

    this->data = alloc_array<char>(allocator, len + 1);
    this->length = len;

    zmemcpy(this->data, cstr, (size_t)len);

    this->data[len] = '\0';
}

String_Ref::String_Ref(const Atom *atom) : data(atom->data), length(atom->length) {}


#ifdef ZPLATFORM_WINDOWS

void String::init(Allocator* allocator, wchar_t* wstr, s64 _length)
{
    int required_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wstr, (int)_length, nullptr, 0, nullptr, nullptr);
    assert(required_size);

    this->data = alloc_array<char>(allocator, ((s64)required_size) + 1);
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

Wide_String::Wide_String(Allocator *allocator, const String_Ref str_ref)
{
    if (str_ref.length == 0) {
        assert(str_ref.data == nullptr);
        this->data = nullptr;
        this->length = 0;
        return;
    }

    auto size = MultiByteToWideChar(CP_UTF8, MB_PRECOMPOSED, str_ref.data, (int)str_ref.length + 1, nullptr, 0);
    assert(size > 0);

    LPWSTR buf = alloc_array<WCHAR>(allocator, size);
    auto written_size = MultiByteToWideChar(CP_UTF8, MB_PRECOMPOSED, str_ref.data, (int)str_ref.length + 1, buf, size);
    assert(written_size == size);

    this->data = buf;
    this->length = written_size - 1;
}

#endif //ZPLATFORM_WINDOWS

String string_copy(Allocator *allocator, const String_Ref &original)
{
    String result(alloc_array<char>(allocator, original.length + 1), original.length);

    zmemcpy(result.data, original.data, (size_t)original.length);
    result.data[original.length] = '\0';

    return result;
}

String string_copy(Allocator *allocator, const char *cstr, s64 length)
{
    return string_copy(allocator, String_Ref(cstr, length));
}

String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b)
{
    auto new_length = a.length + b.length;

    String result(alloc_array<char>(allocator, new_length + 1), new_length);

    if (a.length) zmemcpy(result.data, a.data, (size_t)a.length);
    if (b.length) zmemcpy(result.data + a.length, b.data, (size_t)b.length);
    result.data[new_length] = '\0';

    return result;
}

String string_append(Allocator *allocator, Array_Ref<String_Ref> strings, const String_Ref &separator/*=""*/)
{
    s64 new_length = 0;

    for (s64 i = 0; i < strings.count; i++) {
        new_length += strings[i].length;
    }

    new_length += separator.length * (strings.count - 1);

    String result(alloc_array<char>(allocator, new_length + 1), new_length);

    auto cursor = result.data;
    for (s64 i = 0; i < strings.count; i++) {
        zmemcpy(cursor, strings[i].data, strings[i].length);
        cursor += strings[i].length;

        if (separator.length) {
            zmemcpy(cursor, separator.data, separator.length);
            cursor += separator.length;
        }
    }

    result.data[new_length] = '\0';

    return result;
}

#ifdef ZPLATFORM_WINDOWS
Wide_String string_append(Allocator *allocator, const Wide_String_Ref &a, const Wide_String_Ref &b)
{
    auto new_length = a.length + b.length;
    auto data = alloc_array<wchar_t>(allocator, new_length + 1);

    Wide_String new_str(data, new_length);

    memcpy(new_str.data, a.data, a.length * sizeof(wchar_t));
    memcpy(new_str.data + a.length, b.data, b.length * sizeof(wchar_t));

    new_str.data[new_length] = L'\0';

    return new_str;
}
#endif //ZPLATFORM_WINDOWS

bool string_empty(const String_Ref &string)
{
    return string.length == 0 || string.data == nullptr;
}

bool string_contains(const String_Ref &str, const String_Ref &sub_str)
{
    if (sub_str.length > str.length) return false;
    if (str == sub_str) return true;

    for (s64 str_i = 0; str_i < str.length; str_i++) {

        auto rem_length = str.length - str_i;
        if (sub_str.length > rem_length) return false;

        bool match = true;
        for (s64 substr_i = 0; substr_i < sub_str.length; substr_i++) {
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
    if (a.length == 0) return true;

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

    auto out_length = string_format(dest, fmt, args);
    assert(out_length < ZSTRING_FORMAT_STACK_BUFFER_SIZE);

    va_end(args);

    return out_length;
}

i32 string_format(char *dest, const String_Ref fmt, va_list args)
{
    assert(dest && fmt.data);

    static char buffer[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    assert_msg(fmt.data[fmt.length] == '\0', "Null terminated string expected");
    auto written_size = vsnprintf(buffer, ZSTRING_FORMAT_STACK_BUFFER_SIZE, fmt.data, args);
    buffer[written_size] = '\0';
    zmemcpy(dest, buffer, written_size + 1);

    return written_size;
}

ZINLINE file_local u64 _digit_value(char c)
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

s64 string_to_s64(const String_Ref &string, s64 base /*= 10*/)
{
    s64 result = 0;

    s64 start_index = 0;
    bool negate = false;

    if (string.data[0] == '-') {
        negate = true;
        start_index = 1;
    }

    for (s64 i = start_index; i < string.length; i++) {
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
