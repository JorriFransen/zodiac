#include "zstring.h"

#include <stdio.h>
#include <stdlib.h>

#include "asserts.h"
#include "atom.h"
#include "containers/dynamic_array.h"
#include "defines.h"
#include "memory/allocator.h"
#include "memory/zmemory.h"

#ifdef _WIN32
#include <Windows.h>
#endif

namespace Zodiac
{

String string_create(char *cstr, s64 length)
{
    debug_assert(cstr);
    debug_assert(length >= 0);

    String result;

    result.data = cstr;
    result.length = length;

    assert(result.data[length] == '\0');

    return result;
}

String string_create(String_Ref ref)
{
    return string_create((char *)ref.data, ref.length);
}

String string_create(Allocator * allocator, char *cstr, s64 len)
{
    debug_assert(cstr);
    debug_assert(len >= 0);

    String result;

    result.data = alloc_array<char>(allocator, len + 1);
    result.length = len;

    zmemcpy(result.data, cstr, (size_t)len);

    result.data[len] = '\0';

    return result;
}

String string_create(Allocator *allocator, char *cstr)
{
    auto length = cstr ? strlen(cstr) : 0;
    return string_create(allocator, cstr, length);
}

String_Ref::String_Ref(const Atom &atom) : data(atom.data), length(atom.length) {}

#ifdef ZPLATFORM_WINDOWS

String string_create(Allocator* allocator, wchar_t* wstr, s64 _length)
{
    int required_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wstr, (int)_length, nullptr, 0, nullptr, nullptr);
    assert(required_size);

    auto data = alloc_array<char>(allocator, ((s64)required_size) + 1);
    s64 length = required_size;

    auto written_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wstr, (int)_length, data, required_size + 1, nullptr, nullptr);

    assert(written_size == required_size);
    if (written_size == required_size) {
        data[required_size] = '\0';
    } else {
        assert(false);
        platform_exit(42);
    }

    return string_create(data, length);
}

String string_create(Allocator* allocator, Wide_String_Ref ref)
{
    return string_create(allocator, (wchar_t *)ref.data, ref.length);
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
    auto data = alloc_array<char>(allocator, original.length + 1);

    zmemcpy(data, original.data, (size_t)original.length);
    data[original.length] = '\0';

    return string_create(data, original.length);
}

String string_copy(Allocator *allocator, const char *cstr, s64 length)
{
    return string_copy(allocator, String_Ref(cstr, length));
}

String string_append(Allocator *allocator, const String_Ref &a, const String_Ref &b)
{
    auto new_length = a.length + b.length;

    auto data = alloc_array<char>(allocator, new_length + 1);

    if (a.length) zmemcpy(data, a.data, (size_t)a.length);
    if (b.length) zmemcpy(data + a.length, b.data, (size_t)b.length);
    data[new_length] = '\0';

    return string_create(data, new_length);
}

String string_append(Allocator *allocator, Array_Ref<String_Ref> strings, const String_Ref &separator/*=""*/)
{
    s64 new_length = 0;

    for (s64 i = 0; i < strings.count; i++) {
        new_length += strings[i].length;
    }

    new_length += separator.length * (strings.count - 1);

    auto data = alloc_array<char>(allocator, new_length + 1);


    auto cursor = data;
    for (s64 i = 0; i < strings.count; i++) {
        zmemcpy(cursor, strings[i].data, strings[i].length);
        cursor += strings[i].length;

        if (separator.length && i < strings.count - 1) {
            zmemcpy(cursor, separator.data, separator.length);
            cursor += separator.length;
        }
    }

    data[new_length] = '\0';

    return string_create(data, new_length);
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

    return string_create(buf, size);
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

// TODO: FIXME: Emit these arrays with a macro
file_local char special_characters[] = {
    '\n',
    '\t',
};

file_local char escape_characters[] = {
    'n',
    't',
};

file_local s64 is_special_character(char c) {

    for (s64 i = 0; i < sizeof(special_characters) / sizeof(special_characters[0]); i++) {
        if (c  == special_characters[i]) return i;
    }

    return -1;
}

file_local s64 is_escape_character(char c) {
    for (s64 i = 0; i < sizeof(escape_characters) / sizeof(escape_characters[0]); i++) {
        if (c == escape_characters[i]) return i;
    }

    return -1;
}

String convert_special_characters_to_escape_characters(Allocator *allocator, const String_Ref str)
{
    s64 special_count = 0;

    for (s64 i = 0; i < str.length; i++) {
        auto c = str[i];

        if (is_special_character(c) != -1) {
            special_count += 1;
            break;
        }
    }

    if (!special_count) {
        return string_copy(allocator, str);
    }

    auto new_length = str.length + special_count;

    auto data = alloc_array<char>(allocator, new_length + 1);

    s64 ni = 0;
    for (s64 i = 0; i < str.length; i++) {
        auto special_index = is_special_character(str[i]);
        if (special_index != -1) {
            data[ni++] = '\\';
            data[ni++] = escape_characters[special_index];
        } else {
            data[ni++] = str[i];
        }
    }

    data[new_length] = '\0';
    return string_create(data, new_length);
}

String convert_escape_characters_to_special_characters(Allocator *allocator, const String_Ref str, const char **err_char/*=nullptr*/)
{
    s64 escape_count = 0;

    for (s64 i = 0; i < str.length; i++) {
        auto c = str[i];

        if (c == '\\') {
            assert(i + 1 < str.length);

            if (is_escape_character(str[i + 1]) == -1) {
                if (err_char) {
                    *err_char = &str[i + 1];
                } else {
                    assert_msg(false, "Invalid escape character!");
                }
            }

            escape_count += 1;
        }
    }

    if (!escape_count) {
        return string_copy(allocator, str);
    }

    auto new_length = str.length - escape_count;

    auto data = alloc_array<char>(allocator, new_length + 1);

    s64 ni = 0;
    for (s64 i = 0; i < str.length; i++) {
        if (str[i] == '\\')  {
            debug_assert(i + 1 < str.length);
            i += 1;
            auto escape_index = is_escape_character(str[i]);
            data[ni++] = special_characters[escape_index];
        } else {
            data[ni++] = str[i];
        }
    }

    data[new_length] = '\0';
    return string_create(data, new_length);
}

}
