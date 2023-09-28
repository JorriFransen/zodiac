#include "lexer.h"

#include "error.h"
#include "memory/temporary_allocator.h"
#include "memory/zmemory.h"
#include "util/asserts.h"
#include "util/logger.h"
#include "zodiac_context.h"

#include <cctype>
#include <cmath>
#include <errno.h>
#include <float.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

namespace Zodiac
{

file_local bool lex_int(Lexer *lexer);
file_local bool lex_real(Lexer *lexer);

// Emit atom definitions for all keywords
#define ZODIAC_KEYWORD(n) Atom keyword_##n;
ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

// Emit array with all keywords
file_local Atom *all_keywords[] = {
#define ZODIAC_KEYWORD(n) &(keyword_##n),
    ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD
};

// Emit atom definitions for all directives
#define ZODIAC_DIRECTIVE(n) Atom directive_##n;
ALL_ZODIAC_DIRECTIVES
#undef ZODIAC_DIRECTIVE

void zodiac_register_keywords(Atom_Table *at)
{
    Atom_Block *block = at->current_block;

    // Emit init for all keywords
#define ZODIAC_KEYWORD(n) keyword_##n = atom_get(at, #n);
    ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

    // Emit init for all directives
#define ZODIAC_DIRECTIVE(n) directive_##n = atom_get(at, #n);
    ALL_ZODIAC_DIRECTIVES
#undef ZODIAC_DIRECTIVE

    assert_msg(block == at->current_block, "Expected all keyword atoms to fit in the same block...");
}

#define report_lex_error(lexer, range, fmt, ...)                                            \
{                                                                                           \
    (lexer)->token.kind = TOK_INVALID;                                                      \
    zodiac_report_error((lexer)->context, ZODIAC_LEX_ERROR, (range), (fmt), ##__VA_ARGS__); \
}

bool is_keyword(const Atom &atom)
{
    return atom.data >= all_keywords[0]->data && atom.data <= all_keywords[sizeof(all_keywords)/sizeof(all_keywords[0]) - 1]->data;
}

void lexer_create(Zodiac_Context *context, Lexer *out_lexer)
{
    assert(context && out_lexer);

    out_lexer->context = context;
    out_lexer->stream_start = nullptr;
    out_lexer->stream = nullptr;
    out_lexer->line_start = nullptr;

    out_lexer->token = {};
}

void lexer_init_stream(Lexer *lexer, const String_Ref stream, const String_Ref stream_name)
{
    assert(lexer && lexer->context && stream.data);

    lexer->stream_name = stream_name;

    lexer->stream_start = stream.data;
    lexer->stream = stream.data;
    lexer->line_start = stream.data;

    lexer->token.kind = TOK_INVALID;
    lexer->token.range.start.name = stream_name;
    lexer->token.range.start.line = 1;
    lexer->token.range.start.index_in_line = 0;

    next_token(lexer);
}

void lexer_destroy(Lexer *lexer)
{
    assert(lexer && lexer->stream_start && lexer->stream);

    zzeromem(lexer, sizeof(Lexer));
}

bool next_token(Lexer *lex)
{
    assert(lex && lex->stream_start && lex->stream);

next_token__start_lexing_token:
    lex->token.kind = TOK_INVALID;
    auto start = lex->stream;

    switch (*lex->stream) {

#define TWO_CHAR_TOKEN_CASE(first_char, second_char, two_char_kind) \
case (first_char): {                                                \
    lex->token.kind = (Token_Kind)*lex->stream;                     \
    lex->stream += 1;                                               \
    if (*lex->stream == (second_char)) {                            \
        lex->token.kind = (two_char_kind);                          \
        lex->stream += 1;                                           \
    }                                                               \
    break;                                                          \
}


        case 0: {
            lex->token.kind = TOK_EOF;
            break;
        }

        case ' ': case '\n': case '\r': case '\t': {
            if (*lex->stream == '\n') {
                lex->token.range.start.line += 1;
                lex->line_start = lex->stream + 1;
            }
            lex->stream += 1;
            while (isspace(*lex->stream)) {
                if (*lex->stream == '\n') {
                    lex->token.range.start.line += 1;
                    lex->line_start = lex->stream + 1;
                }
                lex->stream += 1;
            }

            goto next_token__start_lexing_token;
            break;
        }

        case '"': {
            lex->token.kind = TOK_STRING;
            lex->stream += 1;
            while (*lex->stream != '"') {
                lex->stream += 1;
            }
            lex->stream += 1;
            break;
        }

        case '\'': {
            lex->token.kind = TOK_CHAR;
            lex->stream += 1;
            lex->token.character = *lex->stream;
            lex->stream += 1;

            if (*lex->stream != '\'') {
                report_lex_error(lex, lex->token.range, "Exected \"'\" to end character literal");
                return false;
            }

            lex->stream += 1;
            break;
        }

        case '/': {
            lex->token.kind = (Token_Kind)'/';
            lex->stream += 1;
            if (*lex->stream == '/') {
                // Single line comment
                while (*lex->stream && *lex->stream != '\n') {
                    lex->stream += 1;
                }
                goto next_token__start_lexing_token;
            }
            break;
        }

        case '.': {
            if (isdigit(lex->stream[1])) {
                if (!lex_real(lex)) return false;
            } else {
                lex->token.kind = (Token_Kind)*lex->stream;
                lex->stream += 1;
            }
            break;
        }

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        {
            while (isdigit(*lex->stream)) {
                lex->stream += 1;
            }

            char c = *lex->stream;
            lex->stream = start;

            if (c == '.' || tolower(c) == 'e') {
                if (!lex_real(lex)) return false;
            } else {
                if (!lex_int(lex)) return false;
            }
            break;
        }

        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '_':
        {
            lex->token.kind = TOK_NAME;
            while (isalnum(*lex->stream) || *lex->stream == '_') {
                lex->stream += 1;
            }
            break;
        }

        TWO_CHAR_TOKEN_CASE('=', '=', TOK_EQ);
        TWO_CHAR_TOKEN_CASE('!', '=', TOK_NEQ);
        TWO_CHAR_TOKEN_CASE('<', '=', TOK_LTEQ);
        TWO_CHAR_TOKEN_CASE('>', '=', TOK_GTEQ);
        TWO_CHAR_TOKEN_CASE('-', '>', TOK_RIGHT_ARROW);

        default: {
            if (*lex->stream && std::isprint(*lex->stream)) {
                lex->token.kind = (Token_Kind)*lex->stream;
                lex->stream += 1;
            } else {
                ZFATAL("Unexpected character: '%c', value: '%d'", *lex->stream, *lex->stream);
                return false;
            }
            break;
        }

#undef TWO_CHAR_TOKEN_CASE

    }

    auto length = lex->stream - start;

    if (length) {

            if (lex->token.kind == TOK_STRING) {

                const char *err_char = nullptr;
                String str_lit = convert_escape_characters_to_special_characters(temp_allocator_allocator(), String_Ref(start, length), &err_char);
                if (err_char) {
                    auto range = lex->token.range;
                    report_lex_error(lex, range, "Invalid escape sequence in string literal: '\\%c'", *err_char);
                    return false;
                }

                lex->token.atom = atom_get(&lex->context->atoms, str_lit);

            } else {

                lex->token.atom = atom_get(&lex->context->atoms, start, length);
            }

        if (lex->token.kind == TOK_NAME && is_keyword(lex->token.atom)) {
            lex->token.kind = TOK_KEYWORD;
        }

    } else {
        lex->token.atom = {};
    }

    lex->token.range.start.index_in_line = lex->stream - lex->line_start - length + 1;

    return true;
}

bool is_token(Lexer *lexer, Token_Kind kind)
{
    return lexer->token.kind == kind;
}

bool is_token(Lexer *lexer, char c)
{
    return is_token(lexer, (Token_Kind)c);
}

bool match_token(Lexer *lexer, Token_Kind kind)
{
    if (is_token(lexer, kind)) {
        next_token(lexer);
        return true;
    }

    return false;
}

bool match_token(Lexer *lexer, char c)
{
    return match_token(lexer, (Token_Kind)c);
}

bool expect_token(Lexer *lexer, Token_Kind kind)
{
    if (is_token(lexer, kind)) {
        next_token(lexer);
        return true;
    }

    Token t = lexer->token;
    ZFATAL("Expected token '%s', got: '%s'", tmp_token_kind_str(kind).data,  tmp_token_str(t).data);
    return false;
}

bool expect_token(Lexer *lexer, char c)
{
    return expect_token(lexer, (Token_Kind)c);
}

void print_token(Token token)
{
    auto str = tmp_token_str(token);
    printf("%.*s\n", (int)str.length, str.data);
}

file_local String_Ref token_kind_to_string[TOK_LAST + 1] = {
    [TOK_INVALID] = "INVALID",
    [TOK_INT] = "INT",
    [TOK_REAL] = "REAL",
    [TOK_STRING] = "STRING",
    [TOK_CHAR] = "CHAR",
    [TOK_NAME] = "NAME",
    [TOK_KEYWORD] = "KEYWORD",
    [TOK_RIGHT_ARROW] = "->",
    [TOK_EQ] = "==",
    [TOK_NEQ] = "!=",
    [TOK_LTEQ] = "<=",
    [TOK_GTEQ] = ">=",
    [TOK_EOF] = "EOF",
};

String_Ref tmp_token_str(Token token)
{
    static char buffer[1024];
    i32 length = 0;

    if (token.atom.length > 0) {
        length = string_format(buffer, "%s", token.atom.data);
    } else if (token.kind == TOK_EOF) {
        return token_kind_to_string[token.kind];
    } else {
        assert(false);
    }

    auto buf = buffer;
    if (length == 0) buf = nullptr;
    return String_Ref(buf, length);
}

String_Ref tmp_token_kind_str(Token_Kind kind)
{
    static char buffer[256];
    i32 length = 0;

    if (isprint((char)kind)) {
        length = string_format(buffer, "%c", (char)kind);
    } else {
        return token_kind_to_string[kind];
    }

    auto buf = buffer;
    if (length == 0) buf = nullptr;
    return String_Ref(buf, length);
}

file_local u8 char_to_digit[256] = {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

file_local bool lex_int(Lexer *lexer)
{
    assert(lexer);

    u64 base = 10;
    u64 result = 0;

    auto start = lexer->stream;
    u64 length = 0;

    if (*lexer->stream == '0') {
        lexer->stream += 1;

        if (tolower(*lexer->stream) == 'x') {
            lexer->stream += 1;
            base = 16;
            start = lexer->stream;
        } else if (tolower(*lexer->stream) == 'b') {
            lexer->stream += 1;
            base = 2;
            start = lexer->stream;
        }
    }

    while (true) {

        u64 digit = char_to_digit[(int)*lexer->stream];
        if (digit == 0 && *lexer->stream != '0') {
            break;
        }

        if (digit >= base) {
            ZFATAL("Digit '%c' out of range for base %i", *lexer->stream, base);
            return false;
        }

        if (result > (UINT64_MAX - digit) / base) {
            ZFATAL("Integer literal overflow: '%.*s'", (int)length + 1, start);
            // Skip the rest of this integer
            while (isdigit(*lexer->stream)) lexer->stream += 1;
            return false;
        }

        result = result * base + digit;
        lexer->stream += 1;
        length += 1;
    }

    if (lexer->stream == start) {
        ZFATAL("Expected base %d digit, got '%c'", base, *lexer->stream);
        return false;
    }

    lexer->token.kind = TOK_INT;
    lexer->token.integer = result;

    return true;
}

file_local bool lex_real(Lexer *lexer)
{
    assert(lexer);

    const char *start = lexer->stream;

    // NOTE:
    // The scanning we do here is to leave the stream in the correct position when done.
    // The actual conversion is done by strtod/strtof.

    // All digits before the '.'
    while (isdigit(*lexer->stream)) lexer->stream += 1;

    if (*lexer->stream == '.') lexer->stream += 1;

    // All digits after the '.'
    while (isdigit(*lexer->stream)) lexer->stream += 1;

    // Scientific notation
    if (tolower(*lexer->stream) == 'e') {
        lexer->stream += 1;

        if (*lexer->stream == '+' || *lexer->stream == '-') {
            lexer->stream += 1;
        }

        if (!isdigit(*lexer->stream)) {
            ZFATAL("Expected digit after float literal exponent, found '%c'.", *lexer->stream);
            return false;
        }

        // All digits after 'e' and optional '+'/'-'
        while (isdigit(*lexer->stream)) lexer->stream += 1;
    }

    Real_Value result;
    char *err;

    result.r32 = strtof(start, &err);
    if (result.r32 == 0.0 && err == start) {
        ZFATAL("Convertion to float failed");
        return false;
    }
    if (result.r32 == HUGE_VALF || result.r32 == -HUGE_VALF) {
        ZFATAL("Float literal overflow");
        return false;
    }
    if (result.r32 <= FLT_MIN && errno == ERANGE) {
        ZFATAL("Float literal underflow");
        return false;
    }

    result.r64 = strtod(start, &err);
    if (result.r64 == 0.0 && err == start) {
        ZFATAL("Convertion to double failed");
        return false;
    }
    if (result.r64 == HUGE_VAL || result.r64 == -HUGE_VAL) {
        ZFATAL("Double literal overflow");
        return false;
    }
    if (result.r64 <= DBL_MIN && errno == ERANGE) {
        ZFATAL("Double literal underflow");
        return false;
    }

    lexer->token.kind = TOK_REAL;
    lexer->token.real = result;

    return true;
}

}
