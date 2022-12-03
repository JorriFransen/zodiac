#include "lexer.h"

#include <logger.h>
#include <memory/zmemory.h>

#include <cfloat>
#include <cmath>
#include <cstdlib>

namespace Zodiac
{

file_local bool lex_int(Lexer *lexer);
file_local bool lex_real(Lexer *lexer);

// Emit variable definition for all keywords
#define ZODIAC_KEYWORD(n) Atom keyword_##n;
ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

// Emit array with all keywords
file_local Atom *all_keywords[] = {
#define ZODIAC_KEYWORD(n) &(keyword_##n),
    ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD
};

void zodiac_register_keywords(Atom_Table *at)
{
    Atom_Block *block = at->current_block;

    // Emit init for all keywords
#define ZODIAC_KEYWORD(n) keyword_##n = atom_get(at, #n);
    ALL_ZODIAC_KEYWORDS
#undef ZODIAC_KEYWORD

    assert_msg(block == at->current_block, "Expected all keyword atoms to fit in the same block...");
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

    out_lexer->token.kind = TOK_INVALID;
}

void lexer_init_stream(Lexer *lexer, const char *stream)
{
    assert(lexer && lexer->context && stream);

    lexer->stream_start = stream;
    lexer->stream = stream;

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

        case ' ': case '\n': case '\t': {
            lex->stream += 1;
            while (isspace(*lex->stream)) {
                lex->stream += 1;
            }

            goto next_token__start_lexing_token;
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
        lex->token.atom = atom_get(&lex->context->atoms, start, length);

        if (lex->token.kind == TOK_NAME && is_keyword(lex->token.atom)) {
            lex->token.kind = TOK_KEYWORD;
        }

    } else {
        lex->token.atom = {};
    }

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

    ZFATAL("Expected token %s, '%c'", token_kind_str(kind), (char)kind);
    return false;
}

bool expect_token(Lexer *lexer, char c)
{
    return expect_token(lexer, (Token_Kind)c);
}

void print_token(Token token)
{
    auto str = tmp_token_string(token);
    printf("%.*s\n", (int)str.length, str.data);
}

String_Ref tmp_token_string(Token token)
{
    static char buffer[1024];

    i32 length;

    const char *name = token_kind_str(token.kind);

    if (token.atom.length > 0) {
        length = string_format(buffer, "%s '%.*s'", name, (int)token.atom.length, token.atom.data);
    } else {
        length = string_format(buffer, "%s", name);
    }

    // Optionally print value
    switch (token.kind) {
        case TOK_INT: {
            length = string_format(buffer, "%s, %llu", buffer, token.integer);
            break;
        }

        case TOK_REAL: {
            length = string_format(buffer, "%s, %f", buffer, token.real.r64);
            break;
        }

        default: break;
    }

    return String_Ref(buffer, length);
}

const char *token_kind_str(Token_Kind kind)
{
    switch (kind) {
        case TOK_INVALID: return "<INVALID>";
        case TOK_INT: return "<INT>";
        case TOK_REAL: return "<REAL>";
        case TOK_NAME: return "<NAME>";
        case TOK_KEYWORD: return "<KEYWORD>";
        case TOK_EQ: return "<TOK_EQ>";
        case TOK_NEQ: return "<TOK_NEQ>";
        case TOK_LTEQ: return "<TOK_LTEQ>";
        case TOK_GTEQ: return "<TOK_GTEQ>";
        case TOK_EOF: return "<EOF>";

        default:
        {
            if (kind < 128 && isprint(kind)) {
                return "<ASCII>";
            } else {
                assert_msg(false, "Unhandled token kind in token_kind_str()");
                return nullptr;
            }
        }
    }
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
