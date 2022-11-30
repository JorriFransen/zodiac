#include "lexer.h"

#include <logger.h>
#include <memory/zmemory.h>

namespace Zodiac
{

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

void next_token(Lexer *lex)
{
    assert(lex && lex->stream_start && lex->stream);

next_token__start_lexing_token:
    lex->token.kind = TOK_INVALID;
    auto start = lex->stream;

    switch (*lex->stream) {

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

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        {
            lex->token.kind = TOK_NUMBER;
            Integer_Value value = {};
            while (isdigit(*lex->stream)) {
                value.u64 *= 10;
                value.u64 += *lex->stream - '0';
                lex->stream += 1;
            }
            lex->token.number = value;
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

        default: {
            if (*lex->stream && std::isprint(*lex->stream)) {
                lex->token.kind = (Token_Kind)*lex->stream;
                lex->stream += 1;
            } else {
                ZFATAL("Unexpected character: '%c', value: '%d'", *lex->stream, *lex->stream);
            }
            break;
        }
    }

    auto length = lex->stream - start;
    if (length) lex->token.atom = atom_get(&lex->context->atoms, start, length);
    else lex->token.atom = {};
}

bool is_token(Lexer *lexer, Token_Kind kind)
{
    return lexer->token.kind == kind;
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

    if (token.atom.length >= 0) {
        length = string_format(buffer, "%s '%.*s'", name, (int)token.atom.length, token.atom.data);
    } else {
        length = string_format(buffer, "%s", name);
    }

    return String_Ref(buffer, length);
}

const char *token_kind_str(Token_Kind kind)
{
    switch (kind) {
        case TOK_INVALID: return "<INVALID>";
        case TOK_NUMBER: return "<NUMBER>";
        case TOK_NAME: return "<NAME>";
        case TOK_KEYWORD: return "<KEYWORD>";
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

}
