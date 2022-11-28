#include "lexer.h"

#include <memory/zmemory.h>

namespace Zodiac
{

void lexer_create(const char *stream, Lexer *out_lexer)
{
    out_lexer->stream_start = stream;
    out_lexer->stream = stream;

    out_lexer->token.kind = TOK_INVALID;

    next_token(out_lexer);
}

void lexer_destroy(Lexer *lexer)
{
    zzeromem(lexer, sizeof(Lexer));
}

void next_token(Lexer *lex)
{
    assert(lex);

    lex->token.kind = TOK_INVALID;
    lex->token.start = lex->stream;

    switch (*lex->stream) {

        case 0: {
            lex->token.kind = TOK_EOF;
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
            lex->token.kind = (Token_Kind)*lex->stream;
            lex->stream += 1;
            break;
        }
    }

    lex->token.end = lex->stream;
}

bool is_token(Lexer *lexer, Token_Kind kind)
{
    return lexer->token.kind == kind;
}

void print_token(Token token)
{
    const char *name = token_kind_str(token.kind);
    int length = token.end - token.start;

    if (length) printf("%s '%.*s'\n", name, length, token.start);
    else printf("%s\n", name);
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
