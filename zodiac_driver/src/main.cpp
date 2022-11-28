
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

using namespace Zodiac;

enum Token_Kind
{
    TOK_INVALID = 0,
    // Ascii range
    TOK_NUMBER = 128,
    TOK_NAME,
    TOK_KEYWORD,
    TOK_EOF,
};

struct Token
{
    Token_Kind kind;

    const char *start;
    const char *end;

    union
    {
        Integer_Value number;
    };
};

file_local void init_lexer_stream(const String_Ref stream_data);
file_local void next_token();
file_local bool is_token(Token_Kind kind);
file_local void print_token(Token token);
file_local const char *token_kind_str(Token_Kind kind);

file_local const char *token_stream;
file_local Token token = { .kind = TOK_INVALID };

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    ZTRACE("Systems init done");

    init_lexer_stream("1+2-_VarName*another_var()");

    while (!(is_token(TOK_EOF) || is_token(TOK_INVALID))) {
        print_token(token);
        next_token();
    }
    print_token(token);

    return 0;
}

file_local void init_lexer_stream(const String_Ref stream_data)
{
    token_stream = stream_data.data;
    next_token();
}

file_local void next_token()
{
    token.start = token_stream;

    switch (*token_stream) {

        case 0: {
            token.kind = TOK_EOF;
            break;
        }

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
            token.kind = TOK_NUMBER;
            Integer_Value value = {};
            while (isdigit(*token_stream)) {
                value.u64 *= 10;
                value.u64 += *token_stream - '0';
                token_stream += 1;
            }
            token.number = value;
            break;
        }

        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '_':
        {
            token.kind = TOK_NAME;
            while (isalnum(*token_stream) || *token_stream == '_') {
                token_stream += 1;
            }
            break;
        }

        default: {
            token.kind = (Token_Kind)*token_stream;
            token_stream += 1;
            break;
        }
    }

    token.end = token_stream;
}

file_local bool is_token(Token_Kind kind)
{
    return token.kind == kind;
}

file_local void print_token(Token token)
{
    const char *name = token_kind_str(token.kind);
    int length = token.end - token.start;

    if (length) printf("%s '%.*s'\n", name, length, token.start);
    else printf("%s\n", name);
}

file_local const char *token_kind_str(Token_Kind kind)
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

