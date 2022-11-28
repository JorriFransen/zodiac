
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <lexer.h>

using namespace Zodiac;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Lexer lexer;
    lexer_create("1+2-_VarName*another_var()", &lexer);

    while (!(is_token(&lexer, TOK_EOF) || is_token(&lexer, TOK_INVALID))) {
        print_token(lexer.token);
        next_token(&lexer);
    }
    print_token(lexer.token);

    lexer_destroy(&lexer);

    return 0;
}

