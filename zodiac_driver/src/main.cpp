
#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

#include <ast.h>
#include <lexer.h>

#include <memory/pool_allocator.h>

using namespace Zodiac;

// file_local AST_Expression *parse_expr3();
// file_local AST_Expression *parse_expr2();
// file_local AST_Expression *parse_expr1();
// file_local AST_Expression *parse_expression();

Lexer *lxr;
Allocator *expression_allocator;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;

    Zodiac_Context c;
    zodiac_context_create(&c);

    Lexer lexer;
    lexer_create(&c, &lexer);
    lexer_init_stream(&lexer, "1 + 2 * -3"); // -5
    // lexer_init_stream(&lexer, "(1 + 2) * -3"); // -9


    // lxr = &lexer;
    // auto result = parse_expression();
    // ZINFO("Result: %i", result);

    return 0;
}

// expr3 = INT | '(' expr ')'
// expr2 = ([+-] expr2) | expr3
// expr1 = expr2 ([/*] expr2)*
// expr = expr1 ([+-] expr1)*

// file_local int parse_expr3()
// {
//     if (is_token(lxr, TOK_INT)) {
//         int result = lxr->token.integer;
//         next_token(lxr);
//         return result;
//     } else {

//         if (match_token(lxr, '(')) {
//             auto result = parse_expression();
//             expect_token(lxr, ')');
//             return result;
//         } else {
//             ZFATAL("Expected '('");
//             return 0;
//         }

//     }
// }

// file_local int parse_expr2()
// {
//     if (is_token(lxr, '+')) {
//         next_token(lxr);
//         return parse_expr2();
//     } else if (is_token(lxr, '-')) {
//         next_token(lxr);
//         return -parse_expr2();
//     } else {
//         return parse_expr3();
//     }
// }

// file_local int parse_expr1()
// {
//     auto lhs = parse_expr2();

//     while (is_token(lxr, '/') || is_token(lxr, '*')) {
//         char op = lxr->token.kind;
//         next_token(lxr);

//         auto rhs = parse_expr2();

//         if (op == '/') lhs = lhs / rhs;
//         else lhs = lhs * rhs;
//     }

//     return lhs;
// }

// file_local int parse_expression()
// {
//     auto lhs = parse_expr1();

//     while (is_token(lxr, '+') || is_token(lxr, '-')) {
//         char op = lxr->token.kind;
//         next_token(lxr);

//         auto rhs = parse_expr1();

//         if (op == '+') lhs = lhs + rhs;
//         else lhs = lhs - rhs;

//     }

//     return lhs;
// }
