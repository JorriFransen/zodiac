
#pragma once

#include "allocator.h"
#include "array.h"
#include "atom.h"
#include "build_data.h"
#include "file_pos.h"
#include "hash_table.h"
#include "token.h"
#include "token_stream.h"
#include "zodiac_string.h"

using namespace Zodiac;

struct Lexer
{
    Build_Data *build_data = nullptr;
    Allocator *allocator = nullptr;
};

struct Lexed_File
{
    String path = {};
    Array<Token> tokens = {};
    bool valid = false;
};

struct Lexer_Data
{
    Lexer *lexer = nullptr;
    String file_path = {};
    String file_data = {};
    uint64_t file_index = 0;
    uint64_t file_size = 0;

    uint64_t current_line = 1;
    uint64_t current_column = 1;

    Lexed_File lexed_file = {};
};

struct Lexed_File_Token_Stream : public Token_Stream
{
    Lexed_File *lexed_file = nullptr;
    int64_t current_index = 0;

    Lexed_File_Token_Stream(){}
    ~Lexed_File_Token_Stream(){}

    Token current_token();
    Token next_token();
    Token peek_token(uint64_t offset);

    void free() {};
};

Lexer lexer_create(Allocator *allocator, Build_Data *build_data);
void lexer_init(Allocator *allocator, Build_Data *build_data, Lexer *lexer);
Lexed_File lexer_lex_file(Lexer *lexer, const String& file_path);
void lexer_free_lexed_file(Lexer *lexer, Lexed_File *lexed_file);

Lexer_Data lexer_data_create(Lexer *lexer, String file_path, String file_data, uint64_t file_size);

Token next_token(Lexer_Data *ld);

Token lex_keyword_or_identifier(Lexer_Data *ld);
Token lex_identifier(Lexer_Data *ld);
Token lex_number_literal(Lexer_Data *ld);
Token lex_character_literal(Lexer_Data *ld);
Token lex_string_literal(Lexer_Data *ld);

void advance(Lexer_Data *ld, uint64_t count = 1);

char current_char(Lexer_Data *ld);
char peek_char(Lexer_Data *ld, uint64_t offset);

const char *current_char_ptr(Lexer_Data *ld);

void skip_whitespace(Lexer_Data *ld);

bool is_alpha(char c);
bool is_alpha_num(char c);
bool is_num(char c);

bool is_whitespace(char c);
bool is_newline(char c);

File_Pos get_file_pos(Lexer_Data *ld);

String lexer_replace_character_literals(Allocator *allocator, const char *str, int64_t length);
char lexer_get_escape_char(char ident);

void lexed_file_print(Lexed_File *lf);

Token_Stream *lexer_new_token_stream(Allocator *allocator, Lexed_File *lf);
