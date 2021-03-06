#pragma once

#include "array.h"
#include "atom.h"
#include "file_pos.h"
#include "operator.h"
#include "struct_predecls.h"

#include <cstdio>

namespace Zodiac
{

enum class PT_Node_Kind
{
    INVALID,

    IDENTIFIER,
    FUNCTION_PROTO,
    PARAMETER,
    EXPRESSION_LIST,

    DECLARATION,

    STATEMENT,

    EXPRESSION,
};

struct Declaration_PTN;
struct Parameter_PTN;
struct Expression_PTN;

typedef PT_Node_Kind PTN_Kind;

typedef uint64_t PTN_Flag;

enum PTN_FLAG : PTN_Flag
{
    PTN_FLAG_NONE               = 0x00,
    PTN_FLAG_DECL_IS_NAKED      = 0x01,
    PTN_FLAG_FUNC_NORETURN      = 0x02,
    PTN_FLAG_FUNC_FOREIGN       = 0x04,
    PTN_FLAG_SEMICOLON          = 0x08,
    PTN_FLAG_FUNC_COMPILER_FUNC = 0x10,
};

struct PT_Node
{
    PT_Node_Kind kind = PT_Node_Kind::INVALID;
    PTN_Flag flags = PTN_FLAG_NONE;

    File_Pos begin_file_pos = {};
    File_Pos end_file_pos = {};
};

typedef PT_Node PTN;

struct Identifier_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Atom atom = {};
};

struct Function_Proto_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Array<Parameter_PTN*> parameters = {};
    Expression_PTN *return_type_expression = nullptr;
};

struct Statement_PTN;
struct Expression_List_PTN;

struct Switch_Case_Expression_PTN
{
    union
    {
        Expression_PTN *expression = nullptr;
        Expression_PTN *range_begin_expr;
    };

    Expression_PTN *range_end_expr = nullptr;
};

struct Switch_Case_PTN
{
    Array<Switch_Case_Expression_PTN> expressions = {};
    Statement_PTN *body = nullptr;
    bool is_default = false;
    bool parse_error = false;

    File_Pos begin_fp = {};
    File_Pos end_fp = {};
};

enum class Statement_PTN_Kind
{
    INVALID,
    BLOCK,
    DECLARATION,
    EXPRESSION,
    RETURN,
    BREAK,
    ASSIGNMENT,
    WHILE,
    FOR,
    FOREACH,
    IF,
    SWITCH,
};

struct Statement_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Statement_PTN_Kind kind = Statement_PTN_Kind::INVALID;

    union
    {
        Expression_PTN *expression;
        Declaration_PTN *declaration;

        struct
        {
            Array<Statement_PTN*> statements = {};
        } block;

        struct
        {
            Expression_PTN *expression;
        } return_stmt;

        struct
        {
            Expression_PTN *ident_expression;
            Expression_PTN *rhs_expression;
        } assignment;

        struct
        {
            Expression_PTN *cond_expr;
            Statement_PTN *body;
        } while_stmt;

        struct
        {
            Statement_PTN *init_stmt;
            Expression_PTN *cond_expr;
            Statement_PTN *step_stmt;
            Statement_PTN *body_stmt;
        } for_stmt;

        struct
        {
            Identifier_PTN *it_identifier;
            Identifier_PTN *it_index_identifier;
            Expression_PTN *array_expression;
            Statement_PTN *body_stmt;
            bool it_is_pointer;
        } foreach;

        struct
        {
            Expression_PTN *cond_expr;
            Statement_PTN *then_stmt;
            Statement_PTN *else_stmt;
        } if_stmt;

        struct
        {
            Expression_PTN *expression;
            Array<Switch_Case_PTN> cases;
            bool has_default_case;
            bool allow_incomplete;

        } switch_stmt;
    };

    Statement_PTN() {}
};

enum class Declaration_PTN_Kind
{
    INVALID,

    IMPORT,
    USING,

    VARIABLE,
    CONSTANT,

    FUNCTION,
    STRUCT,
    UNION,
    ENUM,
    TYPEDEF,

    RUN,
    STATIC_IF,
    STATIC_ASSERT,
};

struct Declaration_PTN
{
    static PTN_Kind _kind;
    PT_Node self = {};
    Declaration_PTN_Kind kind = Declaration_PTN_Kind::INVALID;

    Identifier_PTN *identifier = nullptr;

    union
    {
        struct
        {
            Expression_PTN *module_ident_expr;
        } import;

        struct
        {
            Expression_PTN *ident_expr;
        } using_decl;

        struct
        {
            Expression_PTN *init_expression;
            Expression_PTN *type_expression;
        } variable;

        struct
        {
            Atom module_name = {};
            Function_Proto_PTN *prototype;
            Statement_PTN *body;
        } function;

        struct
        {
            Array<Declaration_PTN *> member_declarations;
            Array<Parameter_PTN *> parameters;
            Array<Identifier_PTN *> usings;
        } structure, union_decl;

        struct
        {
            Expression_PTN *type_spec;
            Array<PTN *> members;
        } enum_decl;

        struct
        {
            Expression_PTN *type_expression;
        } typedef_decl;

        struct
        {
            Expression_PTN *type_expression;
            Expression_PTN *init_expression;
        } constant;

        struct
        {
            Expression_PTN *expression;
        } run;

        struct
        {
            Expression_PTN *cond_expression;
            Array<Declaration_PTN *> then_declarations;
            Array<Declaration_PTN *> else_declarations;
        } static_if;

        struct
        {
            Expression_PTN *cond_expression;
        } static_assert_decl;

        struct
        {
            Identifier_PTN *identifier;
            Statement_PTN *body;
        } test;
    };

    Declaration_PTN() {}
};

struct Expression_List_PTN
{
    static PT_Node_Kind _kind;
    PT_Node self = {};
    Array<Expression_PTN*> expressions = {};
};

struct Parameter_PTN
{
    static PT_Node_Kind _kind;
    PT_Node self = {};

    Identifier_PTN *identifier = nullptr;
    Expression_PTN *type_expression = nullptr;
};

enum class Expression_PTN_Kind
{
    INVALID,

    CALL,
    IDENTIFIER,
    BINARY,
    UNARY,
    DOT,
    COMPOUND,
    SUBSCRIPT,

    INTEGER_LITERAL,
    FLOAT_LITERAL,
    STRING_LITERAL,
    CHAR_LITERAL,
    BOOL_LITERAL,
    NULL_LITERAL,

    ARRAY_TYPE,
    POINTER_TYPE,
    POLY_TYPE,
    FUNCTION_TYPE,
};

struct Expression_PTN
{
    static PT_Node_Kind _kind;
    PT_Node self = {};

    Expression_PTN_Kind kind = Expression_PTN_Kind::INVALID;

    union
    {
        Identifier_PTN *identifier;

        struct
        {
            bool is_builtin;
            Expression_PTN *ident_expression;
            Expression_List_PTN *arg_list;
        } call;

        struct
        {
            Binary_Operator op = BINOP_INVALID;
            Expression_PTN *lhs;
            Expression_PTN *rhs;
        } binary;

        struct
        {
            Unary_Operator op = UNOP_INVALID;
            Expression_PTN *operand_expression;
        } unary;

        Integer_Literal integer_literal;

        Float_Literal float_literal;

        struct
        {
            Atom atom;
        } string_literal;

        struct
        {
            char c;
        } char_literal;

        struct
        {
            bool value;
        } bool_literal;

        struct
        {
            Expression_PTN *parent_expression;
            Identifier_PTN *child_identifier;
        } dot;

        struct
        {
            Expression_List_PTN *list;
            Expression_PTN *type_expression;
        } compound;

        struct
        {
            Expression_PTN *pointer_expression;
            Expression_PTN *index_expression;
        } subscript;

        struct
        {
            Expression_PTN *length_expression;
            Expression_PTN *element_type_expression;
        } array_type;

        struct
        {
            Expression_PTN *pointee_type_expression;
        } pointer_type;

        struct
        {
            Identifier_PTN *identifier;
            Identifier_PTN *specification_identifier;
        } poly_type;

        Function_Proto_PTN *function_proto;
    };

    Expression_PTN() {}
};

void init_ptn(PTN *ptn, PTN_Kind kind, const File_Pos &begin_file_pos,
              const File_Pos &end_file_pos);

template <typename T>
T *new_ptn(Allocator *allocator, const File_Pos &begin_file_pos, const File_Pos &end_file_pos)
{
    T *result = alloc_type<T>(allocator);
    init_ptn(&result->self, T::_kind, begin_file_pos, end_file_pos);
    return result;
}

void free_ptn(Allocator *allocator, PTN *ptn);
void free_ptn(Allocator *allocator, Declaration_PTN *ptn);
void free_ptn(Allocator *allocator, Function_Proto_PTN *ptn);
void free_ptn(Allocator *allocator, Parameter_PTN *ptn);
void free_ptn(Allocator *allocator, Statement_PTN *ptn);
void free_ptn(Allocator *allocator, Expression_PTN *ptn);
void free_ptn(Allocator *allocator, Expression_List_PTN *ptn);
void free_ptn(Allocator *allocator, Identifier_PTN *ptn);

Statement_PTN *new_statement(Allocator *allocator, Statement_PTN_Kind kind,
                             const File_Pos &begin_fp, const File_Pos &end_fp);

Identifier_PTN *new_identifier_ptn(Allocator *allocator, const Atom& atom,
                                   const File_Pos &begin_fp, const File_Pos &end_fp);

Statement_PTN *new_block_statement_ptn(Allocator *allocator, Array<Statement_PTN*> statements,
                                       const File_Pos &begin_fp, const File_Pos &end_fp);
Statement_PTN *new_expression_statement_ptn(Allocator *allocator, Expression_PTN *expr,
                                            const File_Pos &begin_fp, const File_Pos &end_fp);
Statement_PTN *new_declaration_statement_ptn(Allocator *allocator, Declaration_PTN *decl,
                                             const File_Pos &begin_fp, const File_Pos &end_fp);
Statement_PTN *new_return_statement_ptn(Allocator *allocator, Expression_PTN *expr,
                                        const File_Pos &begin_fp, const File_Pos &end_fp);
Statement_PTN *new_break_statement_ptn(Allocator *allocator, const File_Pos &begin_fp,
                                       const File_Pos &end_fp);
Statement_PTN *new_assignment_statement_ptn(Allocator *allocator, Expression_PTN *ident_expression,
                                            Expression_PTN *rhs_expression,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp);
Statement_PTN *new_while_statement_ptn(Allocator *allocator, Expression_PTN *while_expr,
                                       Statement_PTN *while_body, const File_Pos &begin_fp,
                                       const File_Pos &end_fp);

Statement_PTN *new_for_statement_ptn(Allocator *allocator, Statement_PTN *init_stmt,
                                     Expression_PTN *cond_expr, Statement_PTN *step_stmt,
                                     Statement_PTN *body_stmt,
                                     const File_Pos &begin_fp, const File_Pos &end_fp);

Statement_PTN *new_foreach_statement_ptn(Allocator *allocator, Identifier_PTN *it_ident,
                                         bool it_is_pointer,
                                         Identifier_PTN *it_idx_ident,
                                         Expression_PTN *array_expr,
                                         Statement_PTN *body_stmt,
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp);

Statement_PTN *new_if_statement_ptn(Allocator *allocator, Expression_PTN *cond_expr,
                                    Statement_PTN *then_stmt, Statement_PTN *else_stmt,
                                    const File_Pos &begin_fp, const File_Pos &end_fp);
Statement_PTN *new_switch_statement_ptn(Allocator *allocator, Expression_PTN *expression,
                                        Array<Switch_Case_PTN> cases,
                                        bool has_default_case,
                                        bool allow_incomplete,
                                        const File_Pos &begin_fp,
                                        const File_Pos &end_fp);

Function_Proto_PTN *new_function_prototype_parse_tree_node(Allocator *allocator,
                                                           Array<Parameter_PTN*> parameters,
                                                           Expression_PTN *return_type_expr,
                                                           const File_Pos &begin_fp,
                                                           const File_Pos &end_fp);

Declaration_PTN *new_import_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                            Expression_PTN *module_ident_expr,
                                              const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_function_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                              Atom module_name, Function_Proto_PTN *prototype,
                                              Statement_PTN *body,
                                              const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_variable_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                              Expression_PTN *type_expression,
                                              Expression_PTN *init_expression,
                                              const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_struct_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                            Array<Declaration_PTN *> members,
                                            Array<Parameter_PTN *> parameters,
                                            Array<Identifier_PTN *> usings,
                                            const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_union_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                            Array<Declaration_PTN*> members,
                                            Array<Parameter_PTN*> parameters,
                                            const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_enum_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                          Expression_PTN *type_spec_expr,
                                          Array<PTN*> members, const File_Pos &begin_fp,
                                          const File_Pos &end_fp);

Declaration_PTN *new_typedef_declaration_ptn(Allocator *allocator,
                                             Identifier_PTN *identifier,
                                             Expression_PTN *type_expr,
                                             const File_Pos &begin_fp,
                                             const File_Pos &end_fp);

Declaration_PTN *new_constant_declaration_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                              Expression_PTN *type_expr,
                                              Expression_PTN *init_expr,
                                              const File_Pos &begin_fp, const File_Pos &enf_fp);


Declaration_PTN *new_using_declaration_ptn(Allocator *allocator, Expression_PTN *ident_expr,
                                           const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_run_declaration_ptn(Allocator *allocator, Expression_PTN *run_expr,
                                         const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_static_if_declaration_ptn(Allocator *allocator,
                                               Expression_PTN *cond_expr,
                                               Array<Declaration_PTN *> then_decls,
                                               Array<Declaration_PTN *> else_decls,
                                               const File_Pos &begin_fp, const File_Pos &end_fp);

Declaration_PTN *new_static_assert_declaration_ptn(Allocator *allocator,
                                                   Expression_PTN *cond_expr,
                                                   const File_Pos bfp, const File_Pos &efp);

Expression_List_PTN *new_expression_list_ptn(Allocator *allocator,
                                             Array<Expression_PTN*> expressions,
                                             const File_Pos &begin_fp, const File_Pos &end_fp);

Expression_PTN *new_call_expression_ptn(Allocator *allocator, bool is_builtin,
                                        Expression_PTN *ident_expr, Expression_List_PTN *arg_list,
                                        const File_Pos &begin_file_pos,
                                        const File_Pos &end_file_pos);

Expression_PTN *new_identifier_expression_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                              const File_Pos &begin_file_pos,
                                              const File_Pos &end_file_pos);

Expression_PTN *new_binary_expression_ptn(Allocator *allocator, Binary_Operator op,
                                          Expression_PTN *lhs, Expression_PTN *rhs,
                                          const File_Pos &begin_file_pos,
                                          const File_Pos &end_file_pos);

Expression_PTN *new_unary_expression_ptn(Allocator *allocator, Unary_Operator op,
                                         Expression_PTN *operand_expression,
                                         const File_Pos &begin_file_pos,
                                         const File_Pos &end_file_pos);

Expression_PTN *new_number_literal_expression_ptn(Allocator *allocator, Atom atom,
                                                  const File_Pos &begin_file_pos,
                                                  const File_Pos &end_file_pos);
Expression_PTN *new_string_literal_expression_ptn(Allocator *allocator, Atom atom,
                                                  const File_Pos &begin_file_pos,
                                                  const File_Pos &end_file_pos);
Expression_PTN *new_char_literal_expression_ptn(Allocator *allocator, char c,
                                                const File_Pos &begin_file_pos,
                                                const File_Pos &end_file_pos);
Expression_PTN *new_boolean_literal_expression_ptn(Allocator *allocator, bool value,
                                                   const File_Pos &begin_file_pos,
                                                   const File_Pos &end_file_pos);
Expression_PTN *new_null_literal_expression_ptn(Allocator *allocator,
                                                const File_Pos &begin_file_pos,
                                                const File_Pos &end_file_pos);
Expression_PTN *new_dot_expression_ptn(Allocator *allocator, Expression_PTN *parent,
                                       Identifier_PTN *child_ident,
                                       const File_Pos &begin_file_pos,
                                       const File_Pos &end_file_pos);
Expression_PTN *new_compound_expression_ptn(Allocator *allocator,
                                            Expression_List_PTN *expr_list,
                                            Expression_PTN *type_expression,
                                            const File_Pos &begin_file_pos,
                                            const File_Pos &end_file_pos);
Expression_PTN *new_subscript_expression_ptn(Allocator *allocator,
                                             Expression_PTN *pointer_expression,
                                             Expression_PTN *index_expression,
                                             const File_Pos &begin_fp,
                                             const File_Pos &end_fp);
Expression_PTN *new_array_type_expression_ptn(Allocator *allocator,
                                              Expression_PTN *length_expression,
                                              Expression_PTN *element_type_expression,
                                              const File_Pos &begin_file_pos,
                                              const File_Pos &end_file_pos);
Expression_PTN *new_pointer_type_expression_ptn(Allocator *allocator,
                                                Expression_PTN *pointee_type_expression,
                                                const File_Pos &begin_file_pos,
                                                const File_Pos &end_file_pos);
Expression_PTN *new_poly_type_expression_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                             Identifier_PTN *specification_identifier,
                                             const File_Pos &begin_file_pos,
                                             const File_Pos &end_file_pos);
Expression_PTN *new_function_type_expression_ptn(Allocator *allocator,
                                                 Function_Proto_PTN *function_proto,
                                                 const File_Pos &begin_fp,
                                                 const File_Pos &end_fp);

Parameter_PTN *new_parameter_ptn(Allocator *allocator, Identifier_PTN *identifier,
                                 Expression_PTN *type_expression,
                                 const File_Pos &begin_file_pos, const File_Pos &end_file_pos);

Switch_Case_Expression_PTN create_switch_case_expression_ptn(Expression_PTN *range_begin_expr,
                                                             Expression_PTN *range_end_expr);

typedef uint64_t PTN_Copy_Flags;
enum PTN_Copy_Flag__ : PTN_Copy_Flags
{
    PTNC_FLAG_NONE                  = 0x00,
    PTNC_FLAG_DONT_COPY_IDENTIFIERS = 0x01,
    PTNC_FLAG_DONT_COPY_EXPRESSIONS = 0x02,
};

Declaration_PTN *copy_declaration_ptn(Allocator *allocator, Declaration_PTN *decl,
                                      PTN_Copy_Flags flags = PTNC_FLAG_NONE);
Expression_PTN *copy_expression_ptn(Allocator *allocator, Expression_PTN *expr,
                                    PTN_Copy_Flags flags = PTNC_FLAG_NONE);
Identifier_PTN *copy_identifier_ptn(Allocator *allocator, Identifier_PTN *identifier);

void print_indent(uint64_t indent);
void print_ptn(PTN *ptn, uint64_t indent);
void print_statement_ptn(Statement_PTN *statement, uint64_t indent, bool newline = true);
void print_declaration_ptn(Declaration_PTN *declaration, uint64_t indent, bool newline = true);
void print_expression_ptn(Expression_PTN *expression, uint64_t indent);

}
