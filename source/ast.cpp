
#include "ast.h"

#include "builtin.h"
#include "c_allocator.h"
#include "parser.h"
#include "parse_tree_node.h"
#include "scope.h"
#include "string_builder.h"
#include "temp_allocator.h"

#include <inttypes.h>

#include <tracy/Tracy.hpp>

namespace Zodiac
{

    AST_Node_Kind AST_Module::_kind = AST_Node_Kind::MODULE;
    AST_Node_Kind AST_Identifier::_kind = AST_Node_Kind::IDENTIFIER;
    AST_Node_Kind AST_Declaration::_kind = AST_Node_Kind::DECLARATION;
    AST_Node_Kind AST_Switch_Case::_kind = AST_Node_Kind::SWITCH_CASE;
    AST_Node_Kind AST_Statement::_kind = AST_Node_Kind::STATEMENT;
    AST_Node_Kind AST_Expression::_kind = AST_Node_Kind::EXPRESSION;
    AST_Node_Kind AST_Type_Spec::_kind = AST_Node_Kind::TYPE_SPEC;
    AST_Node_Kind AST_Type::_kind = AST_Node_Kind::TYPE;

    void ast_node_init(AST_Node *ast_node, AST_Node_Kind kind, Scope *scope,
                       const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(ast_node);
        ast_node->kind = kind;
        ast_node->scope = scope;
        ast_node->waiting_on = nullptr;
        ast_node->begin_file_pos = begin_fp;
        ast_node->end_file_pos = end_fp;
    }

    AST_Module *ast_create_from_parsed_file(AST_Builder *ast_builder,
                                            Parsed_File *parsed_file,
                                            Scope *global_scope)
    {
        ZoneScoped

        assert(parsed_file);

        Declarations global_decls = {};
        bucket_array_init(ast_builder->allocator, &global_decls);

        auto begin_fp = parsed_file->begin_file_pos;
        auto end_fp = parsed_file->end_file_pos;


        Scope *module_scope = scope_new(ast_builder->allocator, Scope_Kind::MODULE,
                                        global_scope, parsed_file->declarations.count);

        AST_Module *ast_module = ast_module_new(ast_builder->allocator, parsed_file->name,
                                                module_scope, begin_fp, end_fp);

        bool valid = true;

        assert(parsed_file->declarations.count);
        for (int64_t i = 0; i < parsed_file->declarations.count; i++)
        {
            AST_Declaration *ast_decl =
                ast_create_declaration_from_ptn(ast_builder,
                                                parsed_file->declarations[i], nullptr,
                                                module_scope, ast_module);

            if (!ast_decl) {
                valid = false;
                // @TODO: @CLEANUP: Why don't we just break or return null here?
                continue;
            }

            ast_decl->decl_flags |= AST_DECL_FLAG_GLOBAL;
            assert(ast_decl);
            bucket_array_add(&global_decls, ast_decl);
        }


        if (!valid) return nullptr;

        assert(global_decls.count);

        auto dl = bucket_array_first(&global_decls);
        while (dl.bucket) {
            AST_Declaration **p_decl = bucket_locator_get_ptr(dl);

            ast_flatten_declaration(ast_builder, p_decl);

            bucket_locator_advance(&dl);
        }

        ast_module->declarations = global_decls;
        ast_module->module_scope = module_scope;

        return ast_module;
    }

    AST_Declaration *ast_create_declaration_from_ptn(AST_Builder *ast_builder,
                                                     Declaration_PTN *ptn,
                                                     Array<AST_Declaration*> *var_decls,
                                                     Scope *parent_scope,
                                                     AST_Module *ast_module)
    {
        assert(parent_scope);

        AST_Identifier *ast_ident = nullptr;
        File_Pos begin_fp;

        if (ptn->identifier) {
            assert(ptn->identifier);
            ast_ident = ast_create_identifier_from_ptn(ast_builder, ptn->identifier,
                                                       parent_scope);
            assert(ast_ident);
            begin_fp = ast_ident->begin_file_pos;
        } else {
            begin_fp = ptn->self.begin_file_pos;
        }

        AST_Declaration *result = nullptr;

        bool add_to_scope = true;

        switch (ptn->kind) {
            case Declaration_PTN_Kind::INVALID: assert(false);

            case Declaration_PTN_Kind::IMPORT: {
                auto ast_ident_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->import.module_ident_expr,
                                                   parent_scope);
                assert(ast_ident_expr);

                auto end_fp = ast_ident_expr->end_file_pos;

                result =  ast_import_declaration_new(ast_builder->allocator, ast_ident,
                                                     ast_ident_expr, parent_scope,
                                                     begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::USING: {
                auto import_ident_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->using_decl.ident_expr,
                                                   parent_scope);
                assert(import_ident_expr);

                auto end_fp = ptn->self.end_file_pos;

                result = ast_using_declaration_new(ast_builder->allocator, import_ident_expr,
                                                    parent_scope, begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::VARIABLE: {
                AST_Type_Spec *type_expr = nullptr;
                AST_Expression *init_expr = nullptr;

                if (ptn->variable.type_expression) {
                    type_expr =
                        ast_create_type_spec_from_ptn(ast_builder,
                                                      &ptn->variable.type_expression->self,
                                                      parent_scope);
                    assert(type_expr);
                }

                if (ptn->variable.init_expression) {
                    init_expr =
                        ast_create_expression_from_ptn(ast_builder,
                                                       ptn->variable.init_expression,
                                                       parent_scope, type_expr);
                    assert(init_expr);
                }

                assert(type_expr || init_expr);

                File_Pos end_fp;
                if (init_expr) end_fp = init_expr->end_file_pos;
                else end_fp = type_expr->end_file_pos;

                result = ast_variable_declaration_new(ast_builder->allocator, ast_ident,
                                                      type_expr, init_expr, parent_scope,
                                                      begin_fp, end_fp);
                if (var_decls) {
                    array_append(var_decls, result);
                }

                break;
            }

            case Declaration_PTN_Kind::CONSTANT: {
                AST_Type_Spec *ast_type = nullptr;
                if (ptn->constant.type_expression) {
                    ast_type =
                        ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                 ptn->constant.type_expression,
                                                                 parent_scope);
                    assert(ast_type);
                }

                AST_Expression *ast_init_expr = nullptr;
                if (ptn->constant.init_expression) {
                    ast_init_expr =
                        ast_create_expression_from_ptn(ast_builder,
                                                       ptn->constant.init_expression,
                                                       parent_scope, ast_type);
                    assert(ast_init_expr);
                }

                assert(ast_type || ast_init_expr);

                File_Pos end_fp;
                if (ast_init_expr) end_fp = ast_init_expr->end_file_pos;
                else end_fp = ast_type->end_file_pos;

                result = ast_constant_declaration_new(ast_builder->allocator, ast_ident,
                                                      ast_type, ast_init_expr, parent_scope,
                                                      begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::FUNCTION: {
                AST_Type_Spec *ast_type =
                    ast_create_type_spec_from_ptn(ast_builder, &ptn->function.prototype->self,
                                                  parent_scope);
                assert(ast_type);

                assert(ast_type->kind == AST_Type_Spec_Kind::FUNCTION);
                assert(ptn->function.prototype->parameters.count ==
                       ast_type->function.parameter_type_specs.count);

                auto func_parent_scope = parent_scope;
                if (func_parent_scope->kind != Scope_Kind::MODULE) {

                    while (func_parent_scope->kind != Scope_Kind::MODULE &&
                           func_parent_scope->kind != Scope_Kind::STATIC_IF &&
                           func_parent_scope->kind != Scope_Kind::AGGREGATE) {

                        auto fps = func_parent_scope->parent;
                        assert(fps);
                        assert(fps->kind == Scope_Kind::MODULE ||
                               fps->kind == Scope_Kind::PARAMETER ||
                               fps->kind == Scope_Kind::STATIC_IF);
                        func_parent_scope = fps;
                    }
                }

                Scope *param_scope =
                    scope_new(ast_builder->allocator, Scope_Kind::PARAMETER,
                              func_parent_scope,
                              ast_type->function.parameter_type_specs.count);

                Array<AST_Declaration*> ast_param_decls = {};
                if (ast_type->function.parameter_type_specs.count) {

                    array_init(ast_builder->allocator, &ast_param_decls,
                               ast_type->function.parameter_type_specs.count);

                    auto& ptn_params = ptn->function.prototype->parameters;

                    for (int64_t i = 0; i < ptn_params.count; i++) {
                        auto param_ts = ast_type->function.parameter_type_specs[i];
                        auto param_decl = ast_create_declaration_from_ptn(ast_builder,
                                                                          ptn_params[i],
                                                                          param_ts,
                                                                          param_scope);
                        assert(param_decl);

                        array_append(&ast_param_decls, param_decl);
                    }

                }

                Array<AST_Declaration*> ast_var_decls = {};
                array_init(ast_builder->allocator, &ast_var_decls);

                AST_Statement *ast_body = nullptr;
                if (ptn->function.body) {
                    ast_body =
                        ast_create_statement_from_ptn(ast_builder, ptn->function.body,
                                                      &ast_var_decls, param_scope,
                                                      ast_module);
                    if (!ast_body) return nullptr;
                }

                bool is_naked = ptn->self.flags & PTN_FLAG_DECL_IS_NAKED;
                bool is_noreturn = ptn->self.flags & PTN_FLAG_FUNC_NORETURN;
                bool is_foreign = ptn->self.flags & PTN_FLAG_FUNC_FOREIGN;
                bool is_compiler_func = ptn->self.flags & PTN_FLAG_FUNC_COMPILER_FUNC;

                auto end_fp = ptn->self.end_file_pos;

                result = ast_function_declaration_new(ast_builder->allocator, ast_ident,
                                                      ptn->function.module_name,
                                                      ast_type,
                                                      ast_param_decls, ast_var_decls,
                                                      ast_body,
                                                      is_naked, is_noreturn, is_foreign,
                                                      is_compiler_func,
                                                      parent_scope,
                                                      param_scope,
                                                      begin_fp, end_fp);
                param_scope->function_declaration = result;
                ast_type->function.from_declaration = result;
                break;
            }

            case Declaration_PTN_Kind::STRUCT: {
                auto param_scope = scope_new(ast_builder->allocator, Scope_Kind::PARAMETER,
                                             parent_scope,
                                             ptn->structure.parameters.count);

                auto mem_scope = scope_new(ast_builder->allocator, Scope_Kind::AGGREGATE,
                                           param_scope,
                                           ptn->structure.member_declarations.count);

                Array<AST_Declaration *> ast_member_decls = {};
                if (ptn->structure.member_declarations.count) {
                    array_init(ast_builder->allocator, &ast_member_decls,
                               ptn->structure.member_declarations.count);

                    int64_t index_in_parent = 0;
                    for (int64_t i = 0; i < ptn->structure.member_declarations.count; i++) {
                        auto ptn_mem_decl = ptn->structure.member_declarations[i];
                        auto ast_mem_decl = ast_create_declaration_from_ptn(ast_builder,
                                                                            ptn_mem_decl,
                                                                            nullptr,
                                                                            mem_scope,
                                                                            ast_module);
                        assert(ast_mem_decl);
                        ast_mem_decl->decl_flags |= AST_DECL_FLAG_IS_STRUCT_MEMBER;
                        if (ast_mem_decl->kind == AST_Declaration_Kind::VARIABLE) {
                            assert(ast_mem_decl->variable.index_in_parent == -1);
                            ast_mem_decl->variable.index_in_parent = index_in_parent;
                            index_in_parent += 1;
                        } else {
                            assert(ast_mem_decl->kind == AST_Declaration_Kind::CONSTANT ||
                                   ast_mem_decl->kind == AST_Declaration_Kind::FUNCTION);
                        }

                        array_append(&ast_member_decls, ast_mem_decl);
                    }
                }

                Array<AST_Declaration *> ast_parameters = {};
                if (ptn->structure.parameters.count) {
                    array_init(ast_builder->allocator, &ast_parameters,
                               ptn->structure.parameters.count);

                    for (int64_t i = 0; i < ptn->structure.parameters.count; i++) {
                        auto ptn_param = ptn->structure.parameters[i];
                        AST_Type_Spec *ast_param_ts = nullptr;

                        if (ptn_param->type_expression)
                        {
                            auto type_expr = ptn_param->type_expression;
                            ast_param_ts =
                                ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                         type_expr, param_scope);
                        }


                        auto ast_param_decl =
                            ast_create_declaration_from_ptn(ast_builder, ptn_param,
                                                            ast_param_ts, param_scope);

                        assert(ast_param_decl);

                        array_append(&ast_parameters, ast_param_decl);
                    }
                }

                Array<AST_Identifier *> ast_usings = {};
                if (ptn->structure.usings.count) {
                    array_init(ast_builder->allocator, &ast_usings, ptn->structure.usings.count);

                    for (int64_t i = 0; i < ptn->structure.usings.count; i++) {
                        auto ptn_using = ptn->structure.usings[i];
                        auto ast_using = ast_create_identifier_from_ptn(ast_builder, ptn_using,
                                                                        mem_scope);
                        array_append(&ast_usings, ast_using);
                    }
                }

                assert(ast_parameters.count == 0);
                auto end_fp = ptn->self.end_file_pos;

                result = ast_structure_declaration_new(ast_builder->allocator, ast_ident,
                                                       ast_member_decls, ast_parameters,
                                                       ast_usings,
                                                       parent_scope, param_scope, mem_scope,
                                                       begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::UNION: {
                auto param_scope = scope_new(ast_builder->allocator, Scope_Kind::PARAMETER,
                                             parent_scope,
                                             ptn->union_decl.parameters.count);

                auto mem_scope = scope_new(ast_builder->allocator, Scope_Kind::AGGREGATE,
                                           param_scope,
                                           ptn->union_decl.member_declarations.count);

                Array<AST_Declaration *> ast_mem_decls = {};
                if (ptn->union_decl.member_declarations.count) {
                    array_init(ast_builder->allocator, &ast_mem_decls,
                               ptn->union_decl.member_declarations.count);

                    for (int64_t i = 0; i < ptn->union_decl.member_declarations.count; i++) {
                        auto ptn_mem_decl = ptn->structure.member_declarations[i];
                        auto ast_mem_decl = ast_create_declaration_from_ptn(ast_builder,
                                                                            ptn_mem_decl,
                                                                            nullptr,
                                                                            mem_scope,
                                                                            ast_module);

                        assert(ast_mem_decl);
                        ast_mem_decl->decl_flags |= AST_DECL_FLAG_IS_UNION_MEMBER;

                        array_append(&ast_mem_decls, ast_mem_decl);
                    }
                }

                Array<AST_Declaration *> ast_parameters = {};
                if (ptn->union_decl.parameters.count) {
                    array_init(ast_builder->allocator, &ast_parameters,
                               ptn->union_decl.parameters.count);

                    for (int64_t i = 0; i < ptn->union_decl.parameters.count; i++) {
                        auto ptn_param = ptn->union_decl.parameters[i];
                        AST_Type_Spec *ast_param_ts = nullptr;

                        if (ptn_param->type_expression)
                        {
                            auto type_expr = ptn_param->type_expression;
                            ast_param_ts =
                                ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                         type_expr, param_scope);
                        }


                        auto ast_param_decl =
                            ast_create_declaration_from_ptn(ast_builder, ptn_param,
                                                            ast_param_ts, param_scope);

                        assert(ast_param_decl);

                        array_append(&ast_parameters, ast_param_decl);
                    }
                }

                assert(ast_parameters.count == 0);
                auto end_fp = ptn->self.end_file_pos;

                result = ast_union_declaration_new(ast_builder->allocator, ast_ident,
                                                   ast_mem_decls, ast_parameters,
                                                   parent_scope, param_scope, mem_scope,
                                                   begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::ENUM: {
                AST_Type_Spec *ast_ts = nullptr;

                auto ptn_ts = (PTN*)ptn->enum_decl.type_spec;
                if (ptn_ts) {
                    ast_ts = ast_create_type_spec_from_ptn(ast_builder, ptn_ts, parent_scope);
                } else {
                    ast_ts = ast_type_spec_from_type_new(ast_builder->allocator,
                                                         Builtin::type_s64,
                                                         parent_scope);
                }

                Array<AST_Declaration*> ast_members;
                array_init(ast_builder->allocator, &ast_members, ptn->enum_decl.members.count);

                auto enum_scope = scope_new(ast_builder->allocator, Scope_Kind::AGGREGATE,
                                            parent_scope,
                                            ptn->enum_decl.members.count);

                for (int64_t i = 0; i < ptn->enum_decl.members.count; i++) {
                    auto ptn_mem = ptn->enum_decl.members[i];
                    auto ast_member = ast_create_enum_member_from_ptn(ast_builder, ptn_mem,
                                                                      enum_scope);
                    assert(ast_member->kind == AST_Declaration_Kind::CONSTANT);
                    assert(ast_member->constant.type_spec == nullptr);
                    ast_member->constant.type_spec = ast_ts;

                    array_append(&ast_members, ast_member);
                    if (!ast_scope_add_declaration(ast_builder, enum_scope, ast_member)) {
                        return nullptr;
                    }
                }

                result = ast_enum_declaration_new(ast_builder->allocator, ast_ident, ast_ts,
                                                  ast_members,
                                                  parent_scope, enum_scope,
                                                  begin_fp, ptn->self.end_file_pos);
                break;
            }

            case Declaration_PTN_Kind::TYPEDEF: {
                auto type_spec =
                    ast_create_type_spec_from_expression_ptn(ast_builder,
                                                  ptn->typedef_decl.type_expression, parent_scope);

                result = ast_typedef_declaration_new(ast_builder->allocator, ast_ident,
                                                     type_spec, parent_scope,
                                                     ast_ident->begin_file_pos,
                                                     ptn->self.end_file_pos);
                break;
            }

            case Declaration_PTN_Kind::RUN: {
                auto expression = ast_create_expression_from_ptn(ast_builder, ptn->run.expression,
                                                                 parent_scope);
                result = ast_run_declaration_new(ast_builder->allocator, expression,
                                                 parent_scope,
                                                 ptn->self.begin_file_pos,
                                                 ptn->self.end_file_pos);
                break;
            }

            case Declaration_PTN_Kind::STATIC_IF: {
                auto cond_expr = ast_create_expression_from_ptn(ast_builder,
                                                                ptn->static_if.cond_expression,
                                                                parent_scope);


                auto then_decls =
                    array_create<AST_Declaration *>(ast_builder->allocator,
                                                    ptn->static_if.then_declarations.count);

                assert(ptn->static_if.then_declarations.count);
                Scope *then_scope = scope_new(ast_builder->allocator, Scope_Kind::STATIC_IF,
                                              parent_scope);

                for (int64_t i = 0; i < ptn->static_if.then_declarations.count; i++) {
                    auto ptn_decl = ptn->static_if.then_declarations[i];
                    auto ast_decl = ast_create_declaration_from_ptn(ast_builder, ptn_decl,
                                                                    var_decls, then_scope,
                                                                    ast_module);

                    array_append(&then_decls, ast_decl);
                }

                auto else_decls =
                    array_create<AST_Declaration *>(ast_builder->allocator,
                                                    ptn->static_if.else_declarations.count);

                Scope *else_scope = nullptr;
                if (ptn->static_if.else_declarations.count) {
                    else_scope = scope_new(ast_builder->allocator, Scope_Kind::STATIC_IF,
                                           parent_scope);
                }

                for (int64_t i = 0; i < ptn->static_if.else_declarations.count; i++) {
                    auto ptn_decl = ptn->static_if.else_declarations[i];
                    auto ast_decl = ast_create_declaration_from_ptn(ast_builder, ptn_decl,
                                                                    var_decls, else_scope,
                                                                    ast_module);

                    array_append(&else_decls, ast_decl);
                }

                result = ast_static_if_declaration_new(ast_builder->allocator, cond_expr,
                                                       then_decls, else_decls,
                                                       then_scope, else_scope,
                                                       parent_scope,
                                                       ptn->self.begin_file_pos,
                                                       ptn->self.end_file_pos);
                break;
            }

            case Declaration_PTN_Kind::STATIC_ASSERT: {
                auto cond_expr =
                    ast_create_expression_from_ptn(ast_builder,
                                                   ptn->static_assert_decl.cond_expression,
                                                   parent_scope);

                result = ast_static_assert_declaration_new(ast_builder->allocator, cond_expr,
                                                           parent_scope,
                                                           ptn->self.begin_file_pos,
                                                           ptn->self.end_file_pos);
                break;
            }
        }

        assert(result);
        if (add_to_scope && !ast_scope_add_declaration(ast_builder, parent_scope, result)) {
            assert(false && "Redeclaration, report error!!!");
            return nullptr;
        }
        return result;
    }

    AST_Identifier *ast_create_identifier_from_ptn(AST_Builder *ast_builder, Identifier_PTN *ptn,
                                                   Scope *scope)
    {
        return ast_identifier_new(ast_builder->allocator, ptn->atom, scope,
                                  ptn->self.begin_file_pos,
                                  ptn->self.end_file_pos);
    }

    AST_Identifier *ast_create_identifier_from_ptn(AST_Builder *ast_builder, Expression_PTN *ptn,
                                                   Scope *scope)
    {
        switch (ptn->kind)
        {
            case Expression_PTN_Kind::IDENTIFIER:
            {
                return ast_identifier_new(ast_builder->allocator, ptn->identifier->atom, scope,
                                          ptn->self.begin_file_pos,
                                          ptn->self.end_file_pos);
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    AST_Declaration *ast_create_declaration_from_ptn(AST_Builder *ast_builder,
                                                     Parameter_PTN *ptn,
                                                     AST_Type_Spec *type_spec,
                                                     Scope *scope)
    {
        auto ast_ident = ast_create_identifier_from_ptn(ast_builder, ptn->identifier, scope);
        assert(ast_ident);
        auto begin_fp = ast_ident->begin_file_pos;
        auto end_fp = ast_ident->end_file_pos;

        auto result = ast_parameter_declaration_new(ast_builder->allocator, ast_ident,
                                                    type_spec, scope,
                                                    begin_fp, end_fp);
        assert(result);

        if (!ast_scope_add_declaration(ast_builder, scope, result)) {
            return nullptr;
        }
        return result;
    }

    AST_Declaration *ast_create_enum_member_from_ptn(AST_Builder *ast_builder, PTN *ptn,
                                                     Scope *scope)
    {

        AST_Identifier *identifier = nullptr;
        AST_Expression *init_expr = nullptr;

        switch (ptn->kind)
        {
            case PT_Node_Kind::INVALID: assert(false);

            case PT_Node_Kind::IDENTIFIER:
            {
                Identifier_PTN *ident = (Identifier_PTN*)ptn;
                identifier = ast_create_identifier_from_ptn(ast_builder, ident, scope);
                break;
            }

            case PT_Node_Kind::FUNCTION_PROTO: assert(false);
            case PT_Node_Kind::PARAMETER: assert(false);
            case PT_Node_Kind::EXPRESSION_LIST: assert(false);

            case PT_Node_Kind::DECLARATION:
            {
                Declaration_PTN *declaration = (Declaration_PTN*)ptn;
                Identifier_PTN *ident = declaration->identifier;

                assert(declaration->kind == Declaration_PTN_Kind::CONSTANT);
                assert(declaration->constant.type_expression == nullptr);

                identifier = ast_create_identifier_from_ptn(ast_builder, ident, scope);

                auto init_expression = declaration->constant.init_expression;
                if (init_expression)
                {
                    init_expr = ast_create_expression_from_ptn(ast_builder, init_expression,
                                                               scope);
                }
                break;
            }

            case PT_Node_Kind::STATEMENT: assert(false);
            case PT_Node_Kind::EXPRESSION: assert(false);
        }

        assert(identifier);

        auto result = ast_constant_declaration_new(ast_builder->allocator, identifier,
                                                   nullptr, init_expr, scope,
                                                   ptn->begin_file_pos, ptn->end_file_pos);

        result->decl_flags |= AST_DECL_FLAG_IS_ENUM_MEMBER;

        if (init_expr)
        {
            if (init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL)
            {
                result->decl_flags |= AST_DECL_FLAG_ENUM_MEMBER_INTINIT;
            }
            else if (init_expr->kind == AST_Expression_Kind::IDENTIFIER)
            {
                result->decl_flags |= AST_DECL_FLAG_ENUM_MEMBER_IDENTINIT;
            }
            else assert(false);
        }

        return result;
    }

    AST_Statement *ast_create_statement_from_ptn(AST_Builder *ast_builder, Statement_PTN *ptn,
                                                 Array<AST_Declaration*> *var_decls,
                                                 Scope *parent_scope, AST_Module *ast_module)
    {
        assert(ptn);
        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;

        switch (ptn->kind) {
            case Statement_PTN_Kind::INVALID: assert(false);

            case Statement_PTN_Kind::BLOCK: {
                Array<AST_Statement*> ast_block_stmts = {};
                Scope *block_scope = scope_new(ast_builder->allocator, Scope_Kind::BLOCK,
                                               parent_scope, ptn->block.statements.count);

                if (ptn->block.statements.count) {
                    array_init(ast_builder->allocator, &ast_block_stmts);

                    for (int64_t i = 0; i < ptn->block.statements.count; i++) {
                        AST_Statement *ast_block_stmt =
                            ast_create_statement_from_ptn(ast_builder,
                                                          ptn->block.statements[i],
                                                          var_decls, block_scope,
                                                          ast_module);
                        if (!ast_block_stmt) return nullptr;

                        array_append(&ast_block_stmts, ast_block_stmt);
                    }
                }

                return ast_block_statement_new(ast_builder->allocator, ast_block_stmts,
                                               parent_scope, block_scope, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::DECLARATION: {
                auto ast_declaration = ast_create_declaration_from_ptn(ast_builder,
                                                                       ptn->declaration,
                                                                       var_decls,
                                                                       parent_scope,
                                                                       ast_module);
                if (!ast_declaration) return nullptr;
                return ast_declaration_statement_new(ast_builder->allocator, ast_declaration,
                                                     parent_scope, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::EXPRESSION: {
                auto ast_expression = ast_create_expression_from_ptn(ast_builder, ptn->expression,
                                                                     parent_scope);
                return ast_expression_statement_new(ast_builder->allocator, ast_expression,
                                                    parent_scope, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::RETURN: {

                AST_Expression *return_val_expr = nullptr;
                if (ptn->return_stmt.expression) {
                    return_val_expr =
                        ast_create_expression_from_ptn(ast_builder,
                                                       ptn->return_stmt.expression,
                                                       parent_scope);
                    assert(return_val_expr);
                }

                return ast_return_statement_new(ast_builder->allocator, return_val_expr,
                                                parent_scope, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::BREAK: {
                assert(stack_count(&ast_builder->break_stack));
                AST_Statement *break_from = stack_top(&ast_builder->break_stack);
                assert(break_from);
                return ast_break_statement_new(ast_builder->allocator, break_from, parent_scope,
                                               begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::ASSIGNMENT: {
                auto ast_ident_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->assignment.ident_expression,
                                                   parent_scope);
                assert(ast_ident_expr);

                auto ast_rhs_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->assignment.rhs_expression,
                                                   parent_scope, ast_ident_expr);
                assert(ast_rhs_expr);

                return ast_assignment_statement_new(ast_builder->allocator, ast_ident_expr,
                                                    ast_rhs_expr, parent_scope, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::WHILE: {
                auto cond_expr = ast_create_expression_from_ptn(ast_builder,
                                                                ptn->while_stmt.cond_expr,
                                                                parent_scope);

                AST_Statement *while_stmt = ast_while_statement_new(ast_builder->allocator,
                                                                    cond_expr,
                                                                    nullptr, // Body
                                                                    nullptr, // Body scope
                                                                    parent_scope,
                                                                    begin_fp, end_fp);

                stack_push(&ast_builder->break_stack, while_stmt);
                auto body = ast_create_statement_from_ptn(ast_builder,
                                                          ptn->while_stmt.body,
                                                          var_decls, parent_scope,
                                                          ast_module);
                stack_pop(&ast_builder->break_stack);

                Scope *body_scope = nullptr;

                if (body->kind == AST_Statement_Kind::BLOCK) {
                    body_scope = body->block.scope;
                } else {
                    assert(false);
                }

                assert(body_scope);
                while_stmt->while_stmt.body = body;
                while_stmt->while_stmt.body_scope = body_scope;
                return while_stmt;
                break;
            }

            case Statement_PTN_Kind::FOR:
            {
                auto for_scope = scope_new(ast_builder->allocator, Scope_Kind::BLOCK,
                                           parent_scope);
                auto init_stmt = ast_create_statement_from_ptn(ast_builder,
                                                               ptn->for_stmt.init_stmt,
                                                               var_decls, for_scope,
                                                               ast_module);

                auto cond_expr = ast_create_expression_from_ptn(ast_builder,
                                                                ptn->for_stmt.cond_expr,
                                                                for_scope,
                                                                ast_module);

                auto step_stmt = ast_create_statement_from_ptn(ast_builder,
                                                               ptn->for_stmt.step_stmt,
                                                               var_decls, for_scope,
                                                               ast_module);

                auto body_stmt = ast_create_statement_from_ptn(ast_builder,
                                                               ptn->for_stmt.body_stmt,
                                                               var_decls, for_scope,
                                                               ast_module);

                Array<AST_Statement *> init_statements = {};
                array_init(ast_builder->allocator, &init_statements, 1);
                array_append(&init_statements, init_stmt);

                Array<AST_Statement *> step_statements = {};
                array_init(ast_builder->allocator, &step_statements, 1);
                array_append(&step_statements, step_stmt);

                return ast_for_statement_new(ast_builder->allocator, init_statements,
                                             cond_expr, nullptr, step_statements,
                                             body_stmt, for_scope, parent_scope,
                                             begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::FOREACH:
            {
                Array<AST_Statement *> init_stmts = {};
                int64_t init_count = 1;
                array_init(ast_builder->allocator, &init_stmts, init_count);

                Array<AST_Statement *> step_statements = {};
                array_init(ast_builder->allocator, &step_statements, init_count);

                auto for_scope = scope_new(ast_builder->allocator, Scope_Kind::BLOCK,
                                           parent_scope);

                auto array_expr =
                    ast_create_expression_from_ptn(ast_builder,
                                                   ptn->foreach.array_expression, for_scope);

                auto ii_bfp = ptn->self.begin_file_pos;
                auto ii_efp = ptn->self.begin_file_pos;
                ii_bfp.file_name = string_append(ast_builder->allocator,
                                                 string_ref("<foreach expansion> "),
                                                 ii_bfp.file_name);
                ii_efp.file_name = ii_bfp.file_name;

                auto zero = ast_integer_literal_expression_new(ast_builder->allocator, 0,
                                                               for_scope, ii_bfp, ii_efp);

                auto ptn_idx_ident = ptn->foreach.it_index_identifier;
                AST_Identifier *index_ident = nullptr;
                if (ptn_idx_ident == nullptr)
                {
                    index_ident = ast_identifier_new(ast_builder->allocator,
                                                     Builtin::atom_it_index, for_scope,
                                                     ii_bfp, ii_efp);
                }
                else
                {
                    index_ident = ast_create_identifier_from_ptn(ast_builder, ptn_idx_ident,
                                                                 for_scope);
                }

                auto idx_ident_expr = ast_identifier_expression_new(ast_builder->allocator,
                                                                    index_ident, for_scope,
                                                                    ii_bfp, ii_efp);


                auto index_decl =
                    ast_variable_declaration_new(ast_builder->allocator, index_ident, nullptr,
                                                 zero, for_scope, ii_bfp, ii_efp);
                array_append(var_decls, index_decl);
                ast_scope_add_declaration(ast_builder, for_scope, index_decl);
                auto index_stmt = ast_declaration_statement_new(ast_builder->allocator,
                                                                index_decl, for_scope,
                                                                ii_bfp, ii_efp);
                array_append(&init_stmts, index_stmt);

                auto ptn_it_ident = ptn->foreach.it_identifier;

                AST_Identifier *it_ident = nullptr;
                if (ptn_it_ident)
                {
                    it_ident = ast_create_identifier_from_ptn(ast_builder, ptn_it_ident,
                                                              for_scope);
                }
                else
                {
                    it_ident = ast_identifier_new(ast_builder->allocator, Builtin::atom_it,
                                                  for_scope, ii_bfp, ii_efp);
                }

                auto first_it = ast_subscript_expression_new(ast_builder->allocator, array_expr,
                                                             idx_ident_expr, for_scope,
                                                             ii_bfp, ii_efp);

                if (ptn->foreach.it_is_pointer)
                {
                    first_it = ast_addrof_expression_new(ast_builder->allocator, first_it,
                                                         for_scope, ii_bfp, ii_efp);
                }

                auto it_decl = ast_variable_declaration_new(ast_builder->allocator, it_ident,
                                                            nullptr, first_it, for_scope,
                                                            ii_bfp, ii_efp);
                array_append(var_decls, it_decl);
                ast_scope_add_declaration(ast_builder, for_scope, it_decl);
                auto count_ident = ast_identifier_new(ast_builder->allocator, Builtin::atom_count,
                                                      for_scope, ii_bfp, ii_efp);

                auto cond_rhs = ast_dot_expression_new(ast_builder->allocator,
                                                       AST_Dot_Expression_Kind::ARRAY_COUNT,
                                                       array_expr, count_ident, for_scope,
                                                       ii_bfp, ii_efp);

                auto cond_expr = ast_binary_expression_new(ast_builder->allocator,
                                                           BINOP_LT,
                                                           idx_ident_expr, cond_rhs,
                                                           for_scope, ii_bfp, ii_efp);

                auto one = ast_integer_literal_expression_new(ast_builder->allocator, 1,
                                                              for_scope, ii_bfp, ii_efp);

                auto new_idx_expr = ast_binary_expression_new(ast_builder->allocator,
                                                             BINOP_ADD,
                                                             idx_ident_expr, one,
                                                             for_scope, ii_bfp, ii_efp);

                auto idx_step_expr = ast_assignment_statement_new(ast_builder->allocator,
                                                                  idx_ident_expr,
                                                                  new_idx_expr,
                                                                  for_scope, ii_bfp, ii_efp);
                array_append(&step_statements, idx_step_expr);

                auto body_stmt =
                    ast_create_statement_from_ptn(ast_builder, ptn->foreach.body_stmt,
                                                  var_decls, for_scope,
                                                  ast_module);

                return ast_for_statement_new(ast_builder->allocator, init_stmts, cond_expr,
                                             it_decl, step_statements, body_stmt,
                                             for_scope, parent_scope,
                                             begin_fp, end_fp);
            }

            case Statement_PTN_Kind::IF: {
                auto cond_expr = ast_create_expression_from_ptn(ast_builder,
                                                                ptn->if_stmt.cond_expr,
                                                                parent_scope);
                auto then_stmt = ast_create_statement_from_ptn(ast_builder,
                                                               ptn->if_stmt.then_stmt,
                                                               var_decls, parent_scope,
                                                               ast_module);
                Scope *then_scope = nullptr;

                if (then_stmt->kind == AST_Statement_Kind::BLOCK) {
                    then_scope = then_stmt->block.scope;
                } else {
                    then_scope = scope_new(ast_builder->allocator, Scope_Kind::BLOCK,
                                           parent_scope);
                }

                assert(then_scope);

                Scope *else_scope = nullptr;

                AST_Statement *else_stmt = nullptr;

                if (ptn->if_stmt.else_stmt) {
                    else_stmt = ast_create_statement_from_ptn(ast_builder,
                                                              ptn->if_stmt.else_stmt,
                                                              var_decls, parent_scope,
                                                              ast_module);
                    assert(else_stmt);

                    if (else_stmt->kind == AST_Statement_Kind::BLOCK) {
                        else_scope = else_stmt->block.scope;
                    } else if (else_stmt->kind == AST_Statement_Kind::IF) {
                        else_scope = else_stmt->if_stmt.then_scope;
                    } else {
                        else_scope = scope_new(ast_builder->allocator, Scope_Kind::BLOCK,
                                               parent_scope);
                    }

                    assert(else_scope);
                }

                return ast_if_statement_new(ast_builder->allocator, cond_expr,
                                            then_stmt, else_stmt,
                                            then_scope, else_scope,
                                            parent_scope,
                                            begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::SWITCH:
            {
                Array<AST_Switch_Case*> cases = {};
                array_init(ast_builder->allocator, &cases, ptn->switch_stmt.cases.count);

                AST_Expression *expression =
                    ast_create_expression_from_ptn(ast_builder, ptn->switch_stmt.expression,
                                                   parent_scope);

                AST_Switch_Case *default_case = nullptr;
                uint32_t case_expr_count = 0;

                bool allow_incomplete = ptn->switch_stmt.allow_incomplete;

                AST_Statement *switch_stmt =
                    ast_switch_statement_new(ast_builder->allocator,
                            expression,
                            ptn->switch_stmt.cases.count,
                            allow_incomplete,
                            parent_scope,
                            begin_fp,
                            end_fp);

                for (int64_t i = 0; i < ptn->switch_stmt.cases.count; i++)
                {
                    auto ptn_case = ptn->switch_stmt.cases[i];

                    bool is_default = ptn_case.is_default;

                    Bucket_Array<AST_Expression *, 4> case_expressions ={};

                    if (is_default) {
                        assert(!ptn_case.expressions.count);
                    } else {
                        auto ptn_exprs = ptn_case.expressions;
                        bucket_array_init(ast_builder->allocator, &case_expressions);

                        assert(ptn_case.expressions.count);
                        for (int64_t expr_i = 0; expr_i < ptn_exprs.count; expr_i++) {
                            auto switch_case_expr = ptn_exprs[expr_i];
                            AST_Expression *case_expr = nullptr;
                            if (!switch_case_expr.range_end_expr) {
                                auto ptn_case_expr = switch_case_expr.expression;
                                case_expr =
                                    ast_create_expression_from_ptn(ast_builder,
                                                                   ptn_case_expr,
                                                                   parent_scope);
                            } else {
                                auto r_begin_expr =
                                    ast_create_expression_from_ptn(
                                            ast_builder,
                                            switch_case_expr.range_begin_expr,
                                            parent_scope
                                );

                                auto r_end_expr = ast_create_expression_from_ptn(
                                        ast_builder,
                                        switch_case_expr.range_end_expr,
                                        parent_scope
                                );

                                case_expr = ast_range_expression_new(ast_builder->allocator,
                                                                     r_begin_expr,
                                                                     r_end_expr, parent_scope);
                            }

                            assert(case_expr);
                            bucket_array_add(&case_expressions, case_expr);
                        }

                        assert(ptn_exprs.count < UINT32_MAX);
                        case_expr_count += (uint32_t)ptn_exprs.count;

                    }

                    stack_push(&ast_builder->break_stack, switch_stmt);
                    AST_Statement *body = ast_create_statement_from_ptn(ast_builder,
                                                                        ptn_case.body,
                                                                        var_decls,
                                                                        parent_scope,
                                                                        ast_module);
                    stack_pop(&ast_builder->break_stack);

                    AST_Switch_Case *ast_case = ast_switch_case_new(ast_builder->allocator,
                                                                    case_expressions,
                                                                    is_default, body,
                                                                    parent_scope,
                                                                    ptn_case.begin_fp,
                                                                    ptn_case.end_fp);

                    array_append(&switch_stmt->switch_stmt.cases, ast_case);

                    if (is_default) {
                        assert(default_case == nullptr);
                        default_case = ast_case;
                    }

                    array_append(&cases, ast_case);
                }

                if (default_case) {
                    switch_stmt->switch_stmt.default_case = default_case;
                }
                switch_stmt->switch_stmt.case_expr_count = case_expr_count;
                return switch_stmt;
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Expression *ast_create_expression_from_ptn(AST_Builder *ast_builder,
                                                   Expression_PTN *ptn,
                                                   Scope *scope,
                                                   AST_Node *infer_type_from /*=nullptr*/)
    {
        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;

        switch (ptn->kind) {
            case Expression_PTN_Kind::INVALID: assert(false);

            case Expression_PTN_Kind::CALL: {
                Array<AST_Expression*> arg_exprs = {};
                if (ptn->call.arg_list) {
                    array_init(ast_builder->allocator, &arg_exprs);

                    for (int64_t i = 0; i < ptn->call.arg_list->expressions.count; i++) {
                        AST_Expression *arg_expr =
                            ast_create_expression_from_ptn(ast_builder,
                                    ptn->call.arg_list->expressions[i], scope);
                        assert(arg_expr);

                        array_append(&arg_exprs, arg_expr);
                    }
                }

                if (ptn->call.is_builtin) {
                    AST_Identifier *identifier =
                        ast_create_identifier_from_ptn(ast_builder,
                                                       ptn->call.ident_expression,
                                                       scope);
                    assert(identifier);
                    return ast_builtin_call_expression_new(ast_builder->allocator, identifier,
                                                           arg_exprs, scope, begin_fp, end_fp);
                }

                AST_Expression *ident_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->call.ident_expression,
                                                   scope);
                assert(ident_expr);


                return ast_call_expression_new(ast_builder->allocator, ident_expr, arg_exprs,
                                               scope, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::IDENTIFIER: {
                auto identifier = ast_create_identifier_from_ptn(ast_builder,
                                                                 ptn->identifier, scope);
                return ast_identifier_expression_new(ast_builder->allocator, identifier,
                                                     scope, begin_fp, end_fp);

                break;
            }

            case Expression_PTN_Kind::BINARY: {
                auto ast_lhs_expr = ast_create_expression_from_ptn(ast_builder, ptn->binary.lhs,
                                                                   scope);
                assert(ast_lhs_expr);

                auto ast_rhs_expr = ast_create_expression_from_ptn(ast_builder, ptn->binary.rhs,
                                                                   scope);
                assert(ast_rhs_expr);

                return ast_binary_expression_new(ast_builder->allocator, ptn->binary.op,
                                                 ast_lhs_expr, ast_rhs_expr, scope,
                                                 begin_fp, end_fp);
                break;
            };

            case Expression_PTN_Kind::UNARY: {
                auto ast_operand_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->unary.operand_expression,
                                                   scope);
                assert(ast_operand_expr);

                return ast_unary_expression_new(ast_builder->allocator, ptn->unary.op,
                                                ast_operand_expr, scope,
                                                begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::DOT: {
                auto ast_parent_expr = ast_create_expression_from_ptn(ast_builder,
                                                                      ptn->dot.parent_expression,
                                                                      scope);
                assert(ast_parent_expr);

                auto ast_child_ident =
                    ast_create_identifier_from_ptn(ast_builder, ptn->dot.child_identifier,
                                                   scope);
                assert(ast_child_ident);

                return ast_dot_expression_new(ast_builder->allocator,
                                              AST_Dot_Expression_Kind::UNKNOWN,
                                              ast_parent_expr, ast_child_ident, scope,
                                              begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::COMPOUND: {
                Array<AST_Expression*> ast_compound_exprs = {};
                if (ptn->compound.list->expressions.count) {
                    array_init(ast_builder->allocator, &ast_compound_exprs);

                    for (int64_t i = 0; i < ptn->compound.list->expressions.count; i++) {
                        auto ast_compound_expr =
                            ast_create_expression_from_ptn(ast_builder,
                                                           ptn->compound.list->expressions[i],
                                                           scope);
                        assert(ast_compound_expr);

                        array_append(&ast_compound_exprs, ast_compound_expr);
                    }
                }

                AST_Type_Spec *ast_ts = nullptr;
                if (ptn->compound.type_expression) {
                    ast_ts =
                        ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                 ptn->compound.type_expression,
                                                                 scope);
                    assert(ast_ts);
                }

                return ast_compound_expression_new(ast_builder->allocator, ast_compound_exprs,
                                                   ast_ts, scope,
                                                   begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::SUBSCRIPT: {
                auto pointer_expr =
                    ast_create_expression_from_ptn(ast_builder,
                                                   ptn->subscript.pointer_expression, scope);
                auto index_expr =
                    ast_create_expression_from_ptn(ast_builder,
                                                   ptn->subscript.index_expression, scope);

                auto bfp = ptn->self.begin_file_pos;
                auto efp = ptn->self.end_file_pos;
                return ast_subscript_expression_new(ast_builder->allocator, pointer_expr,
                                                    index_expr, scope,
                                                    bfp, efp);
                break;
            }

            case Expression_PTN_Kind::INTEGER_LITERAL: {
                assert(((int64_t)ptn->integer_literal.u64) ==
                       ptn->integer_literal.s64);
                auto result =  ast_integer_literal_expression_new(ast_builder->allocator,
                                                                  ptn->integer_literal.s64,
                                                                  scope,
                                                                  begin_fp, end_fp);
                result->infer_type_from = infer_type_from;
                return result;
                break;
            }

            case Expression_PTN_Kind::FLOAT_LITERAL: {
                return ast_float_literal_expression_new(ast_builder->allocator,
                                                        ptn->float_literal.r32,
                                                        ptn->float_literal.r64,
                                                        scope,
                                                        begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::STRING_LITERAL: {
                return ast_string_literal_expression_new(ast_builder->allocator,
                                                         ptn->string_literal.atom,
                                                         scope,
                                                         begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::CHAR_LITERAL: {
                return ast_char_literal_expression_new(ast_builder->allocator,
                                                       ptn->char_literal.c,
                                                       scope,
                                                       begin_fp, end_fp);
            }

            case Expression_PTN_Kind::BOOL_LITERAL: {
                return ast_boolean_literal_expression_new(ast_builder->allocator,
                                                          ptn->bool_literal.value,
                                                          scope,
                                                          begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::NULL_LITERAL: {
                // assert(infer_type_from);
                auto result =  ast_null_literal_expression_new(ast_builder->allocator, scope,
                                                               begin_fp, end_fp);
                result->infer_type_from = infer_type_from;
                return result;
                break;
            }

            case Expression_PTN_Kind::ARRAY_TYPE: assert(false);

            case Expression_PTN_Kind::POINTER_TYPE: {
                auto ptn_operand_expr = ptn->pointer_type.pointee_type_expression;
                auto operand_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn_operand_expr, scope);
                return ast_addrof_expression_new(ast_builder->allocator, operand_expr,
                                                 scope, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::POLY_TYPE: {
                assert(ptn->poly_type.identifier);

                auto ast_ident =
                    ast_create_identifier_from_ptn(ast_builder, ptn->poly_type.identifier, scope);
                assert(ast_ident);

                AST_Identifier *ast_spec_ident = nullptr;
                if (ptn->poly_type.specification_identifier) {
                    ast_spec_ident = ast_create_identifier_from_ptn(ast_builder,
                                               ptn->poly_type.specification_identifier,
                                               scope);
                    assert(ast_spec_ident);
                }

                AST_Declaration *poly_type_decl =
                    ast_poly_type_declaration_new(ast_builder->allocator, ast_ident,
                                                  ast_spec_ident, scope, begin_fp, end_fp);
                assert(poly_type_decl);
                return ast_poly_identifier_expression_new(ast_builder->allocator, poly_type_decl,
                                                          scope, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::FUNCTION_TYPE: {
                assert(false);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Type_Spec *ast_create_type_spec_from_ptn(AST_Builder *ast_builder, PT_Node *ptn,
                                                 Scope *scope)
    {
        auto begin_fp = ptn->begin_file_pos;
        auto end_fp = ptn->end_file_pos;

        switch (ptn->kind)
        {
            case PT_Node_Kind::INVALID: assert(false);
            case PT_Node_Kind::IDENTIFIER: assert(false);

            case PT_Node_Kind::FUNCTION_PROTO: {
                auto function_ptn = (Function_Proto_PTN*)ptn;

                Array<AST_Type_Spec*> ast_param_types = {};
                if (function_ptn->parameters.count) {
                    array_init(ast_builder->allocator, &ast_param_types);
                    for (int64_t i = 0; i < function_ptn->parameters.count; i++) {
                        AST_Type_Spec *ast_param_type =
                            ast_create_type_spec_from_ptn(ast_builder,
                                                          &function_ptn->parameters[i]->self,
                                                          scope);
                        assert(ast_param_type);

                        array_append(&ast_param_types, ast_param_type);
                    }

                }

                AST_Type_Spec *ast_return_type = nullptr;
                if (function_ptn->return_type_expression) {
                    ast_return_type =
                        ast_create_type_spec_from_ptn(
                               ast_builder,
                               &function_ptn->return_type_expression->self,
                               scope
                    );
                    assert(ast_return_type);
                }

                return ast_function_type_spec_new(ast_builder->allocator, ast_param_types,
                                                  ast_return_type, nullptr, scope,
                                                  begin_fp, end_fp);
                break;
            }

            case PT_Node_Kind::PARAMETER: {
                auto param_ptn = (Parameter_PTN*)ptn;
                return ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                param_ptn->type_expression,
                                                                scope);
                break;
            }

            case PT_Node_Kind::EXPRESSION_LIST: assert(false);
            case PT_Node_Kind::DECLARATION: assert(false);
            case PT_Node_Kind::STATEMENT: assert(false);

            case PT_Node_Kind::EXPRESSION: {
                return ast_create_type_spec_from_expression_ptn(ast_builder,
                                                               (Expression_PTN*)ptn, scope);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Type_Spec *ast_create_type_spec_from_expression_ptn(AST_Builder *ast_builder,
                                                            Expression_PTN *ptn, Scope *scope)
    {
        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;

        switch (ptn->kind) {
            case Expression_PTN_Kind::INVALID: assert(false);

            case Expression_PTN_Kind::CALL: {
                assert(!ptn->call.is_builtin);

                auto ast_ident_expr =
                    ast_create_expression_from_ptn(ast_builder, ptn->call.ident_expression,
                                                   scope);
                assert(ast_ident_expr);

                Array<AST_Expression*> ast_arg_exprs = {};
                array_init(ast_builder->allocator, &ast_arg_exprs);

                for (int64_t i = 0; i < ptn->call.arg_list->expressions.count; i++) {
                    auto ast_arg_expr =
                        ast_create_expression_from_ptn(ast_builder,
                                                       ptn->call.arg_list->expressions[i], scope);
                    assert(ast_arg_expr);

                    array_append(&ast_arg_exprs, ast_arg_expr);
                }

                return ast_templated_type_spec_new(ast_builder->allocator, ast_ident_expr,
                                                   ast_arg_exprs, scope,
                                                   begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::IDENTIFIER: {
                auto ident = ast_create_identifier_from_ptn(ast_builder, ptn->identifier, scope);
                assert(ident);

                return ast_identifier_type_spec_new(ast_builder->allocator, ident, scope,
                                                    begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::BINARY: assert(false);
            case Expression_PTN_Kind::UNARY: assert(false);

            case Expression_PTN_Kind::DOT: {
                auto ast_dot_expr = ast_create_expression_from_ptn(ast_builder, ptn, scope);
                assert(ast_dot_expr);

                return ast_dot_type_spec_new(ast_builder->allocator, ast_dot_expr, scope,
                                             begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::COMPOUND: assert(false);
            case Expression_PTN_Kind::SUBSCRIPT: assert(false);
            case Expression_PTN_Kind::INTEGER_LITERAL: assert(false);
            case Expression_PTN_Kind::FLOAT_LITERAL: assert(false);
            case Expression_PTN_Kind::STRING_LITERAL: assert(false);
            case Expression_PTN_Kind::CHAR_LITERAL: assert(false);
            case Expression_PTN_Kind::BOOL_LITERAL: assert(false);

            case Expression_PTN_Kind::NULL_LITERAL: assert(false);

            case Expression_PTN_Kind::ARRAY_TYPE: {
                auto length_ptn = ptn->array_type.length_expression;
                auto elem_type_ptn = ptn->array_type.element_type_expression;
                AST_Expression *length_expr = nullptr;
                if (length_ptn)
                {
                    length_expr = ast_create_expression_from_ptn(ast_builder, length_ptn, scope);
                }
                auto ast_elem_ts =
                    ast_create_type_spec_from_expression_ptn(ast_builder, elem_type_ptn, scope);
                assert(ast_elem_ts);

                return ast_array_type_spec_new(ast_builder->allocator, length_expr, ast_elem_ts,
                                               scope, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::POINTER_TYPE: {
                auto ptn_base_expr = ptn->pointer_type.pointee_type_expression;
                auto ast_base_ts = ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                            ptn_base_expr, scope);
                assert(ast_base_ts);
                ast_base_ts->ts_flags |= AST_TS_FLAG_CHILD_OF_POINTER_TS;

                return ast_pointer_type_spec_new(ast_builder->allocator, ast_base_ts,
                                                 scope, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::POLY_TYPE: {
                auto ast_ident = ast_create_identifier_from_ptn(ast_builder,
                                                            ptn->poly_type.identifier, scope);
                assert(ast_ident);

                AST_Identifier *ast_spec_ident = nullptr;
                if (ptn->poly_type.specification_identifier) {
                    ast_spec_ident = ast_create_identifier_from_ptn(ast_builder,
                                            ptn->poly_type.specification_identifier, scope);
                    assert(ast_spec_ident);
                }

                AST_Declaration *ast_decl =
                    ast_poly_type_declaration_new(ast_builder->allocator, ast_ident,
                                                  ast_spec_ident, scope, begin_fp, end_fp);
                assert(ast_decl);

                return ast_poly_identifier_type_spec_new(ast_builder->allocator, ast_decl,
                                                         ast_spec_ident, scope,
                                                         begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::FUNCTION_TYPE: {
                auto proto = ptn->function_proto;
                auto param_count = proto->parameters.count;
                auto param_type_specs = array_create<AST_Type_Spec *>(ast_builder->allocator,
                                                                      param_count);

                for (int64_t i = 0; i < param_count; i++) {
                    auto param = proto->parameters[i];
                    auto param_type_spec =
                        ast_create_type_spec_from_expression_ptn(ast_builder,
                                                                 param->type_expression,
                                                                 scope);

                    array_append(&param_type_specs, param_type_spec);
                }

                AST_Type_Spec *return_ts = nullptr;

                if (proto->return_type_expression) {
                    return_ts = ast_create_type_spec_from_expression_ptn(ast_builder,
                                                             proto->return_type_expression, scope);
                } else {
                    return_ts = ast_type_spec_from_type_new(ast_builder->allocator,
                                                            Builtin::type_void, scope);
                }

                assert(return_ts);
                return ast_function_type_spec_new(ast_builder->allocator, param_type_specs,
                                                  return_ts, nullptr, scope,
                                                  begin_fp, end_fp);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    void ast_flatten_declaration(AST_Builder *builder, AST_Declaration **p_decl)
    {
        auto decl = *p_decl;

        assert(!decl->flat);

        Array<AST_Node **> nodes = {};
        array_init(builder->allocator, &nodes);

        ast_flatten_declaration(builder, p_decl, &nodes);

        if (!nodes.count) array_free(&nodes);

        decl->flat = ast_flat_declaration_new(builder->allocator, nodes);
    }

    void ast_flatten_declaration(AST_Builder *builder, AST_Declaration **p_decl,
                                 Array<AST_Node **> *nodes)
    {
        auto decl = *p_decl;
        auto node = (AST_Node **)p_decl;

        switch (decl->kind) {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT: {
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::USING: {
                ast_flatten_expression(builder, &decl->using_decl.ident_expr, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::VARIABLE: {

                if (decl->variable.type_spec)
                    ast_flatten_type_spec(builder, &decl->variable.type_spec, nodes);

                if (decl->variable.init_expression)
                    ast_flatten_expression(builder, &decl->variable.init_expression, nodes);

                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::CONSTANT: {

                if (decl->constant.type_spec)
                    ast_flatten_type_spec(builder, &decl->constant.type_spec, nodes);

                assert(decl->constant.init_expression ||
                       (decl->decl_flags & AST_DECL_FLAG_IS_ENUM_MEMBER));

                if (decl->constant.init_expression)
                    ast_flatten_expression(builder, &decl->constant.init_expression, nodes);

                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::PARAMETER: {
                ast_flatten_type_spec(builder, &decl->parameter.type_spec, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::FUNCTION: {

                for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++) {
                    auto param_decl = &decl->function.parameter_declarations[i];
                    ast_flatten_declaration(builder, param_decl, nodes);
                }

                ast_flatten_type_spec(builder, &decl->function.type_spec, nodes);

                if (decl->function.body) {
                    ast_flatten_statement(builder, &decl->function.body, nodes);
                } else {
                    assert(decl->decl_flags & AST_DECL_FLAG_FOREIGN);
                }

                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::TYPEDEF: {
                ast_flatten_type_spec(builder, &decl->typedef_decl.type_spec, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: {
                assert(decl->structure.parameters.count == 0);

                // We don't flatten member declarations, to be able to handle
                //  pointers to aggregates. We iterate all members when resolving
                //  the structure declaration pushed below.

                // for (int64_t i = 0; i < decl->structure.member_declarations.count; i++) {
                //     ast_flatten_declaration(builder, decl->structure.member_declarations[i],
                //                             nodes);
                // }

                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::UNION:
            {
                 assert(decl->union_decl.parameters.count == 0);

                // We don't flatten member declarations, to be able to handle
                //  pointers to aggregates. We iterate all members when resolving
                //  the union declaration pushed below.

                // for (int64_t i = 0; i < decl->union_decl.member_declarations.count; i++) {
                //     ast_flatten_declaration(builder, decl->union_decl.member_declarations[i],
                //                             nodes);
                // }

                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::ENUM: {
                assert(decl->enum_decl.type_spec);
                if (decl->enum_decl.type_spec)
                    ast_flatten_type_spec(builder, &decl->enum_decl.type_spec, nodes);

                // We don't flatten member declarations, because then we might get stuck on
                //  a reference to a later member. We iterate all members when resolving the
                //  enum declaration pushed below.
                //
                // for (int64_t i = 0; i < decl->enum_decl.member_declarations.count; i++) {
                //     ast_flatten_declaration(builder, decl->enum_decl.member_declarations[i],
                //                             nodes);
                // }

                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: assert(false);

            case AST_Declaration_Kind::RUN: {
                ast_flatten_expression(builder, &decl->run.expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::STATIC_IF: {
                ast_flatten_expression(builder, &decl->static_if.cond_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::STATIC_ASSERT: {
                ast_flatten_expression(builder, &decl->static_assert_decl.cond_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Declaration_Kind::IMPORT_LINK: assert(false);
        }
    }

    void ast_flatten_statement(AST_Builder *builder, AST_Statement **p_stmt,
                               Array<AST_Node **> *nodes)
    {
        auto stmt = *p_stmt;
        auto node = (AST_Node **)p_stmt;

        switch (stmt->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: {

                for (int64_t i = 0; i < stmt->block.statements.count; i++) {
                    ast_flatten_statement(builder, &stmt->block.statements[i], nodes);
                }

                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: {
                ast_flatten_expression(builder, &stmt->assignment.identifier_expression, nodes);
                ast_flatten_expression(builder, &stmt->assignment.rhs_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::RETURN: {
                if (stmt->expression) {
                    ast_flatten_expression(builder, &stmt->expression, nodes);
                }

                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::BREAK: {
                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
                ast_flatten_declaration(builder, &stmt->declaration, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
                ast_flatten_expression(builder, &stmt->expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::WHILE: {
                ast_flatten_expression(builder, &stmt->while_stmt.cond_expr, nodes);
                ast_flatten_statement(builder, &stmt->while_stmt.body, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::FOR: {
                for (int64_t i = 0; i < stmt->for_stmt.init_statements.count; i++) {
                    ast_flatten_statement(builder, &stmt->for_stmt.init_statements[i], nodes);
                }

                if (stmt->for_stmt.it_decl) {
                    ast_flatten_declaration(builder, &stmt->for_stmt.it_decl, nodes);
                }

                ast_flatten_expression(builder, &stmt->for_stmt.cond_expr, nodes);

                for (int64_t i = 0; i < stmt->for_stmt.step_statements.count; i++) {
                    ast_flatten_statement(builder, &stmt->for_stmt.step_statements[i], nodes);
                }

                ast_flatten_statement(builder, &stmt->for_stmt.body_stmt, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::IF: {
                ast_flatten_expression(builder, &stmt->if_stmt.cond_expr, nodes);
                ast_flatten_statement(builder, &stmt->if_stmt.then_stmt, nodes);

                if (stmt->if_stmt.else_stmt)
                    ast_flatten_statement(builder, &stmt->if_stmt.else_stmt, nodes);

                array_append(nodes, node);
                break;
            }

            case AST_Statement_Kind::SWITCH: {
                ast_flatten_expression(builder, &stmt->switch_stmt.expression, nodes);

                for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++) {
                    auto switch_case = stmt->switch_stmt.cases[i];
                    if (!switch_case->is_default) {

                        auto el = bucket_array_first(&switch_case->expressions);
                        while (el.bucket) {
                            auto p_expr = bucket_locator_get_ptr(el);
                            ast_flatten_expression(builder, p_expr, nodes);
                            bucket_locator_advance(&el);
                        }
                    }

                    ast_flatten_statement(builder, &switch_case->body, nodes);
                }
                array_append(nodes, node);
                break;
            }
        }
    }

    void ast_flatten_expression(AST_Builder *builder, AST_Expression **p_expr,
                                Array<AST_Node **> *nodes)
    {
        auto expr = *p_expr;
        auto node = (AST_Node **)p_expr;

        switch (expr->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: {
                ast_flatten_expression(builder, &expr->dot.parent_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::BINARY: {
                ast_flatten_expression(builder, &expr->binary.lhs, nodes);
                ast_flatten_expression(builder, &expr->binary.rhs, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::UNARY: {
                ast_flatten_expression(builder, &expr->unary.operand_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::CALL: {
                ast_flatten_expression(builder, &expr->call.ident_expression, nodes);

                for (int64_t i = 0; i < expr->call.arg_expressions.count; i++) {
                    ast_flatten_expression(builder, &expr->call.arg_expressions[i], nodes);
                }

                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {

                auto args = expr->builtin_call.arg_expressions;

                if (expr->builtin_call.identifier->atom == Builtin::atom_offsetof) {
                    assert(args.count == 2);
                    ast_flatten_expression(builder, &args[1], nodes);

                } else {
                    for (int64_t i = 0; i < args.count; i++) {
                        ast_flatten_expression(builder, &args[i], nodes);
                    }
                }

                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::ADDROF: {
                ast_flatten_expression(builder, &expr->addrof.operand_expr, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
                ast_flatten_expression(builder, &expr->subscript.index_expression, nodes);
                ast_flatten_expression(builder, &expr->subscript.pointer_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::CAST: assert(false);

            case AST_Expression_Kind::INTEGER_LITERAL:
            case AST_Expression_Kind::STRING_LITERAL:
            case AST_Expression_Kind::FLOAT_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL:
            case AST_Expression_Kind::BOOL_LITERAL:
            case AST_Expression_Kind::NULL_LITERAL: {
                array_append(nodes, node);
                break;
            }

            case AST_Expression_Kind::RANGE: {
                ast_flatten_expression(builder, &expr->range.begin, nodes);
                ast_flatten_expression(builder, &expr->range.end, nodes);
                array_append(nodes, node);
                break;
            }
        }
    }

    void ast_flatten_type_spec(AST_Builder *builder, AST_Type_Spec **p_type_spec,
                               Array<AST_Node **> *nodes)
    {
        auto type_spec = *p_type_spec;
        auto node = (AST_Node **)p_type_spec;

        switch (type_spec->kind) {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER:
            {
                array_append(nodes, node);
                break;
            }

            case AST_Type_Spec_Kind::POINTER: {
                ast_flatten_type_spec(builder, &type_spec->base_type_spec, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Type_Spec_Kind::DOT: {
                ast_flatten_expression(builder, &type_spec->dot_expression, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Type_Spec_Kind::FUNCTION: {
                for (int64_t i = 0; i < type_spec->function.parameter_type_specs.count; i++) {
                    auto param_ts = &type_spec->function.parameter_type_specs[i];
                    ast_flatten_type_spec(builder, param_ts, nodes);
                }

                if (type_spec->function.return_type_spec)
                    ast_flatten_type_spec(builder, &type_spec->function.return_type_spec, nodes);

                array_append(nodes, node);
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: {
                ast_flatten_expression(builder, &type_spec->array.length_expression, nodes);
                ast_flatten_type_spec(builder, &type_spec->array.element_type_spec, nodes);
                array_append(nodes, node);
                break;
            }

            case AST_Type_Spec_Kind::TEMPLATED: assert(false);
            case AST_Type_Spec_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Type_Spec_Kind::FROM_TYPE: {
                array_append(nodes, node);
                break;
            }
        }
    }

    AST_Flat_Declaration *ast_flat_declaration_new(Allocator *allocator, Array<AST_Node **> nodes)
    {
        auto result = alloc_type<AST_Flat_Declaration>(allocator);

        result->nodes = nodes;
        result->waiting_on = 0;

        return result;
    }

    AST_Identifier *ast_identifier_new(Allocator *allocator, Atom& atom, Scope *scope,
                                       const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Identifier>(allocator, scope, begin_fp, end_fp);

        result->atom = atom;
        result->declaration = nullptr;

        return result;
    }

    AST_Module *ast_module_new(Allocator *allocator, Atom name,
                               Scope *module_scope,
                               const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(module_scope->kind == Scope_Kind::MODULE);

        auto result = ast_node_new<AST_Module>(allocator, module_scope, begin_fp, end_fp);

        result->name = name;

        result->declarations = {};
        result->module_scope = nullptr;

        return result;
    }

    AST_Declaration *ast_declaration_new(Allocator *allocator,
                                         AST_Declaration_Kind kind,
                                         AST_Identifier *identifier,
                                         Scope *scope,
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Declaration>(allocator, scope, begin_fp, end_fp);

        result->kind = kind;
        result->identifier = identifier;

        return result;
    }

    AST_Declaration *ast_import_declaration_new(Allocator *allocator,
                                                AST_Identifier *identifier,
                                                AST_Expression *ident_expr,
                                                Scope *scope,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::IMPORT,
                                          identifier, scope, begin_fp, end_fp);

        result->import.ident_expression = ident_expr;
        result->import.ast_module = nullptr;

        return result;
    }

    AST_Declaration *ast_using_declaration_new(Allocator *allocator,
                                               AST_Expression *import_ident_expr,
                                                Scope *scope,
                                               const File_Pos &begin_fp,
                                               const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::USING, nullptr,
                                          scope, begin_fp, end_fp);

        result->using_decl.ident_expr = import_ident_expr;

        return result;
    }

    AST_Declaration *ast_variable_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec,
                                                  AST_Expression *init_expr,
                                                  Scope *scope,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::VARIABLE, identifier,
                                          scope, begin_fp, end_fp);

        result->variable.type_spec = type_spec;
        result->variable.init_expression = init_expr;
        result->variable.index_in_parent = -1;

        return result;
    }

    AST_Declaration *ast_constant_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec,
                                                  AST_Expression *init_expr,
                                                  Scope *scope,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::CONSTANT, identifier,
                                          scope, begin_fp, end_fp);
        result->constant.type_spec = type_spec;
        result->constant.init_expression = init_expr;

        return result;
    }

    AST_Declaration *ast_parameter_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   AST_Type_Spec *type_spec,
                                                   Scope *scope,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::PARAMETER, identifier,
                                          scope, begin_fp, end_fp);

        result->parameter.type_spec = type_spec;

        return result;
    }

    AST_Declaration *ast_function_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  Atom module_name,
                                                  AST_Type_Spec *type_spec,
                                                  Array<AST_Declaration*> parameter_declarations,
                                                  Array<AST_Declaration*> variable_declarations,
                                                  AST_Statement *body,
                                                  bool is_naked, bool is_noreturn,
                                                  bool is_foreign,
                                                  bool is_compiler_func,
                                                  Scope *parent_scope,
                                                  Scope *param_scope,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        if (body) assert(body->kind == AST_Statement_Kind::BLOCK);

        assert(identifier);
        assert(param_scope->kind == Scope_Kind::PARAMETER);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::FUNCTION, identifier,
                                          parent_scope, begin_fp, end_fp);

        result->function.type_spec = type_spec;
        result->function.parameter_declarations = parameter_declarations;
        result->function.variable_declarations = variable_declarations;
        result->function.body = body;
        result->function.parameter_scope = param_scope;
        result->function.module_name = module_name;

        array_init(allocator, &result->function.called_functions);

        if (is_naked) {
            result->decl_flags |= AST_DECL_FLAG_IS_NAKED;
        }

        if (is_noreturn) {
            result->decl_flags |= AST_DECL_FLAG_NORETURN;
        }

        if (is_foreign) {
            result->decl_flags |= AST_DECL_FLAG_FOREIGN;
        }

        if (is_compiler_func) {
            result->decl_flags |= AST_DECL_FLAG_COMPILER_FUNC;
        }

        return result;
    }

    AST_Declaration *ast_type_declaration_new(Allocator *allocator, AST_Type *type,
                                              AST_Identifier *identifier, Scope *scope)
    {
        File_Pos fp = { 0, 0, 0, string_ref("<builtin>") };
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::TYPE, identifier,
                                          scope, fp, fp);

        result->type = type;

        return result;
    }

    AST_Declaration *ast_structure_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   Array<AST_Declaration *> member_decls,
                                                   Array<AST_Declaration *> parameters,
                                                   Array<AST_Identifier *> usings,
                                                   Scope *parent_scope,
                                                   Scope *param_scope,
                                                   Scope *mem_scope,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        assert(param_scope);
        assert(param_scope->kind == Scope_Kind::PARAMETER);
        assert(mem_scope->kind == Scope_Kind::AGGREGATE);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::STRUCTURE,
                                          identifier, parent_scope, begin_fp, end_fp);

        result->structure.member_declarations = member_decls;
        result->structure.parameters = parameters;
        result->structure.usings = usings;
        result->structure.imported_members = {};

        result->structure.parameter_scope = param_scope;
        result->structure.member_scope = mem_scope;

        return result;
    }

    AST_Declaration *ast_union_declaration_new(Allocator *allocator,
                                               AST_Identifier *identifier,
                                               Array<AST_Declaration*> member_decls,
                                               Array<AST_Declaration*> parameters,
                                               Scope *parent_scope,
                                               Scope *param_scope,
                                               Scope *mem_scope,
                                               const File_Pos &begin_fp,
                                               const File_Pos &end_fp)
    {
        assert(param_scope);
        assert(param_scope->kind == Scope_Kind::PARAMETER);
        assert(mem_scope->kind == Scope_Kind::AGGREGATE);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::UNION,
                                          identifier, parent_scope, begin_fp, end_fp);

        result->union_decl.member_declarations = member_decls;
        result->union_decl.parameters = parameters;

        result->union_decl.parameter_scope = param_scope;
        result->union_decl.member_scope = mem_scope;

        return result;

    }

    AST_Declaration *ast_enum_declaration_new(Allocator *allocator,
                                              AST_Identifier *identifier,
                                              AST_Type_Spec *ast_ts,
                                              Array<AST_Declaration*> member_decls,
                                              Scope *parent_scope,
                                              Scope *member_scope,
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp)
    {
        assert(member_scope->kind == Scope_Kind::AGGREGATE);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::ENUM,
                                          identifier, parent_scope, begin_fp, end_fp);
        result->enum_decl.type_spec = ast_ts;
        result->enum_decl.member_declarations = member_decls;
        // result->enum_decl.member_scope = scope_new(allocator, Scope_Kind::AGGREGATE,
        //                                             parent_scope, member_decls.count);
        result->enum_decl.member_scope = member_scope;

        return result;
    }

    AST_Declaration *ast_typedef_declaration_new(Allocator *allocator,
                                                 AST_Identifier *identifier,
                                                 AST_Type_Spec *type_spec,
                                                 Scope *scope,
                                                 const File_Pos &begin_fp,
                                                 const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::TYPEDEF,
                                          identifier, scope, begin_fp, end_fp);
        result->typedef_decl.type_spec = type_spec;
        return result;
    }

    AST_Declaration *ast_poly_type_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   AST_Identifier *spec_ident,
                                                   Scope *scope,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::POLY_TYPE,
                                          identifier, scope,
                                          begin_fp, end_fp);

        result->poly_type.specification_identifier = spec_ident;

        return result;
    }

    AST_Declaration *ast_run_declaration_new(Allocator *allocator, AST_Expression *expression,
                                             Scope *scope,
                                             const File_Pos &bfp, const File_Pos &efp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::RUN, nullptr,
                                          scope, bfp, efp);
        result->run.expression = expression;

        return result;
    }

    AST_Declaration *ast_static_if_declaration_new(Allocator *allocator, AST_Expression *cond_expr,
                                                   Array<AST_Declaration *> then_decls,
                                                   Array<AST_Declaration *> else_decls,
                                                   Scope *then_scope, Scope *else_scope,
                                                   Scope *parent_scope,
                                                   const File_Pos &bfp, const File_Pos &efp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::STATIC_IF, nullptr,
                                          parent_scope, bfp, efp);

        result->static_if.cond_expression = cond_expr;
        result->static_if.then_declarations = then_decls;
        result->static_if.else_declarations = else_decls;
        result->static_if.then_scope = then_scope;
        result->static_if.else_scope = else_scope;

        return result;
    }

    AST_Declaration  *ast_static_assert_declaration_new(Allocator *allocator,
                                                        AST_Expression *cond_expr,
                                                        Scope *scope,
                                                        const File_Pos &bfp,
                                                        const File_Pos &efp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::STATIC_ASSERT,
                                          nullptr, scope, bfp, efp);
        result->static_assert_decl.cond_expression = cond_expr;
        return result;
    }

    AST_Declaration *ast_import_link_declaration_new(Allocator *allocator,
                                                     AST_Declaration *using_member,
                                                     AST_Declaration *imported_member,
                                                     Scope *scope)
    {
        assert(using_member->kind == AST_Declaration_Kind::VARIABLE);
        assert(imported_member->kind == AST_Declaration_Kind::VARIABLE ||
               imported_member->kind == AST_Declaration_Kind::IMPORT_LINK);

        auto bfp = using_member->begin_file_pos;
        auto efp = using_member->end_file_pos;

        auto identifier = ast_identifier_new(allocator, imported_member->identifier->atom, scope,
                                             bfp, efp);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::IMPORT_LINK,
                                          identifier, scope, bfp, efp);

        result->import_link.using_member = using_member;
        result->import_link.imported_member = imported_member;
        result->import_link.index_in_parent = -1;

        assert(imported_member->type);
        result->type = imported_member->type;
        result->flags |= AST_NODE_FLAG_RESOLVED_ID;
        result->flags |= AST_NODE_FLAG_TYPED;

        return result;
    }

    AST_Switch_Case *ast_switch_case_new(Allocator *allocator,
                                         Bucket_Array<AST_Expression *, 4> case_exprs,
                                         bool is_default, AST_Statement *body,
                                         Scope *scope,
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Switch_Case>(allocator, scope, begin_fp, end_fp);

        result->expressions = case_exprs;
        result->is_default = is_default;
        result->body = body;

        return result;
    }

    AST_Statement *ast_statement_new(Allocator *allocator, AST_Statement_Kind kind,
                                     Scope *scope,
                                     const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Statement>(allocator, scope, begin_fp, end_fp);

        result->kind = kind;

        return result;
    }

    AST_Statement *ast_block_statement_new(Allocator *allocator, Array<AST_Statement*> statements,
                                           Scope *parent_scope, Scope *block_scope,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(block_scope->kind == Scope_Kind::BLOCK);

        auto result = ast_statement_new(allocator, AST_Statement_Kind::BLOCK, parent_scope,
                                        begin_fp, end_fp);

        result->block.statements = statements;
        result->block.scope = block_scope;

        return result;
    }

    AST_Statement *ast_assignment_statement_new(Allocator *allocator, AST_Expression *ident_expr,
                                                AST_Expression *rhs_expr, Scope *scope,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::ASSIGNMENT, scope,
                                        begin_fp, end_fp);

        result->assignment.identifier_expression = ident_expr;
        result->assignment.rhs_expression = rhs_expr;

        return result;
    }

    AST_Statement *ast_return_statement_new(Allocator *allocator, AST_Expression *return_expr,
                                            Scope *scope,
                                            const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::RETURN, scope,
                                        begin_fp, end_fp);

        result->expression = return_expr;

        return result;
    }

    AST_Statement *ast_break_statement_new(Allocator *allocator, AST_Statement *target_stmt,
                                           Scope *scope,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::BREAK, scope,
                                        begin_fp, end_fp);
        result->break_stmt.break_from = target_stmt;
        return result;
    }

    AST_Statement *ast_declaration_statement_new(Allocator *allocator,
                                                 AST_Declaration *declaration,
                                                 Scope *scope,
                                                 const File_Pos &begin_fp,
                                                 const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::DECLARATION, scope,
                                        begin_fp, end_fp);

        result->declaration = declaration;

        return result;
    }

    AST_Statement *ast_expression_statement_new(Allocator *allocator, AST_Expression *expression,
                                                Scope *scope,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::EXPRESSION, scope,
                                        begin_fp, end_fp);

        result->expression = expression;

        return result;
    }

    AST_Statement *ast_while_statement_new(Allocator *allocator, AST_Expression *cond_expr,
                                           AST_Statement *body, Scope *body_scope,
                                           Scope *parent_scope,
                                           const File_Pos & begin_fp,
                                           const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::WHILE, parent_scope,
                                        begin_fp, end_fp);

        result->while_stmt.cond_expr = cond_expr;
        result->while_stmt.body = body;
        result->while_stmt.body_scope = body_scope;

        return result;
    }

    AST_Statement *ast_for_statement_new(Allocator *allocator,
                                         Array<AST_Statement *> init_statements,
                                         AST_Expression *cond_expr,
                                         AST_Declaration *it_decl,
                                         Array<AST_Statement *> step_statements,
                                         AST_Statement *body_stmt,
                                         Scope *for_scope,
                                         Scope *parent_scope,
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp)
    {
        assert(for_scope->kind == Scope_Kind::BLOCK);

        auto result = ast_statement_new(allocator, AST_Statement_Kind::FOR, parent_scope,
                                        begin_fp, end_fp);

        result->for_stmt.init_statements = init_statements;
        result->for_stmt.cond_expr = cond_expr;
        result->for_stmt.it_decl = it_decl;
        result->for_stmt.step_statements = step_statements;
        result->for_stmt.body_stmt = body_stmt;
        result->for_stmt.scope = for_scope;

        return result;
    }

    AST_Statement *ast_if_statement_new(Allocator *allocator, AST_Expression *cond_expr,
                                           AST_Statement *then_stmt, AST_Statement *else_stmt,
                                           Scope *then_scope, Scope *else_scope,
                                           Scope *parent_scope,
                                           const File_Pos & begin_fp, const File_Pos &end_fp)
    {
        assert(cond_expr);
        assert(then_stmt);
        assert(then_scope);

        if (else_stmt)
        {
            assert(else_scope);
        }

        auto result = ast_statement_new(allocator, AST_Statement_Kind::IF, parent_scope,
                                        begin_fp, end_fp);

        result->if_stmt.cond_expr = cond_expr;
        result->if_stmt.then_stmt = then_stmt;
        result->if_stmt.else_stmt = else_stmt;
        result->if_stmt.then_scope = then_scope;
        result->if_stmt.else_scope = else_scope;

        return result;
    }

    AST_Statement *ast_switch_statement_new(Allocator *allocator, AST_Expression *expression,
                                            uint64_t case_count,
                                            bool allow_incomplete,
                                            Scope *scope,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::SWITCH, scope,
                                        begin_fp, end_fp);

        result->switch_stmt.expression = expression;
        result->switch_stmt.default_case = nullptr;
        array_init(allocator, &result->switch_stmt.cases, case_count);
        result->switch_stmt.case_expr_count = 0;
        result->switch_stmt.allow_incomplete = allow_incomplete;

        return result;
    }

    AST_Expression *ast_expression_new(Allocator *allocator, AST_Expression_Kind kind,
                                       Scope *scope,
                                       const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Expression>(allocator, scope, begin_fp, end_fp);

        result->kind = kind;
        result->expr_flags = AST_EXPR_FLAG_NONE;
        result->infer_type_from = nullptr;

        return result;
    }

    AST_Expression *ast_identifier_expression_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  Scope *scope,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::IDENTIFIER, scope,
                                         begin_fp, end_fp);

        result->identifier = identifier;

        return result;
    }

    AST_Expression *ast_poly_identifier_expression_new(Allocator *allocator,
                                                       AST_Declaration *poly_type_decl,
                                                       Scope *scope,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::POLY_IDENTIFIER, scope,
                                         begin_fp, end_fp);

        result->poly_identifier.poly_type_decl = poly_type_decl;

        return result;
    }

    AST_Expression *ast_dot_expression_new(Allocator *allocator, AST_Dot_Expression_Kind kind,
                                           AST_Expression *parent_expr,
                                           AST_Identifier *child_ident, Scope *scope,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::DOT, scope,
                                         begin_fp, end_fp);

        assert(kind != AST_Dot_Expression_Kind::INVALID);

        result->dot.kind = kind;
        result->dot.parent_expression = parent_expr;
        result->dot.child_identifier = child_ident;
        result->dot.child_decl = nullptr;

        return result;
    }

    AST_Expression *ast_binary_expression_new(Allocator *allocator, Binary_Operator op,
                                              AST_Expression *lhs, AST_Expression *rhs,
                                              Scope *scope,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::BINARY, scope,
                                         begin_fp, end_fp);

        result->binary.op = op;
        result->binary.lhs = lhs;
        result->binary.rhs = rhs;

        return result;
    }

    AST_Expression *ast_unary_expression_new(Allocator *allocator, Unary_Operator op,
                                             AST_Expression *operand_expr, Scope *scope,
                                             const File_Pos &begin_fp,
                                             const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::UNARY, scope,
                                         begin_fp, end_fp);

        result->unary.op = op;
        result->unary.operand_expression = operand_expr;

        return result;
    }

    AST_Expression *ast_call_expression_new(Allocator *allocator, AST_Expression *ident_expr,
                                            Array<AST_Expression*> arg_expressions, Scope *scope,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CALL, scope,
                                         begin_fp, end_fp);

        result->call.ident_expression = ident_expr;
        result->call.arg_expressions = arg_expressions;
        result->call.callee_is_pointer = false;
        result->call.callee_declaration = nullptr;

        return result;
    }

    AST_Expression *ast_builtin_call_expression_new(Allocator *allocator,
                                                    AST_Identifier *identifier,
                                                    Array<AST_Expression*> arg_expressions,
                                                    Scope *scope,
                                                    const File_Pos &begin_fp,
                                                    const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::BUILTIN_CALL, scope,
                                         begin_fp, end_fp);

        result->builtin_call.identifier = identifier;
        result->builtin_call.arg_expressions = arg_expressions;

        return result;
    }

    AST_Expression *ast_addrof_expression_new(Allocator *allocator, AST_Expression *operand_expr,
                                              Scope *scope,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::ADDROF, scope,
                                         begin_fp, end_fp);

        result->addrof.operand_expr = operand_expr;

        return result;
    }

    AST_Expression *ast_compound_expression_new(Allocator *allocator,
                                                Array<AST_Expression*> exprs,
                                                AST_Type_Spec *type_spec,
                                                Scope *scope,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::COMPOUND, scope,
                                         begin_fp, end_fp);

        result->compound.expressions = exprs;
        result->compound.type_spec = type_spec;

        return result;
    }

    AST_Expression *ast_subscript_expression_new(Allocator *allocator,
                                                 AST_Expression *pointer_expr,
                                                 AST_Expression *index_expr,
                                                 Scope *scope,
                                                 const File_Pos &bfp, const File_Pos &efp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::SUBSCRIPT, scope,
                                         bfp, efp);

        result->subscript.pointer_expression = pointer_expr;
        result->subscript.index_expression = index_expr;
        return result;
    }

    AST_Expression *ast_cast_expression_new(Allocator *allocator, AST_Expression *operand_expr,
                                            AST_Type *target_type, Scope *scope,
                                            const File_Pos &bfp, const File_Pos &efp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CAST, scope, bfp, efp);
        result->cast.operand_expression = operand_expr;
        result->cast.target_type = target_type;
        return result;
    }

    AST_Expression *ast_integer_literal_expression_new(Allocator *allocator, int64_t value,
                                                       Scope *scope,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::INTEGER_LITERAL, scope,
                                         begin_fp, end_fp);

        result->integer_literal.s64 = value;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_float_literal_expression_new(Allocator *allocator, float f, double d,
                                                     Scope *scope,
                                                     const File_Pos & begin_fp,
                                                     const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::FLOAT_LITERAL, scope,
                                         begin_fp, end_fp);
        result->float_literal.r32 = f;
        result->float_literal.r64 = d;
        result->expr_flags |= AST_EXPR_FLAG_CONST;
        return result;
    }

    AST_Expression *ast_string_literal_expression_new(Allocator *allocator, Atom& atom,
                                                      Scope *scope,
                                                      const File_Pos &begin_fp,
                                                      const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::STRING_LITERAL, scope,
                                         begin_fp, end_fp);

        result->string_literal.atom = atom;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_char_literal_expression_new(Allocator *allocator, char c,
                                                    Scope *scope,
                                                    const File_Pos & begin_fp,
                                                    const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CHAR_LITERAL, scope,
                                         begin_fp, end_fp);

        result->char_literal.c = c;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_boolean_literal_expression_new(Allocator *allocator, bool value,
                                                       Scope *scope,
                                                       const File_Pos & begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::BOOL_LITERAL, scope,
                                         begin_fp, end_fp);

        result->bool_literal.value = value;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_null_literal_expression_new(Allocator *allocator, Scope *scope,
                                                    const File_Pos &begin_fp,
                                                    const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::NULL_LITERAL, scope,
                                         begin_fp, end_fp);
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_range_expression_new(Allocator *allocator,
                                             AST_Expression *begin_expr,
                                             AST_Expression *end_expr,
                                             Scope *scope)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::RANGE, scope,
                                         begin_expr->begin_file_pos, end_expr->end_file_pos);

        result->range.begin = begin_expr;
        result->range.end = end_expr;

        return result;
    }

    AST_Type_Spec *ast_type_spec_new(Allocator *allocator, AST_Type_Spec_Kind kind,
                                     Scope *scope,
                                     const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        AST_Type_Spec *result = ast_node_new<AST_Type_Spec>(allocator, scope, begin_fp, end_fp);

        result->kind = kind;
        result->type = nullptr;

        return result;
    }

    AST_Type_Spec *ast_identifier_type_spec_new(Allocator *allocator, AST_Identifier *identifier,
                                                Scope *scope,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::IDENTIFIER, scope,
                                        begin_fp, end_fp);

        result->identifier = identifier;

        return result;
    }

    AST_Type_Spec *ast_pointer_type_spec_new(Allocator *allocator, AST_Type_Spec *base_ts,
                                             Scope *scope,
                                             const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(base_ts);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::POINTER, scope,
                                        begin_fp, end_fp);

        result->base_type_spec = base_ts;

        return result;
    }

    AST_Type_Spec *ast_dot_type_spec_new(Allocator *allocator, AST_Expression *dot_expr,
                                         Scope *scope,
                                         const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(dot_expr->kind == AST_Expression_Kind::DOT);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::DOT, scope,
                                        begin_fp, end_fp);

        result->dot_expression = dot_expr;

        return result;
    }

    AST_Type_Spec *ast_function_type_spec_new(Allocator *allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec *return_type_spec,
                                              AST_Declaration *from_declaration,
                                              Scope *scope,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(allocator);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::FUNCTION, scope,
                                        begin_fp, end_fp);

        result->function.parameter_type_specs = param_type_specs;
        result->function.return_type_spec = return_type_spec;
        result->function.from_declaration = from_declaration;

        return result;
    }

    AST_Type_Spec *ast_array_type_spec_new(Allocator *allocator, AST_Expression *length_expr,
                                           AST_Type_Spec *element_ts, Scope *scope,
                                           const File_Pos & begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::ARRAY, scope,
                                        begin_fp, end_fp);

        result->array.length_expression = length_expr;
        result->array.element_type_spec = element_ts;

        return result;
    }

    AST_Type_Spec *ast_templated_type_spec_new(Allocator *allocator, AST_Expression *ident_expr,
                                               Array<AST_Expression*> arg_exprs, Scope *scope,
                                               const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER ||
               ident_expr->kind == AST_Expression_Kind::DOT);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::TEMPLATED, scope,
                                        begin_fp, end_fp);

        result->templated.ident_expression = ident_expr;
        result->templated.argument_expressions = arg_exprs;

        return result;
    }

    AST_Type_Spec *ast_poly_identifier_type_spec_new(Allocator *allocator, AST_Declaration *decl,
                                                     AST_Identifier *spec_ident,
                                                     Scope *scope,
                                                     const File_Pos &begin_fp,
                                                     const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::POLY_IDENTIFIER, scope,
                                        begin_fp, end_fp);

        result->poly_identifier.declaration = decl;
        result->poly_identifier.specification_identifier = spec_ident;

        return result;
    }

    AST_Type_Spec *ast_type_spec_from_type_new(Allocator *allocator, AST_Type *type, Scope *scope)
    {
        File_Pos fp = {};
        fp.file_name = string_ref("<from_type>");

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::FROM_TYPE,
                                        scope, fp, fp);
        result->type = type;
        return result;
    }

    AST_Type *ast_type_new(Allocator *allocator, AST_Type_Kind kind, uint64_t bit_size)
    {
        auto result = alloc_type<AST_Type>(allocator);
        static_cast<AST_Node*>(result)->kind = AST_Type::_kind;
        assert(result);
        result->kind = kind;
        result->bit_size = bit_size;
        result->pointer_to = nullptr;
        return result;
    }

    AST_Type *ast_integer_type_new(Allocator *allocator, uint64_t bit_size, bool sign)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::INTEGER, bit_size);
        result->integer.sign = sign;
        return result;
    }

    AST_Type *ast_float_type_new(Allocator *allocator, uint64_t bit_size)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::FLOAT, bit_size);
        return result;
    }

    AST_Type *ast_bool_type_new(Allocator *allocator, uint64_t bit_size)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::BOOL, bit_size);
        return result;
    }

    AST_Type *ast_pointer_type_new(Allocator *allocator, AST_Type *base_type)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::POINTER, Builtin::pointer_size);
        result->pointer.base = base_type;
        result->bit_size = Builtin::pointer_size;
        result->flags |= AST_NODE_FLAG_RESOLVED_ID;
        result->flags |= AST_NODE_FLAG_TYPED;
        result->flags |= AST_NODE_FLAG_SIZED;
        return result;
    }

    AST_Type *ast_function_type_new(Allocator *allocator, Array<AST_Type*> param_types,
                                    AST_Type *return_type)
    {
        // assert(false); // Copy param types
        auto result = ast_type_new(allocator, AST_Type_Kind::FUNCTION, Builtin::pointer_size);

        result->function.param_types = param_types;
        result->function.return_type = return_type;

        return result;
    }

    AST_Type *ast_structure_type_new(Allocator *allocator, AST_Declaration *declaration,
                                     Scope *member_scope)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::STRUCTURE, 0);
        result->structure.member_scope = member_scope;
        result->structure.declaration = declaration;
        result->structure.member_types = array_create<AST_Type *>(allocator, 4);

        return result;
    }

    AST_Type *ast_union_type_new(Allocator *allocator, AST_Declaration *declaration,
                                 Scope *member_scope)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::UNION, 0);
        result->union_type.member_scope = member_scope;
        result->union_type.declaration = declaration;
        result->union_type.member_types = array_create<AST_Type *>(allocator, 4);
        result->union_type.biggest_member_type = nullptr;

        return result;

    }

    AST_Type *ast_enum_type_new(Allocator *allocator, AST_Declaration *declaration,
                                AST_Type *base_type,
                                Scope *member_scope)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::ENUM, 0);
        result->enum_type.base_type = base_type;
        result->enum_type.member_scope = member_scope;
        result->enum_type.declaration = declaration;
        result->enum_type.unique_member_values = {};

        return result;
    }

    AST_Type *_ast_find_or_create_pointer_type(Allocator *allocator, AST_Type *base_type)
    {
        if (base_type->pointer_to) {
            assert(base_type->pointer_to->pointer.base == base_type);
            return base_type->pointer_to;
        }

        auto result = ast_pointer_type_new(allocator, base_type);
        base_type->pointer_to = result;

        return result;
    }

    AST_Type *_ast_create_array_type(Allocator *allocator, AST_Type *elem_type,
                                     int64_t elem_count)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::ARRAY, 0);

        result->array.element_type = elem_type;
        result->array.element_count = elem_count;

        return result;
    }

    AST_Declaration *ast_find_enum_member(AST_Type *enum_type,
                                          Const_Value member_value)
    {
        assert(enum_type->kind == AST_Type_Kind::ENUM);

        auto decl = enum_type->enum_type.declaration;

        assert(member_value.type == enum_type);

        for (int64_t i = 0; i < decl->enum_decl.member_declarations.count; i++)
        {
            auto mem_decl = decl->enum_decl.member_declarations[i];
            auto init_expr = mem_decl->constant.init_expression;

            if (init_expr->integer_literal.u64 == member_value.integer.u64)
            {
                return mem_decl;
            }
        }

        return nullptr;
    }

    bool ast_scope_add_declaration(AST_Builder *ast_builder, Scope *scope, AST_Declaration *decl)
    {
        assert(decl->identifier ||
               decl->kind == AST_Declaration_Kind::USING ||
               decl->kind == AST_Declaration_Kind::RUN ||
               decl->kind == AST_Declaration_Kind::STATIC_IF ||
               decl->kind == AST_Declaration_Kind::STATIC_ASSERT);

        AST_Declaration *redecl = nullptr;

        if (decl->identifier) {
            redecl = scope_find_declaration(scope, decl->identifier);
        }

        if (redecl) {
            zodiac_report_error(ast_builder->build_data, Zodiac_Error_Kind::REDECLARATION,
                                decl->identifier, "Redeclaration of identifier: '%s'",
                                decl->identifier->atom.data);
            zodiac_report_info(ast_builder->build_data, redecl->identifier,
                               "Previous declaration was here");
            return false;
        }

        scope_add_declaration(scope, decl);

        return true;
    }

    void ast_print_indent(String_Builder *sb, uint64_t indent)
    {
        for (uint64_t i = 0; i < indent; i++) {
            string_builder_append(sb, "    ");
        }
    }

    void ast_print(AST_Node *ast_node)
    {
        auto ca = c_allocator_get();

        String_Builder sb = {};
        string_builder_init(ca, &sb);

        ast_print(&sb, ast_node);

        String result = string_builder_to_string(ca, &sb);

        printf("%.*s", (int)result.length, result.data);

        string_free(ca, result);
        string_builder_free(&sb);
    }

    void ast_print(String_Builder *sb, AST_Node *ast_node)
    {
        assert(ast_node);

        switch (ast_node->kind) {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE: {
                auto module = (AST_Module*)ast_node;

                auto bl = bucket_array_first(&module->declarations);
                while (bl.bucket) {

                    auto p_decl = bucket_locator_get_ptr(bl);
                    auto decl = *p_decl;

                    ast_print_declaration(sb, decl, 0);
                    if (decl->kind == AST_Declaration_Kind::FUNCTION) {
                        string_builder_append(sb, "\n");
                    }

                    bucket_locator_advance(&bl);
                }
                break;
            }

            case AST_Node_Kind::IDENTIFIER: {
                auto ident = (AST_Identifier*)ast_node;
                string_builder_appendf(sb, "%s", ident->atom.data);
                break;
            }

            case AST_Node_Kind::DECLARATION: {
                auto ast_decl = static_cast<AST_Declaration*>(ast_node);
                ast_print_declaration(sb, ast_decl, 0);
                break;
            }

            case AST_Node_Kind::SWITCH_CASE: assert(false);

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }
    }

    void ast_print_declaration(String_Builder *sb, AST_Declaration *ast_decl, uint64_t indent,
                               bool newline/*=true*/)
    {
        if (ast_decl->kind == AST_Declaration_Kind::FUNCTION ||
            ast_decl->kind == AST_Declaration_Kind::STRUCTURE) {
            string_builder_append(sb, "\n");
        }

        ast_print_indent(sb, indent);

        if (ast_decl->decl_flags & AST_DECL_FLAG_IS_NAKED) {
            string_builder_append(sb, "#naked ");
        }

        if (ast_decl->kind == AST_Declaration_Kind::POLY_TYPE) {
            string_builder_append(sb, "$");
        }

        if (ast_decl->identifier) {
            string_builder_appendf(sb, "%s", ast_decl->identifier->atom.data);
        }

        switch (ast_decl->kind) {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT: {
                string_builder_append(sb, " :: import ");
                ast_print_expression(sb, ast_decl->import.ident_expression, 0);
                string_builder_append(sb, ";\n");
                break;
            }

            case AST_Declaration_Kind::USING: {
                string_builder_append(sb, "using ");
                ast_print_expression(sb, ast_decl->using_decl.ident_expr, 0);
                string_builder_append(sb, ";\n");
                break;
            }

            case AST_Declaration_Kind::VARIABLE:
            case AST_Declaration_Kind::CONSTANT: {
                string_builder_append(sb, " :");
                if (ast_decl->variable.type_spec) {
                    string_builder_append(sb, " ");
                    ast_print_type_spec(sb, ast_decl->variable.type_spec);
                    if (ast_decl->variable.init_expression) {
                        string_builder_append(sb, " ");
                    }
                }

                if (ast_decl->variable.init_expression) {
                    if (ast_decl->kind == AST_Declaration_Kind::VARIABLE) {
                        string_builder_append(sb, "= ");
                    } else {
                        string_builder_append(sb, ": ");
                    }

                    ast_print_expression(sb, ast_decl->variable.init_expression, 0);
                }
                string_builder_append(sb, ";");
                if (newline) string_builder_append(sb, "\n");
                break;
            }

            case AST_Declaration_Kind::PARAMETER: {
                if (ast_decl->parameter.type_spec) {
                    string_builder_append(sb, ": ");
                    ast_print_type_spec(sb, ast_decl->parameter.type_spec);
                }
                break;
            }

            case AST_Declaration_Kind::FUNCTION: {
                string_builder_append(sb, " :: func (");
                for (int64_t i = 0; i < ast_decl->function.parameter_declarations.count;
                     i++) {
                    if (i > 0) string_builder_append(sb, ", ");
                    auto param_decl = ast_decl->function.parameter_declarations[i];
                    ast_print_declaration(sb, param_decl, 0);
                }
                string_builder_append(sb, ")");
                auto function_ts = ast_decl->function.type_spec;
                if (function_ts->function.return_type_spec) {
                    string_builder_append(sb, " -> ");
                    ast_print_type_spec(sb, function_ts->function.return_type_spec);
                }

                if (ast_decl->function.body)
                    ast_print_statement(sb, ast_decl->function.body, indent);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::TYPEDEF: {
                string_builder_append(sb, " :: typedef ");
                ast_print_type_spec(sb, ast_decl->typedef_decl.type_spec, true);
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: {
                string_builder_append(sb, " :: struct");
                if (ast_decl->structure.parameters.count) {
                    string_builder_append(sb, "(");
                    for (int64_t i = 0; i < ast_decl->structure.parameters.count; i++) {
                        if (i > 0) string_builder_append(sb, ", ");
                        auto param_decl = ast_decl->structure.parameters[i];
                        ast_print_declaration(sb, param_decl, 0);
                    }
                    string_builder_append(sb, ")");
                }
                string_builder_append(sb, "\n");
                ast_print_indent(sb, indent);
                string_builder_append(sb, "{\n");
                for (int64_t i = 0; i < ast_decl->structure.member_declarations.count; i++) {
                    auto mem_decl = ast_decl->structure.member_declarations[i];
                    ast_print_declaration(sb, mem_decl, indent + 1);
                }
                ast_print_indent(sb, indent);
                string_builder_append(sb, "}\n");
                break;
            }

            case AST_Declaration_Kind::UNION: assert(false);

            case AST_Declaration_Kind::ENUM: {
                string_builder_append(sb, " :: enum\n");
                ast_print_indent(sb, indent);
                string_builder_append(sb, "{\n");

                for (int64_t i = 0; i < ast_decl->enum_decl.member_declarations.count; i++) {
                    auto mem_decl = ast_decl->enum_decl.member_declarations[i];
                    ast_print_declaration(sb, mem_decl, indent + 1);
                }

                ast_print_indent(sb, indent);
                string_builder_append(sb, "}\n");
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE: {
                if (ast_decl->poly_type.specification_identifier) {
                    string_builder_append(sb, "/");
                    ast_print(sb, ast_decl->poly_type.specification_identifier);
                }
                break;
            }

            case AST_Declaration_Kind::RUN: {
                string_builder_append(sb, "#run ");
                ast_print_expression(sb, ast_decl->run.expression, 0);
                string_builder_append(sb, "\n");
                break;
            }

            case AST_Declaration_Kind::STATIC_IF: {
                string_builder_append(sb, "#if (");
                ast_print_expression(sb, ast_decl->static_if.cond_expression, 0);
                string_builder_append(sb, ") {\n");

                for (int64_t i = 0; i < ast_decl->static_if.then_declarations.count; i++) {
                    auto decl = ast_decl->static_if.then_declarations[i];
                    ast_print_declaration(sb, decl, indent + 1);
                    if (decl->kind == AST_Declaration_Kind::FUNCTION) {
                        string_builder_append(sb, "\n");
                    }
                }

                ast_print_indent(sb, indent);
                string_builder_append(sb, "}");

                auto else_decls = ast_decl->static_if.else_declarations;

                if (else_decls.count == 1 &&
                    else_decls[0]->kind == AST_Declaration_Kind::STATIC_IF) {
                    string_builder_append(sb, " else ");
                    ast_print_declaration(sb, else_decls[0], 0);
                } else {
                    string_builder_append(sb, " else {\n");
                    for (int64_t i = 0; i < else_decls.count; i++) {
                        ast_print_declaration(sb, else_decls[i], indent + 1);
                    }
                    string_builder_append(sb, "\n");
                    ast_print_indent(sb, indent);
                    string_builder_append(sb, "}\n");
                }

                break;
            }

            case AST_Declaration_Kind::STATIC_ASSERT: {
                string_builder_append(sb, "static_assert(");
                ast_print_expression(sb, ast_decl->static_assert_decl.cond_expression, 0);
                string_builder_append(sb, ");");
                break;
            }

            case AST_Declaration_Kind::IMPORT_LINK: assert(false);
        }
    }

    void ast_print_statement(String_Builder *sb, AST_Statement *ast_stmt, uint64_t indent,
                             bool newline/*=false*/)
    {
        ast_print_indent(sb, indent);

        switch (ast_stmt->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: {
                string_builder_appendf(sb, "\n");
                ast_print_indent(sb, indent);
                string_builder_appendf(sb, "{\n");
                for (int64_t i = 0; i < ast_stmt->block.statements.count; i++) {
                    if (i > 0) string_builder_appendf(sb, "\n");
                    ast_print_statement(sb, ast_stmt->block.statements[i], indent + 1);
                }
                string_builder_appendf(sb, "\n");
                ast_print_indent(sb, indent);
                string_builder_appendf(sb, "}");
                if (newline) string_builder_appendf(sb, "\n");
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: {
                ast_print_expression(sb, ast_stmt->assignment.identifier_expression, 0);
                string_builder_appendf(sb, " = ");
                ast_print_expression(sb, ast_stmt->assignment.rhs_expression, 0);
                if (newline) string_builder_appendf(sb, "\n");
                break;
            }

            case AST_Statement_Kind::RETURN: {
                string_builder_appendf(sb, "return");
                if (ast_stmt->expression) {
                    string_builder_appendf(sb, " ");
                    ast_print_expression(sb, ast_stmt->expression, 0);
                }
                string_builder_appendf(sb, ";\n");
                break;
            }

            case AST_Statement_Kind::BREAK: {
                string_builder_appendf(sb, "break;");
                if (newline) string_builder_appendf(sb, "\n");
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
                ast_print_declaration(sb, ast_stmt->declaration, 0, false);
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
                ast_print_expression(sb, ast_stmt->expression, 0);
                string_builder_appendf(sb, ";");
                if (newline) string_builder_appendf(sb, "\n");
                break;
            }

            case AST_Statement_Kind::WHILE: {
                string_builder_appendf(sb, "while (");
                ast_print_expression(sb, ast_stmt->while_stmt.cond_expr, 0);
                string_builder_appendf(sb, ")");
                ast_print_statement(sb, ast_stmt->while_stmt.body, indent, false);
                break;
            }

            case AST_Statement_Kind::FOR: {
                string_builder_appendf(sb, "for (");
                for (int64_t i = 0; i < ast_stmt->for_stmt.init_statements.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    auto init_stmt = ast_stmt->for_stmt.init_statements[i];
                    ast_print_statement(sb, init_stmt, 0);
                }
                string_builder_appendf(sb, " ");
                ast_print_expression(sb, ast_stmt->for_stmt.cond_expr, 0);
                string_builder_appendf(sb, "; ");

                for (int64_t i = 0; i < ast_stmt->for_stmt.step_statements.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    auto step_stmt = ast_stmt->for_stmt.step_statements[i];
                    ast_print_statement(sb, step_stmt, 0);
                }
                string_builder_appendf(sb, ")");
                ast_print_statement(sb, ast_stmt->for_stmt.body_stmt, indent);
                break;
            }

            case AST_Statement_Kind::IF: {
                string_builder_appendf(sb, "if (");
                ast_print_expression(sb, ast_stmt->if_stmt.cond_expr, 0);
                string_builder_appendf(sb, ")");

                auto then_stmt = ast_stmt->if_stmt.then_stmt;
                auto then_indent = indent;
                if (then_stmt->kind != AST_Statement_Kind::BLOCK) {
                    string_builder_appendf(sb, "\n");
                    then_indent += 1;
                }

                ast_print_statement(sb, then_stmt, then_indent);

                auto else_stmt = ast_stmt->if_stmt.else_stmt;
                if (else_stmt) {
                    ast_print_indent(sb, indent);
                    string_builder_appendf(sb, "else");

                    auto else_indent = indent;
                    if (else_stmt->kind == AST_Statement_Kind::IF) {
                        string_builder_appendf(sb, "\n");
                    } else if (else_stmt->kind != AST_Statement_Kind::BLOCK) {
                        string_builder_appendf(sb, "\n");
                        else_indent += 1;
                    }

                    ast_print_statement(sb, else_stmt, else_indent);
                }
                break;
            }

            case AST_Statement_Kind::SWITCH: {
                string_builder_appendf(sb, "switch (");
                ast_print_expression(sb, ast_stmt->switch_stmt.expression, 0);
                string_builder_appendf(sb, ")\n");
                ast_print_indent(sb, indent);
                string_builder_appendf(sb, "{\n");

                indent += 1;

                for (int i = 0; i < ast_stmt->switch_stmt.cases.count; i++) {
                    ast_print_indent(sb, indent);

                    AST_Switch_Case *switch_case = ast_stmt->switch_stmt.cases[i];

                    if (switch_case->is_default) {
                        string_builder_appendf(sb, "default:");
                    } else {
                        string_builder_appendf(sb, "case ");

                        auto el = bucket_array_first(&switch_case->expressions);
                        bool first = true;
                        while (el.bucket) {
                            auto case_expr = *bucket_locator_get_ptr(el);
                            if (first) string_builder_appendf(sb, ", ");

                            ast_print_expression(sb, case_expr, 0);

                            first = false;
                            bucket_locator_advance(&el);
                        }

                        string_builder_appendf(sb, ":");
                    }

                    ast_print_statement(sb, switch_case->body, indent);

                    string_builder_appendf(sb, "\n");
                }

                indent -= 1;

                ast_print_indent(sb, indent);
                string_builder_appendf(sb, "}\n");
                break;
            }
        }
    }

    void ast_print_type_spec(String_Builder *sb, AST_Type_Spec *type_spec, bool newline /*=false*/)
    {
        switch (type_spec->kind) {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER: {
                string_builder_appendf(sb, "%s", type_spec->identifier->atom.data);
                break;
            }

            case AST_Type_Spec_Kind::POINTER: {
                string_builder_appendf(sb, "*");
                ast_print_type_spec(sb, type_spec->base_type_spec);
                break;
            }

            case AST_Type_Spec_Kind::DOT: {
                ast_print_expression(sb, type_spec->dot_expression, 0);
                break;
            }

            case AST_Type_Spec_Kind::FUNCTION: {
                string_builder_appendf(sb, "func (");
                for (int64_t i = 0; i < type_spec->function.parameter_type_specs.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    ast_print_type_spec(sb, type_spec->function.parameter_type_specs[i]);
                }
                string_builder_appendf(sb, ")");
                auto return_ts = type_spec->function.return_type_spec;
                if (return_ts) {
                    string_builder_appendf(sb, " -> ");
                    if (return_ts->kind == AST_Type_Spec_Kind::FROM_TYPE) {
                        ast_print_type(sb, return_ts->type);
                    } else {
                        ast_print_type_spec(sb, return_ts);
                    }
                }
                break;
            }

            case AST_Type_Spec_Kind::ARRAY: {
                string_builder_appendf(sb, "[]");
                ast_print_type_spec(sb, type_spec->array.element_type_spec);
                break;
            }

            case AST_Type_Spec_Kind::TEMPLATED: {
                ast_print_expression(sb, type_spec->templated.ident_expression, 0);
                string_builder_appendf(sb, "(");
                for (int64_t i = 0; i < type_spec->templated.argument_expressions.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    ast_print_expression(sb, type_spec->templated.argument_expressions[i], 0);
                }
                string_builder_appendf(sb, ")");
                break;
            }

            case AST_Type_Spec_Kind::POLY_IDENTIFIER: {
                string_builder_appendf(sb, "$");
                ast_print(sb, type_spec->poly_identifier.declaration);
                if (type_spec->poly_identifier.specification_identifier) {
                    string_builder_appendf(sb, "/");
                    ast_print(sb, type_spec->poly_identifier.specification_identifier);
                }
                break;
            }

            case AST_Type_Spec_Kind::FROM_TYPE: {
                string_builder_appendf(sb, "type_spec_from_type(");
                ast_print_type(sb, type_spec->type);
                string_builder_appendf(sb, ")");
                break;
            }
        }

        if (newline) string_builder_appendf(sb, "\n");
    }

    void ast_print_expression(String_Builder *sb, AST_Expression *ast_expr, uint64_t indent)
    {
        ast_print_indent(sb, indent);

        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                string_builder_appendf(sb, "%s", ast_expr->identifier->atom.data);
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: {
                ast_print(sb, ast_expr->poly_identifier.poly_type_decl);
                break;
            }

            case AST_Expression_Kind::DOT: {
                ast_print_expression(sb, ast_expr->dot.parent_expression, 0);
                string_builder_appendf(sb, ".%s", ast_expr->dot.child_identifier->atom.data);
                break;
            }

            case AST_Expression_Kind::BINARY: {
                ast_print_expression(sb, ast_expr->binary.lhs, 0);
                switch (ast_expr->binary.op) {
                    case BINOP_INVALID: assert(false);
                    case BINOP_EQ: string_builder_appendf(sb, " == ");        break;
                    case BINOP_NEQ: string_builder_appendf(sb, " != ");       break;
                    case BINOP_LT: string_builder_appendf(sb, " < ");         break;
                    case BINOP_LTEQ: string_builder_appendf(sb, " <= ");      break;
                    case BINOP_GT: string_builder_appendf(sb, " > ");         break;
                    case BINOP_GTEQ: string_builder_appendf(sb, " >= ");      break;
                    case BINOP_ADD: string_builder_appendf(sb, " + ");        break;
                    case BINOP_SUB: string_builder_appendf(sb, " - ");        break;
                    case BINOP_REMAINDER: string_builder_appendf(sb, " %% "); break;
                    case BINOP_MUL: string_builder_appendf(sb, "  *");        break;
                    case BINOP_DIV: string_builder_appendf(sb, " / ");        break;
                    case BINOP_LSHIFT: string_builder_appendf(sb, " << ");    break;
                    case BINOP_RSHIFT: string_builder_appendf(sb, " >> ");    break;
                    case BINOP_OR: string_builder_appendf(sb, " | ");         break;
                    case BINOP_AND: string_builder_appendf(sb, " & ");        break;
                }
                ast_print_expression(sb, ast_expr->binary.rhs, 0);
                break;
            }

            case AST_Expression_Kind::UNARY: {
                switch(ast_expr->unary.op) {
                    case UNOP_INVALID: assert(false); break;
                    case UNOP_DEREF: string_builder_appendf(sb, "<"); break;
                    case UNOP_MINUS: string_builder_appendf(sb, "-"); break;
                    case UNOP_NOT:   string_builder_appendf(sb, "!"); break;
                }

                ast_print_expression(sb, ast_expr->unary.operand_expression, 0);
                break;
            }

            case AST_Expression_Kind::CALL: {
                ast_print_expression(sb, ast_expr->call.ident_expression, 0);
                string_builder_appendf(sb, "(");
                for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    ast_print_expression(sb, ast_expr->call.arg_expressions[i], 0);
                }
                string_builder_appendf(sb, ")");
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {
                string_builder_appendf(sb, "%s", ast_expr->builtin_call.identifier->atom.data);
                string_builder_appendf(sb, "(");
                for (int64_t i = 0; i < ast_expr->builtin_call.arg_expressions.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    ast_print_expression(sb, ast_expr->builtin_call.arg_expressions[i], 0);
                }
                string_builder_appendf(sb, ")");
                break;
            }

            case AST_Expression_Kind::ADDROF: {
                string_builder_appendf(sb, "*");
                ast_print_expression(sb, ast_expr->addrof.operand_expr, 0);
                break;
            }

            case AST_Expression_Kind::COMPOUND: {

                if (ast_expr->compound.type_spec) {
                    ast_print_type_spec(sb, ast_expr->compound.type_spec);
                    string_builder_appendf(sb, " ");
                }

                string_builder_appendf(sb, "{");
                for (int64_t i = 0; i < ast_expr->compound.expressions.count; i++) {
                    if (i > 0) string_builder_appendf(sb, ", ");
                    else string_builder_appendf(sb, " ");

                    ast_print_expression(sb, ast_expr->compound.expressions[i], 0);
                }
                string_builder_appendf(sb, " }");
                break;
            }

            case AST_Expression_Kind::SUBSCRIPT: {
                ast_print_expression(sb, ast_expr->subscript.pointer_expression, indent);
                string_builder_appendf(sb, "[");
                ast_print_expression(sb, ast_expr->subscript.index_expression, 0);
                string_builder_appendf(sb, "]");
                break;
            }

            case AST_Expression_Kind::CAST: {
                string_builder_appendf(sb, "@compiler_cast(");
                ast_print_type(sb, ast_expr->cast.target_type);
                string_builder_appendf(sb, ", ");
                ast_print_expression(sb, ast_expr->cast.operand_expression, 0);
                string_builder_appendf(sb, ")");
                break;
            };

            case AST_Expression_Kind::INTEGER_LITERAL: {
                string_builder_appendf(sb, "%" PRId64, ast_expr->integer_literal.s64);
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: {
                string_builder_appendf(sb, "%f", ast_expr->float_literal.r32);
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: {
                string_builder_appendf(sb, "\"");
                for (uint64_t i = 0; i < ast_expr->string_literal.atom.length; i++) {
                    char c;
                    if (parser_make_escape_char(ast_expr->string_literal.atom.data[i], &c)) {
                        string_builder_appendf(sb, "\\%c", c);
                    } else {
                        string_builder_appendf(sb, "%c", c);
                    }
                }
                string_builder_appendf(sb, "\"");
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL: {
                const char *prefix = "";
                char c;
                if (parser_make_escape_char(ast_expr->char_literal.c, &c)) {
                    prefix = "\\";
                }
                string_builder_appendf(sb, "'%s%c'", prefix, c);
                break;
            }

            case AST_Expression_Kind::BOOL_LITERAL: {
                string_builder_appendf(sb, "%s", ast_expr->bool_literal.value ? "true" : "false");
                break;
            }

            case AST_Expression_Kind::NULL_LITERAL: {
                string_builder_appendf(sb, "null");
                break;
            }

            case AST_Expression_Kind::RANGE: {
                ast_print_expression(sb, ast_expr->range.begin, 0);
                string_builder_appendf(sb, " .. ");
                ast_print_expression(sb, ast_expr->range.end, 0);
                break;
            }
        }
    }

    void ast_print_scope(Allocator *allocator, AST_Node *anode)
    {
        assert(anode);

        String_Builder sb = {};
        string_builder_init(allocator, &sb);

        ast_print_scope(&sb, anode);

        auto str = string_builder_to_string(allocator, &sb);
        printf("%s\n", str.data);
        free(allocator, str.data);

        string_builder_free(&sb);
    }

    void ast_print_scope(String_Builder *sb, AST_Node *anode, int64_t indent/*=0*/)
    {
        switch (anode->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto ast_module = static_cast<AST_Module*>(anode);
                scope_print(sb, ast_module->module_scope, indent);
                break;
            }

            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto ast_decl = static_cast<AST_Declaration*>(anode);
                ast_print_declaration_scopes(sb, ast_decl, indent);
                break;
            }

            case AST_Node_Kind::SWITCH_CASE: assert(false);

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }
    }

    void ast_print_scope_indent(String_Builder *sb, int64_t indent)
    {
        for (int64_t i = 0; i < indent; i ++) string_builder_append(sb, "    ");
    }

    void ast_print_declaration_scopes(String_Builder *sb, AST_Declaration *ast_decl,
                                      int64_t indent)
    {
        assert(sb);
        switch (ast_decl->kind) {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT: {
                string_builder_append(sb, " (import)");
                break;
            }

            case AST_Declaration_Kind::USING: assert(false);

            case AST_Declaration_Kind::VARIABLE: {
                string_builder_append(sb, " (variable)");
                break;
            }

            case AST_Declaration_Kind::CONSTANT: {
                string_builder_append(sb, " (constant)");
                break;
            }

            case AST_Declaration_Kind::PARAMETER: {
                string_builder_append(sb, " (param)");
                break;
            }

            case AST_Declaration_Kind::FUNCTION: {
                string_builder_append(sb, " (func)\n");
                scope_print(sb, ast_decl->function.parameter_scope, indent);
                if (ast_decl->function.body)
                    scope_print(sb, ast_decl->function.body->block.scope, indent);
                break;
            }

            case AST_Declaration_Kind::TYPE: {
                string_builder_append(sb, " (type)");
                break;
            }

            case AST_Declaration_Kind::TYPEDEF: assert(false);

            case AST_Declaration_Kind::STRUCTURE: {
                string_builder_append(sb, " (struct)\n");
                if (ast_decl->structure.parameters.count > 0) {
                    scope_print(sb, ast_decl->structure.parameter_scope, indent);
                }
                scope_print(sb, ast_decl->structure.member_scope, indent);
                break;
            }

            case AST_Declaration_Kind::UNION: assert(false);

            case AST_Declaration_Kind::ENUM: assert(false);

            case AST_Declaration_Kind::POLY_TYPE: {
                string_builder_append(sb, " (poly_param)");
                break;
            }

            case AST_Declaration_Kind::RUN: assert(false);
            case AST_Declaration_Kind::STATIC_IF: assert(false);

            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);

            case AST_Declaration_Kind::IMPORT_LINK: assert(false);
        }
    }

    void ast_print_type(AST_Type *type)
    {
        auto ta = temp_allocator_get();

        String_Builder sb = {};
        string_builder_init(ta, &sb, 16);

        ast_print_type(&sb, type);

        String result = string_builder_to_string(ta, &sb);

        printf("%.*s", (int)result.length, result.data);
    }

    void ast_print_type(String_Builder *sb, AST_Type *type)
    {
        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);

            case AST_Type_Kind::INTEGER: {
                if (type->integer.sign) string_builder_append(sb, "s");
                else string_builder_append(sb, "u");

                string_builder_appendf(sb, "%" PRIu64, type->bit_size);
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (type->bit_size == 32) string_builder_append(sb, "float");
                else if (type->bit_size == 64) string_builder_append(sb, "double");
                else assert(false);
                break;
            }

            case AST_Type_Kind::BOOL: {
                string_builder_appendf(sb, "b%" PRIu64, type->bit_size);
                break;
            }

            case AST_Type_Kind::POINTER: {
                string_builder_append(sb, "*");
                ast_print_type(sb, type->pointer.base);
                break;
            }

            case AST_Type_Kind::VOID: {
                string_builder_append(sb, "void");
                break;
            }

            case AST_Type_Kind::FUNCTION: {
                string_builder_append(sb, "func(");
                for (int64_t i = 0; i < type->function.param_types.count; i++) {
                    if (i > 0) string_builder_append(sb, ", ");
                    auto param_type = type->function.param_types[i];
                    ast_print_type(sb, param_type);
                }
                string_builder_append(sb, ") -> ");
                ast_print_type(sb, type->function.return_type);
                break;
            }

            case AST_Type_Kind::STRUCTURE: {
                auto decl = type->structure.declaration;
                string_builder_appendf(sb, "struct(%s)", decl->identifier->atom.data);
                break;
            }

            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: {
                auto decl = type->enum_type.declaration;
                string_builder_appendf(sb, "enum(%s)", decl->identifier->atom.data);
                break;
            }

            case AST_Type_Kind::ARRAY: {
                string_builder_appendf(sb, "[%" PRId64 "]", type->array.element_count);
                ast_print_type(sb, type->array.element_type);
                break;
            }
        }
    }

    String ast_type_to_string(Allocator *allocator, AST_Type *type)
    {
        String_Builder sb;
        string_builder_init(allocator, &sb, 32);

        ast_print_type(&sb, type);

        auto result = string_builder_to_string(allocator, &sb);

        string_builder_free(&sb);

        return result;
    }

    String ast_type_to_tstring(AST_Type *type)
    {
        auto ta = temp_allocator_get();
        return ast_type_to_string(ta, type);
    }
}
