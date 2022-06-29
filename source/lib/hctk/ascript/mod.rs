pub mod compile_ascript;

#[cfg(test)]
mod ascript_tests
{
    use grammar::compile_test_grammar;

    use crate::ascript::compile_ascript::compile_reduce_function_expressions;
    use crate::ascript::compile_ascript::compile_struct_type;
    use crate::debug::grammar;
    use crate::grammar::data::ast::ASTNode;
    use crate::grammar::data::ast::AST_Property;
    use crate::grammar::data::ast::AST_Struct;
    use crate::grammar::data::ast::AST_TypeId;
    use crate::grammar::data::ast::Ascript;
    use crate::grammar::data::ast::Ascript as AST_AScript;
    use crate::grammar::data::ast::Body;
    use crate::grammar::data::ast::Production;
    use crate::grammar::data::ast::Reduce;
    use crate::grammar::data::ast::AST_STRING;
    use crate::grammar::parse::compile_ascript_ast;
    use crate::grammar::parse::compile_grammar_ast;
    use crate::types::*;

    #[test]
    fn test_parse_errors_when_struct_type_is_missing()
    {
        let ast = compile_ascript_ast(" { c_Test }".as_bytes().to_vec());

        assert!(ast.is_ok());

        if let ASTNode::AST_Struct(ast_struct) = ast.unwrap() {
            let (_, errors) = compile_struct_type(
                &ast_struct,
                &create_dummy_body(),
                &mut AScriptStore::new(),
                &GrammarStore::default(),
            );

            for error in &errors {
                println!("{}", error);
            }

            assert_eq!(errors.len(), 1);
        } else {
            panic!("Value is not a struct");
        }
    }

    fn create_dummy_body() -> crate::types::Body
    {
        crate::types::Body {
            bytecode_id: 0,
            id: BodyId::new(&ProductionId(0), 0),
            length: 0,
            origin_location: Token::new(),
            production: ProductionId(0),
            reduce_fn_ids: vec![],
            symbols: vec![],
        }
    }

    #[test]
    fn test_parse_errors_when_struct_type_is_redefined()
    {
        let ast = compile_ascript_ast(
            " { t_TestA, t_TestB, t_TestC }".as_bytes().to_vec(),
        );

        assert!(ast.is_ok());

        if let ASTNode::AST_Struct(ast_struct) = ast.unwrap() {
            let (_, errors) = compile_struct_type(
                &ast_struct,
                &create_dummy_body(),
                &mut AScriptStore::new(),
                &GrammarStore::default(),
            );

            for error in &errors {
                println!("{}", error);
            }

            assert_eq!(errors.len(), 1);
        } else {
            panic!("Value is not a struct");
        }
    }

    #[test]
    fn test_parse_errors_when_struct_prop_type_is_redefined()
    {
        let astA =
            compile_ascript_ast(" { t_TestA, apple: u32 }".as_bytes().to_vec());
        assert!(astA.is_ok());

        let astB =
            compile_ascript_ast(" { t_TestA, apple: i64 }".as_bytes().to_vec());

        assert!(astB.is_ok());

        let mut store = AScriptStore::new();

        if let ASTNode::AST_Struct(ast_struct) = astA.unwrap() {
            let (_, errors) = compile_struct_type(
                &ast_struct,
                &create_dummy_body(),
                &mut store,
                &GrammarStore::default(),
            );

            assert!(errors.is_empty());

            if let ASTNode::AST_Struct(ast_struct) = astB.unwrap() {
                let (_, errors) = compile_struct_type(
                    &ast_struct,
                    &create_dummy_body(),
                    &mut store,
                    &GrammarStore::default(),
                );

                for error in &errors {
                    println!("{}", error);
                }

                assert_eq!(errors.len(), 1);
            } else {
                panic!("Value is not a struct");
            }
        } else {
            panic!("Value is not a struct");
        }
    }

    #[test]
    fn test_parse_errors_when_production_has_differing_return_types()
    {
        let grammar = compile_test_grammar(
            "
            <> A > \\1 f:ast { { t_Test } } 
            | \\a 
        ",
        );

        let mut store = AScriptStore::new();

        let errors = compile_reduce_function_expressions(&grammar, &mut store);

        for error in &errors {
            println!("{}", error);
        }

        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_ASTs_are_defined_for_ascript_return_functions()
    {
        let grammar =
            "<> A > \\1 f:ast { { t_Test, val: str($1) } } ".to_string();

        let grammar_ast = compile_grammar_ast(grammar.as_bytes().to_vec());

        match grammar_ast {
            Ok(grammar_ast) => {
                let content = &grammar_ast.content;
                println!("{:#?}", grammar_ast);

                match &content[0] {
                    ASTNode::Production(box Production { bodies, .. }) => {
                        if let ASTNode::Body(box Body {
                            reduce_function, ..
                        }) = &bodies[0]
                        {
                            if let ASTNode::Ascript(box Ascript {
                                ast, ..
                            }) = reduce_function
                            {
                                if let ASTNode::AST_Struct(box AST_Struct {
                                    props,
                                    ..
                                }) = ast
                                {
                                    assert_eq!(props.len(), 2);
                                    if let ASTNode::AST_TypeId(
                                        box AST_TypeId { value, .. },
                                    ) = &props[0]
                                    {
                                        assert_eq!(value, "t_Test")
                                    } else {
                                        panic!("Incorrect type name");
                                    }

                                    if let ASTNode::AST_Property(
                                        box AST_Property { id, value, .. },
                                    ) = &props[1]
                                    {
                                        assert_eq!(id, "val");

                                        if let ASTNode::AST_STRING(..) = value {
                                        } else {
                                            panic!("Prop is not a string");
                                        }
                                    } else {
                                        panic!("Incorrect prop");
                                    }
                                } else {
                                    panic!("Script value is not a struct.")
                                }
                            } else {
                                panic!("AScripT expression not found.")
                            }
                        } else {
                            panic!("Body not found.")
                        }
                    }
                    _ => panic!("Production not found."),
                }
            }
            Err(err) => {
                println!("error\n{}", err);

                // panic!("Failed to compile grammar ast")
            }
        }
    }
}
