pub mod ast_writer;
pub mod compile;
pub mod output_rust;

#[cfg(test)]
mod ascript_tests
{
    use grammar::compile_test_grammar;

    use crate::ascript::ast_writer::ASTWriter;
    use crate::ascript::compile::compile_reduce_function_expressions;
    use crate::ascript::compile::compile_struct_type;
    use crate::ascript::output_rust::output_rust;
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
    use crate::primitives::*;

    #[test]
    fn test_output_rust_on_practical_grammar()
    {
        let grammar = compile_test_grammar(
            "
            <> A > \\vec num num^tom num f:ast { { t_Vec, x:f32($tom), y:f32($3), z:f32($4), first: { t_Num, val:u32($1) } } }
            
            <> num > \\temp g:num 
            ",
        );

        let mut ascript = AScriptStore::new();

        let errors =
            compile_reduce_function_expressions(&grammar, &mut ascript);

        for error in &errors {
            println!("{}", error);
        }

        assert!(errors.is_empty());

        assert_eq!(ascript.struct_table.len(), 2);

        let mut writer = ASTWriter::<String>::new(String::new());

        output_rust(&grammar, &ascript, &mut writer);

        println!("{}", writer.into_string());
    }

    #[test]
    fn test_output_rust_on_trivial_grammar()
    {
        let grammar = compile_test_grammar(
            "
            <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
            | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
            ",
        );

        let mut ascript = AScriptStore::new();

        let errors =
            compile_reduce_function_expressions(&grammar, &mut ascript);

        for error in &errors {
            println!("{}", error);
        }

        assert!(errors.is_empty());

        assert_eq!(ascript.struct_table.len(), 1);

        let mut writer = ASTWriter::<String>::new(String::new());

        output_rust(&grammar, &ascript, &mut writer);

        println!("{}", writer.into_string());
    }

    #[test]
    fn test_parse_errors_when_struct_type_is_missing()
    {
        let ast = compile_ascript_ast(
            " { c_Test }".as_bytes().iter().cloned().collect::<Vec<_>>(),
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

    fn create_dummy_body() -> crate::primitives::Body
    {
        crate::primitives::Body {
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
            " { t_TestA, t_TestB, t_TestC }"
                .as_bytes()
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
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
        let astA = compile_ascript_ast(
            " { t_TestA, apple: u32 }"
                .as_bytes()
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
        );
        assert!(astA.is_ok());

        let astB = compile_ascript_ast(
            " { t_TestA, apple: i64 }"
                .as_bytes()
                .iter()
                .cloned()
                .collect::<Vec<_>>(),
        );

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

        let grammar_ast = compile_grammar_ast(
            grammar.as_bytes().iter().cloned().collect::<Vec<_>>(),
        );

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
