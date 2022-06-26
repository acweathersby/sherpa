use std::collections::BTreeSet;

use crate::ascript::compile::compile_reduce_function_expressions;
use crate::ascript::compile::get_struct_type_from_node;
use crate::debug::compile_test_grammar;
use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::AST_IndexReference;
use crate::grammar::data::ast::AST_Property;
use crate::grammar::data::ast::AST_NUMBER;
use crate::grammar::data::ast::AST_U32;
use crate::primitives::grammar::ReduceFunction;
use crate::primitives::Body;
use crate::primitives::GrammarStore;
use crate::primitives::SymbolID;

use super::compile::AScriptProp;
use super::compile::AScriptStore;
use super::compile::AScriptStruct;
use super::compile::AScriptTypeVal;

const struct_template: &str = "
struct {} {
    {}
}";

pub fn output_rust(grammar: &GrammarStore, ascript: &AScriptStore) -> String
{
    // Build structs
    for (
        struct_id,
        AScriptStruct {
            definition_locations,
            id,
            include_token,
            properties,
            type_name,
        },
    ) in &ascript.struct_table
    {
        let props = properties
            .iter()
            .map(|p| {
                let AScriptProp {
                    first_declared_location,
                    type_val,
                } = ascript.props_table.get(p).unwrap();

                let name = &p.name;
                format!(
                    "{}: {},",
                    name,
                    ascript_type_to_string(type_val, ascript, grammar)
                )
            })
            .collect::<Vec<_>>();

        let token_val = if *include_token {
            "\n    pub tok: Token,".to_string()
        } else {
            "".to_string()
        };

        println!(
            "
/// 
/// origin:
/// ```hcg
/// {} 
/// ```
struct {} {{
    {}{}
}}

impl {} {{
    pub fn new({}) -> Self {{
        Self {{}}
    }}
}}
",
            definition_locations[0].String(),
            type_name,
            props
                .iter()
                .map(|p| format!("pub {}", p))
                .collect::<Vec<_>>()
                .join("\n    "),
            token_val,
            type_name,
            props.join("")
        )
    }

    // Build reduce functions
    for (id, body) in &grammar.bodies_table {
        if !{
            let mut have_result = false;
            for function_id in &body.reduce_fn_ids {
                match grammar.reduce_functions.get(function_id).unwrap() {
                    ReduceFunction::Ascript(function) => match &function.ast {
                        ASTNode::AST_Struct(box ast_struct) => {
                            if let AScriptTypeVal::Struct(struct_type) =
                                get_struct_type_from_node(ast_struct)
                            {
                                let ascript_struct = ascript
                                    .struct_table
                                    .get(&struct_type)
                                    .unwrap();

                                let mut used_props = BTreeSet::<i32>::new();
                                let mut statements = vec![];

                                for prop in &ast_struct.props {
                                    match &prop {
                                        ASTNode::AST_Property(
                                            box AST_Property {
                                                id, value, ..
                                            },
                                        ) => {
                                            let (
                                                reference,
                                                init_string,
                                                type_name,
                                                body_index,
                                            ) = render_expression(
                                                value, body, ascript, grammar,
                                            );

                                            if let Some(body_index) = body_index
                                            {
                                                used_props.insert(body_index);
                                            }

                                            statements.push(format!(
                                                "{}\nlet {} = {}; /* {} */",
                                                init_string,
                                                id,
                                                reference,
                                                body_index.unwrap_or(-1)
                                            ));
                                        }
                                        _ => {}
                                    }
                                }

                                println!(
                                    "fn _fn{:0>3}(args: &mut Vec<HCO>, tok: Token){{",
                                    body.bytecode_id,
                                );

                                let len = body.symbols.len();
                                for (i, _) in
                                    body.symbols.iter().enumerate().rev()
                                {
                                    if used_props.contains(&(i as i32)) {
                                        println!(
                                            "  let i{} = args.pop().unwrap();",
                                            i
                                        )
                                    } else {
                                        println!("  args.pop();")
                                    }
                                }

                                println!(
                                    "  {}",
                                    statements.join("\n").replace("\n", "\n  ")
                                );

                                println!(
                                    "\n  HCO::NODE(ASTNode::{0}({0}::new()))",
                                    ascript_struct.type_name
                                );

                                println!("}}");
                                have_result = true;
                                break;
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            have_result
        }
        // Build a default function that returns the last symbol.
        {}
    }

    String::new()
}

#[test]
fn test_output_rust_on_trivial_grammar()
{
    let grammar = compile_test_grammar(
        "
        <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1) } } 
        | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok } }
    ",
    );

    let mut ascript = AScriptStore::new();

    let errors = compile_reduce_function_expressions(&grammar, &mut ascript);

    for error in &errors {
        println!("{}", error);
    }

    assert!(errors.is_empty());

    assert_eq!(ascript.struct_table.len(), 1);

    output_rust(&grammar, &ascript);
}
/// returns: (expression_ref: String, expression_data: String)
pub fn render_expression(
    ast_expression: &ASTNode,
    body: &Body,
    store: &AScriptStore,
    grammar: &GrammarStore,
) -> (String, String, AScriptTypeVal, Option<i32>)
{
    let (b, s, g) = (body, store, grammar);

    match ast_expression {
        ASTNode::AST_Struct(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_Token(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_Add(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_Vector(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_STRING(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_BOOL(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_U8(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_U16(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_U32(box AST_U32 { initializer, .. }) => {
            match initializer {
                ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => (
                    "r".to_string(),
                    format!("let r:{} = {};", "u32", *value as u64),
                    AScriptTypeVal::U32(Some(*value as u32)),
                    None,
                ),
                _ => {
                    let (r, e, t, i) = render_expression(initializer, b, s, g);

                    match t {
                        AScriptTypeVal::F32(..) => (
                            r.clone(),
                            format!("{1}\nlet {0} = {0} as u32;", r, e),
                            AScriptTypeVal::U32(None),
                            i,
                        ),
                        AScriptTypeVal::Token => (
                            r.clone(),
                            format!("{1}\nlet {0} = {0}.toU32();", r, e),
                            AScriptTypeVal::U32(None),
                            i,
                        ),
                        _ => (r, e, AScriptTypeVal::U32(None), i),
                    }
                }
            }
        }
        ASTNode::AST_U64(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_I8(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_I16(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_I32(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_I64(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_F32(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_F64(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_NUMBER(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_Member(..) => {
            (String::new(), String::new(), AScriptTypeVal::Undefined, None)
        }
        ASTNode::AST_NamedReference(..) => (
            String::new(),
            "NamedReference".to_string(),
            AScriptTypeVal::Undefined,
            None,
        ),
        ASTNode::AST_IndexReference(box AST_IndexReference {
            value, ..
        }) => {
            match body
                .symbols
                .iter()
                .enumerate()
                .filter(|(i, s)| s.original_index == *value as u32)
                .last()
            {
                Some((i, sym)) => match &sym.sym_id {
                    SymbolID::Production(prod_id, ..) => {
                        let types = s.production_types.get(prod_id);
                        println!("{:?}", types);
                        (
                            String::new(),
                            String::new(),
                            AScriptTypeVal::Undefined,
                            Some(i as i32),
                        )
                    }
                    _ => (
                        format!("ref_i{}", sym.original_index),
                        format!(
                            "\nlet ref_i{0} = i{0}.toToken();",
                            sym.original_index
                        ),
                        AScriptTypeVal::Token,
                        Some(i as i32),
                    ),
                },
                None => (
                    String::new(),
                    String::new(),
                    AScriptTypeVal::Undefined,
                    None,
                ),
            }
        }
        _ => (String::new(), String::new(), AScriptTypeVal::Undefined, None),
    }
}

fn ascript_type_to_string(
    ascript_type: &AScriptTypeVal,
    ascript: &AScriptStore,
    grammar: &GrammarStore,
) -> String
{
    match ascript_type {
        AScriptTypeVal::Vector(..) => "Vec<Undefined>".to_string(),
        AScriptTypeVal::Struct(..) => "Struct".to_string(),
        AScriptTypeVal::String(..) => "String".to_string(),
        AScriptTypeVal::Bool(..) => "Bool".to_string(),
        AScriptTypeVal::F64(..) => "f64".to_string(),
        AScriptTypeVal::F32(..) => "f32".to_string(),
        AScriptTypeVal::I64(..) => "i64".to_string(),
        AScriptTypeVal::I32(..) => "i32".to_string(),
        AScriptTypeVal::I16(..) => "i16".to_string(),
        AScriptTypeVal::I8(..) => "i8".to_string(),
        AScriptTypeVal::U64(..) => "u64".to_string(),
        AScriptTypeVal::U32(..) => "u32".to_string(),
        AScriptTypeVal::U16(..) => "u16".to_string(),
        AScriptTypeVal::U8(..) => "U8".to_string(),
        AScriptTypeVal::Undefined => "Undefined".to_string(),
        AScriptTypeVal::Token => "Token".to_string(),
        _ => {
            panic!("Could not resolve compiled ascript type")
        }
    }
}

// Bas is ----------------------------------
// pub fn render_expression(
// ast_expression: &ASTNode,
// body: &Body,
// store: &mut AScriptStore,
// grammar: &GrammarStore,
// ) -> (String, String)
// {
// let mut expression = String::new();
// let mut reference = String::new();
//
// let types = match ast_expression {
// ASTNode::AST_Struct(..) => {}
// ASTNode::AST_Token(..) => {}
// ASTNode::AST_Add(..) => {}
// ASTNode::AST_Vector(..) => {}
// ASTNode::AST_STRING(..) => vec![],
// ASTNode::AST_BOOL(..) => {}
// ASTNode::AST_U8(..) => {}
// ASTNode::AST_U16(..) => {}
// ASTNode::AST_U32(..) => {}
// ASTNode::AST_U64(..) => {}
// ASTNode::AST_I8(..) => {}
// ASTNode::AST_I16(..) => {}
// ASTNode::AST_I32(..) => {}
// ASTNode::AST_I64(..) => {}
// ASTNode::AST_F32(..) => {}
// ASTNode::AST_F64(..) => {}
// ASTNode::AST_NUMBER(..) => {}
// ASTNode::AST_Member(..) => {}
// ASTNode::AST_NamedReference(..) => {}
// ASTNode::AST_IndexReference(..) => {}
// _ => {}
// };
//
// (reference, expression)
// }
