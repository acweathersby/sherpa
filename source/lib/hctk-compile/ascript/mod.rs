pub mod compile;
pub mod errors;
pub mod rust;
pub mod types;

use crate::{builder::pipeline::PipelineTask, SourceType};
use hctk_core::{types::HCError, writer::code_writer::CodeWriter};

/// Constructs a task that compiles a grammar's Ascript into an AST module of the given `source_type`.
/// The module is placed at `<source_output_dir>/<grammar_name>_parser_ast.rs`.
pub fn build_ast(source_type: SourceType) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |ctx| match (source_type, ctx.get_ascript()) {
      (SourceType::Rust, Some(ascript)) => {
        let mut writer = CodeWriter::new(vec![]);
        match rust::write(ascript, &mut writer) {
          Ok(_) => Ok(Some(unsafe { String::from_utf8_unchecked(writer.into_output()) })),
          Err(err) => Err(vec![HCError::from(err)]),
        }
      }
      _ => Ok(Some(String::default())),
    }),
    require_ascript: true,
    require_bytecode: false,
  }
}

#[cfg(test)]
mod rust_ast_build {
  use std::path::PathBuf;

  use crate::ascript::types::AScriptStore;
  use hctk_core::{
    types::{GrammarStore, HCResult},
    writer::code_writer::StringBuffer,
  };

  use super::rust;

  #[test]
  fn test_temp() {
    use std::{io::Write, path::PathBuf, vec};

    let g = GrammarStore::from_path(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../test/e2e/bootstrap/grammar/ir.hcg")
        .canonicalize()
        .unwrap(),
    )
    .unwrap();
    let mut ascript = AScriptStore::new(g).unwrap();

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer).unwrap();

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_grammar_imported_grammar() {
    let g = GrammarStore::from_path(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../test/grammars/script_base.hcg")
        .canonicalize()
        .unwrap(),
    )
    .unwrap();
    let mut ascript = AScriptStore::new(g).unwrap();

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer).unwrap();

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_grammar() {
    let g = GrammarStore::from_str(
      "
@IGNORE g:sp

@EXPORT statement as entry

@NAME llvm_language_test

<> statement > expression       f:ast { { t_Stmt, v:$1 } }

<> expression > sum             

<> sum > mul \\+ sum             f:ast { { t_Sum, l:$1, r:$3 } }
    | mul

<> mul > term \\* expression     f:ast { { t_Mul, l:$1, r:$3 } }
    | term

<> term > \\2                f:ast { { t_Num, v: u16($1) } }

    | \\( expression \\)          f:ast { { t_Paren, v: $2 } }
",
    )
    .unwrap();
    let mut ascript = AScriptStore::new(g).unwrap();

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types2() {
    let g = GrammarStore::from_str(
      "
        @EXPORT markdown as md

        <> markdown > lines
        
            f:ast { {t_Markdown, lines:$1 } }
        
        <> lines > g:nl? line
        
            f:ast { [$2] }
            
            | lines g:nl line
            
            f:ast { [$1, $3] }
        
            | lines ( g:nl f:ast{ { t_EmptyLine, c_Line } } )
        
            f:ast {  [$1, $2] }
        
        <> line >
        
            header_token content
        
            f:ast { { t_Header, c_Line, length:f64($1), content:$2 } }
        
            | 
            
            tk:spaces? tk:ol_token content
        
            f:ast { { t_OL, c_Line, spaces:str($1), content:$3 } }
        
            |
        
            tk:spaces? tk:ul_token content
        
            f:ast { { t_UL, c_Line, spaces:str($1), content:$3 } }
        
            |
        
            tk:spaces? tk:quote_token content
        
            f:ast { { t_Quote, c_Line, spaces:str($1), content:$3 } }
        
            | 
        
            tk:spaces? content
        
            f:ast { { t_Paragraph, c_Line, spaces:str($1), content:$2 } }
        
            |
             
            tk:code_block_delimiter code_line_text? code_line(*) cb_sentinel
        
            f:ast { { t_CodeBlock, c_Line, syntax:str($2), data:$3 } }
        
        <> ol_token > g:num \\. 
        
        <> spaces > g:sp(+\\\" )
        
        <> header_token > \\% (+)
        
        <> ul_token > \\- 
            | \\+ 
        
        <> quote_token > \\>           
        
        <> code_line >
        
            g:nl code_line_text?
        
            f:ast { { t_Text, c_Content, value: str($2) } }
        
        <> code_block_delimiter > \\```
        
        <> code_block_delimiter_with_nl > g:nl \\```
        
        <> cb_sentinel > tk:code_block_delimiter_with_nl
        
        <[ recover cb_sentinel_1 ] 
        
            shift nothing then set prod to cb_sentinel
        >
        
        <> code_line_text > 
            (   g:num 
            |   g:sp
            |   g:id 
            |   g:sym
            )(+\\\" )
        
        <> code_block_sentinel >
        
            g:nl \\``` 
        
        <> content > ( text | format_symbol )(+) f:ast{ [$1] }
        
        <> text > text_symbol(+\\\" )
            f:ast { { t_Text, c_Content, value: str($1) } }
        
        <> text_symbol > 
                g:sym
            |   g:sp
            |   tk:word
            |   tk:num
        
        <> word > g:id 
            | word g:id
        
        <> num > g:num
            | num g:num
        
        <> format_symbol > 
            \\` 
            f:ast { { t_InlineCode, c_Content } }
            | \\* 
            f:ast { { t_MarkerA, c_Content } }
            | \\_ 
            f:ast { { t_MarkerB, c_Content } }
            | \\{
            f:ast { { t_QueryStart, c_Content } }
            | \\}
            f:ast { { t_QueryEnd, c_Content } }
            | \\[ 
            f:ast { { t_AnchorStart, c_Content } }
            | \\![
            f:ast { { t_AnchorImageStart, c_Content } }
            | \\]
            f:ast { { t_AnchorEnd, c_Content } }
            | \\](
            f:ast { { t_AnchorMiddle, c_Content } }
            | \\)
            f:ast { { t_AnchorEnd, c_Content, c_Meta } }
            | \\(
            f:ast { { t_MetaStart, c_Content, c_Meta } }              
        ",
    )
    .unwrap();

    let mut ascript = AScriptStore::new(g).unwrap();

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn handles_multipart_arrays() -> HCResult<()> {
    use HCResult::*;
    let g = GrammarStore::from_str(
      "     
      <> A > B(+) | C 

      <> B > \\tok

      <> C > D(+ t:t ) 
             ( t:x t:y t:z )?
             ( t:x t:y t:z f:ast { tok } )?

              f:ast { [ $1, $2, $3 ] }

        | ( t:ggg t:rrr )

              f:ast{  [$1] }

      <> D > \\xxx
  ",
    )
    .unwrap();

    let ascript = AScriptStore::new(g)?;

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer)?;

    Ok(())
  }

  #[test]
  fn rust_vector_return_types_print_correctly() -> HCResult<()> {
    use HCResult::*;
    let g = GrammarStore::from_str(
      " 
        <> A > B f:ast { { t_A, r:$1 } }

        <> B > \\z ? ( \\d  )(*)  f:ast { [$1, $2] }
        ",
    )
    .unwrap();

    let ascript = AScriptStore::new(g)?;

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer)?;

    println!("{}", String::from_utf8(writer.into_output())?);

    Ok(())
  }

  #[test]
  fn group_productions_get_correct_type_information() {
    let g = GrammarStore::from_str(
      "
      @NAME hc_symbol

      @IGNORE g:sp g:nl
      
      
      <> annotated_symbol > 
              
              symbol^s [unordered tk:reference?^r \\? ?^o ]
      
                  f:ast {{ t_AnnotatedSymbol, symbol:$s, is_optional:bool($o), reference:str($r), tok  }}
              
              | symbol
      
      
      <> symbol > class
      
      
      <> class >
      
              t:c: ( \\num | \\nl | \\sp | \\id | \\sym | \\any )
              
                  f:ast { { t_Class, c_Symbol , c_Terminal, val:str($2),  tok } }
      
      
      <> reference > 
      
              t:^ tk:identifier_syms
      
      
      <> identifier > 
      
              tk:identifier_syms 
      
      
      <> identifier_syms >  
      
              identifier_syms g:id
      
              | identifier_syms \\_
      
              | identifier_syms \\-
      
              | identifier_syms g:num      
      
              | \\_ 
      
              | \\- 
      
              | g:id
        ",
    ).unwrap();

    let mut ascript = AScriptStore::new(g).unwrap();
    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer);
  }

  // pri

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types3() {
    let g = GrammarStore::from_str(
      " 
      <> B > g:id(+)          
      ",
    )
    .unwrap();

    let mut ascript = AScriptStore::new(g).unwrap();

    eprintln!("{:#?}", ascript);

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}

#[cfg(test)]
mod ascript_compile_tests {

  use crate::{
    ascript::{
      compile::{compile_ascript_store, compile_struct_props, compile_struct_type},
      types::AScriptStore,
    },
    compile::verify_property_presence,
  };
  use hctk_core::{
    grammar::{
      data::ast::{ASTNode, AST_Property, AST_Struct, AST_TypeId, Ascript, Body, Production},
      parse::{compile_ascript_ast, compile_grammar_ast},
    },
    types::*,
  };

  #[test]
  fn test_parse_errors_when_struct_type_is_missing() {
    let ast = compile_ascript_ast(" { c_Test }".as_bytes().to_vec());

    assert!(ast.is_ok());

    if let ASTNode::AST_Struct(ast_struct) = ast.unwrap() {
      let (_, errors) = compile_struct_type(
        &mut AScriptStore::default(),
        &ast_struct,
        &create_dummy_body(BodyId(0)),
      );

      errors.debug_print();

      assert_eq!(errors.len(), 1);
    } else {
      panic!("Value is not a struct");
    }
  }

  fn create_dummy_body(id: BodyId) -> hctk_core::types::Body {
    hctk_core::types::Body { id, ..Default::default() }
  }

  #[test]
  fn test_parse_errors_when_struct_type_is_redefined() {
    let ast = compile_ascript_ast(" { t_TestA, t_TestB, t_TestC }".as_bytes().to_vec());

    assert!(ast.is_ok());

    if let ASTNode::AST_Struct(ast_struct) = ast.unwrap() {
      let (_, errors) = compile_struct_type(
        &mut AScriptStore::default(),
        &ast_struct,
        &create_dummy_body(BodyId(0)),
      );

      errors.debug_print();

      assert_eq!(errors.len(), 1);
    } else {
      panic!("Value is not a struct");
    }
  }

  #[test]
  fn test_parse_errors_when_struct_prop_type_is_redefined() {
    let astA = compile_ascript_ast(" { t_TestA, apple: u32 }".as_bytes().to_vec());
    assert!(astA.is_ok());
    let astB = compile_ascript_ast(" { t_TestA, apple: i64 }".as_bytes().to_vec());
    assert!(astB.is_ok());

    let mut ast = AScriptStore::default();

    let body = create_dummy_body(BodyId(0));
    if let ASTNode::AST_Struct(ast_struct) = astA.unwrap() {
      let (id, mut errors) = compile_struct_type(&mut ast, &ast_struct, &body);
      let (_, mut e) = compile_struct_props(&mut ast, &id, &ast_struct, &body);
      errors.append(&mut e);
      errors.debug_print();

      assert!(!errors.have_errors());

      if let ASTNode::AST_Struct(ast_struct) = astB.unwrap() {
        let (id, mut errors) = compile_struct_type(&mut ast, &ast_struct, &body);
        let (_, mut e) = compile_struct_props(&mut ast, &id, &ast_struct, &body);
        errors.append(&mut e);
        errors.debug_print();

        assert_eq!(errors.len(), 1);
      } else {
        panic!("Value is not a struct");
      }
    } else {
      panic!("Value is not a struct");
    }
  }

  #[test]
  fn test_prop_is_made_optional_when_not_present_or_introduced_in_subsequent_definitions() {
    let mut ast = AScriptStore::default();

    for (i, struct_) in [
      " { t_TestA, apple: u32, beetle:bool }",
      " { t_TestA, beetle:bool }",
      " { t_TestB }",
      " { t_TestB, apple: u32 }",
    ]
    .iter()
    .map(|input| compile_ascript_ast(input.as_bytes().to_vec()))
    .enumerate()
    {
      assert!(struct_.is_ok());

      if let ASTNode::AST_Struct(struct_) = struct_.unwrap() {
        let body = create_dummy_body(BodyId(i as u64));
        let (id, errors) = compile_struct_type(&mut ast, &struct_, &body);

        errors.debug_print();

        assert!(errors.is_empty());

        let errors = compile_struct_props(&mut ast, &id, &struct_, &body).1;

        errors.debug_print();

        assert!(errors.is_empty());
      }
    }

    for struct_id in ast.structs.keys().cloned().collect::<Vec<_>>() {
      verify_property_presence(&mut ast, &struct_id);
    }

    for prop in &ast.props {
      if prop.0.name == "beetle" {
        assert!(
          !prop.1.optional,
          "Expected {}~{} to not be optional",
          ast.structs.get(&prop.0.struct_id).unwrap().type_name,
          prop.0.name
        );
      } else {
        assert!(
          prop.1.optional,
          "Expected {}~{} to be optional",
          ast.structs.get(&prop.0.struct_id).unwrap().type_name,
          prop.0.name
        );
      }
    }
  }

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types() {
    let g = GrammarStore::from_str(
      "
            <> A > \\1 f:ast { { t_Test } } 
            | \\a 
        ",
    )
    .unwrap();

    match AScriptStore::new(g) {
      HCResult::MultipleErrors(errors) => {
        errors.debug_print();
        assert_eq!(errors.len(), 1);
      }
      _ => unreachable!("This should have generated an error"),
    }
  }

  #[test]
  fn test_ASTs_are_defined_for_ascript_return_functions() {
    let grammar = "<> A > \\1 f:ast { { t_Test, val: str($1) } } ".to_string();

    let grammar_ast = compile_grammar_ast(grammar.as_bytes().to_vec());

    match grammar_ast {
      Ok(grammar_ast) => {
        let content = &grammar_ast.content;

        match &content[0] {
          ASTNode::Production(box Production { bodies, .. }) => {
            if let ASTNode::Body(box Body { reduce_function, .. }) = &bodies[0] {
              if let ASTNode::Ascript(box Ascript { ast, .. }) = reduce_function {
                if let ASTNode::AST_Struct(box AST_Struct { props, .. }) = ast {
                  assert_eq!(props.len(), 2);
                  if let ASTNode::AST_TypeId(box AST_TypeId { value, .. }) = &props[0] {
                    assert_eq!(value, "t_Test")
                  } else {
                    panic!("Incorrect type name");
                  }

                  if let ASTNode::AST_Property(box AST_Property { id, value, .. }) = &props[1] {
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
        eprintln!("error\n{}", err);

        // panic!("Failed to compile grammar ast")
      }
    }
  }
}
