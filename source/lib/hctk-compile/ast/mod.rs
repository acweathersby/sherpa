pub mod rust;
use crate::builder::disclaimer::DISCLAIMER;
use crate::builder::pipeline::PipelineTask;
use crate::CompileError;
use crate::SourceType;
use hctk_core::writer::code_writer::CodeWriter;
use std::io::BufWriter;

/// Constructs a task that compiles a grammar's Ascript into an AST module of the given `source_type`.
/// The module is placed at `<source_output_dir>/<grammar_name>_parser_ast.rs`.
pub fn build_ast(source_type: SourceType) -> PipelineTask {
  PipelineTask {
    fun: Box::new(move |ctx| match source_type {
      SourceType::Rust => {
        if ctx.in_proc_context() {
          let mut writer = CodeWriter::new(vec![]);
          match rust::write(&ctx.get_grammar(), &ctx.get_ascript(), &mut writer) {
            Ok(_) => Ok(Some(unsafe { String::from_utf8_unchecked(writer.into_output()) })),
            Err(err) => Err(CompileError::from_io_error(&err)),
          }
        } else {
          match ctx.create_file(
            ctx.get_source_output_dir().join(format!("./{}_ast.rs", ctx.get_parser_name())),
          ) {
            Ok(ast_data_file) => {
              let mut writer = CodeWriter::new(BufWriter::new(ast_data_file));
              writer.write(&DISCLAIMER("AST Data", "//!", ctx)).unwrap();
              match rust::write(&ctx.get_grammar(), &ctx.get_ascript(), &mut writer) {
                Ok(_) => {
                  drop(writer);
                  Ok(None)
                }
                Err(err) => Err(CompileError::from_io_error(&err)),
              }
            }
            Err(err) => Err(CompileError::from_io_error(&err)),
          }
        }
      }
      _ => Err(CompileError::from_string(&format!(
        "Unable to build an AST output for the source type {:?}",
        source_type
      ))),
    }),
    require_ascript: true,
    require_bytecode: false,
  }
}

#[cfg(test)]
mod test {
  use hctk::ascript::compile::compile_ascript_store;
  use hctk::debug::compile_test_grammar;
  use hctk::types::AScriptStore;

  use hctk::writer::code_writer::CodeWriter;
  use hctk::writer::code_writer::StringBuffer;

  use super::rust;

  #[test]
  fn test_grammar() {
    let grammar = compile_test_grammar(
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
    );
    let mut ascript = AScriptStore::new();

    let errors = compile_ascript_store(&grammar, &mut ascript);
    for error in &errors {
      eprintln!("{}", error);
    }

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &ascript, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types2() {
    let grammar = compile_test_grammar(
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
    );

    let mut store = AScriptStore::new();

    let errors = compile_ascript_store(&grammar, &mut store);

    println!("{:#?}", store);

    for error in &errors {
      eprintln!("{}", error);
    }

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &store, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }

  #[test]
  fn test_parse_errors_when_production_has_differing_return_types3() {
    let grammar = compile_test_grammar(
      " 
      <> B > g:id(+)          
      ",
    );

    let mut store = AScriptStore::new();

    let errors = compile_ascript_store(&grammar, &mut store);

    for error in &errors {
      eprintln!("{}", error);
    }

    eprintln!("{:#?}", store);

    let mut writer = StringBuffer::new(vec![]);

    rust::write(&grammar, &store, &mut writer);

    eprintln!("{}", String::from_utf8(writer.into_output()).unwrap());
  }
}
