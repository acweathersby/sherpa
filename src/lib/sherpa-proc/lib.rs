#![feature(proc_macro_span)]
#![feature(proc_macro_internals)]
#![feature(proc_macro_diagnostic)]
#![feature(proc_macro_span_shrink)]
use proc_macro::{Span, TokenStream, TokenTree};
use std::{collections::BTreeMap, path::PathBuf};

use sherpa_core::{
  pipeline::{
    tasks::{self},
    BuildPipeline,
    SourceType,
  },
  SherpaError,
  SherpaResult,
};

extern crate proc_macro;

#[derive(Debug)]
enum ParserType {
  _LLVM,
  BYTECODE,
}

fn process_errors(errors: Vec<SherpaError>, _offsets: Offsets) {
  //use SherpaError::*;

  for err in errors {
    match err {
      /*      grammar_err { message, loc, .. } => {
        let start = loc.get_start();
        let end = loc.get_end();
        match (offsets.spans.get(&start), offsets.spans.get(&end)) {
          (Some(start_span), Some(end_span)) => {
            let span = start_span.join(*end_span).unwrap();
            Diagnostic::spanned(span, proc_macro::Level::Error, message)
              .note(format!(
                "Create the definition \"<> {} > ...\" in the grammar",
                loc.to_string()
              ))
              .emit();
          }
          _ => panic!("no dice"),
        }
      } */
      _ => panic!("{}", err),
    }
  }
}
#[derive(Debug)]
struct Offsets {
  line:  usize,
  col:   usize,
  pos:   usize,
  spans: BTreeMap<usize, Span>,
}

fn parse_token_stream(
  stream: TokenStream,
  offset: &mut Offsets,
  output: &mut Vec<String>,
) -> Option<(String, PathBuf)> {
  let mut len = 0;
  let mut path = None;
  for tree in stream {
    len += 1;
    path = None;
    match tree {
      TokenTree::Group(g) => {
        insert_data(&g.span_open(), g.span_open().source_text().unwrap(), offset, output);
        parse_token_stream(g.stream(), offset, output);
        insert_data(&g.span_close(), g.span_close().source_text().unwrap(), offset, output);
      }
      TokenTree::Ident(i) => {
        insert_data(&i.span(), i.to_string(), offset, output);
      }
      TokenTree::Literal(l) => {
        let string = l.span().source_text().unwrap();
        path = Some((string[1..string.len() - 1].to_string(), l.span().source_file().path()));
        insert_data(&l.span(), l.to_string(), offset, output);
      }
      TokenTree::Punct(p) => {
        insert_data(&p.span(), p.to_string(), offset, output);
      }
    }
  }

  if len > 1 {
    None
  } else {
    path
  }
}

fn insert_data(span: &Span, in_string: String, offset: &mut Offsets, output: &mut Vec<String>) {
  let start = span.start();
  let end = span.end();
  let line_diff = if start.line > offset.line {
    offset.col = 0;
    start.line - offset.line
  } else {
    0
  };
  let col_diff = start.column - offset.col;
  let diff = line_diff + col_diff;
  offset.col = end.column;
  offset.line = end.line;
  let string = "\n".repeat(line_diff) + &" ".repeat(col_diff) + &in_string;
  offset.spans.insert(offset.pos + diff, span.before());
  offset.spans.insert(offset.pos + string.len(), span.after());
  offset.pos += string.len();
  output.push(string);
}

/// Compile a Sherpa parser
///
/// # Examples
///
/// Using inline grammar:
/// ```
/// mod my_parser {
/// # use sherpa_proc::compile_mod as compile;
///   compile! {
///     IGNORE { c:sp }
///
///     <> helloworld > "hello" "world"
///           
///           :ast { t_Hello_World, val:str(tok) }
///   }
/// }
///
/// let result = my_parser::ast::default_from("hello world".into());
///
/// eprintln!("{:?}", result);
/// ```
/// This prints to stderr:
/// ```shell
/// [test/e2e/bytecode_parser/./lib.rs:12] result = Ok(
///   Hello_World {
///     val: "hello",
///   },
/// )
/// ```
///
/// Using a grammar file. File paths are relative to file the
/// macro is used in.
///
/// ```
/// mod my_parser {
/// # use sherpa_proc::compile_mod as compile;
///   compile!("test_grammar.sg");
/// }
///
/// let result = my_parser::ast::default_from("hello world".into());
///
/// eprintln!("{:?}", result);
/// ```
#[proc_macro]
pub fn compile_mod(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  // Convert input into a lookup table
  let mut offsets = Offsets { col: 0, line: 0, pos: 0, spans: Default::default() };
  let mut output = vec![];
  let root_dir = std::env::current_dir().map(|d| PathBuf::from(&d)).unwrap();

  let mut pipeline = if let Some((grammar_source_path, source_file_path)) =
    parse_token_stream(input, &mut offsets, &mut output)
  {
    BuildPipeline::from_source(
      root_dir.join(source_file_path.parent().unwrap()).join(grammar_source_path),
      Default::default(),
    )
  } else {
    BuildPipeline::from_string(&output.join(""), &root_dir, Default::default())
  }
  .make_proc();

  pipeline.add_task(tasks::build_rust_preamble());

  match (ParserType::BYTECODE, std::env::var("OUT_DIR").map(|d| PathBuf::from(&d))) {
    (ParserType::_LLVM, Ok(out_dir)) => pipeline
      .set_build_output_dir(&out_dir)
      .add_task(tasks::build_llvm_parser(None, true, false))
      .add_task(tasks::build_llvm_parser_interface()),
    _ => pipeline.add_task(tasks::build_bytecode_parser()),
  };

  if true {
    pipeline.add_task(tasks::build_ascript_types_and_functions(SourceType::Rust));
  }

  match pipeline.run(|errors| {
    process_errors(errors, offsets);
  }) {
    SherpaResult::Ok((_, artifacts, _)) => {
      let result = artifacts.into_iter().map(|(_, s)| s).collect::<Vec<_>>().join("\n");
      result.parse().unwrap()
    }
    SherpaResult::Err(err) => {
      panic!("{}", err);
    }
    _ => Default::default(),
  }
}
