#![feature(proc_macro_span)]
#![feature(proc_macro_internals)]
#![feature(proc_macro_diagnostic)]
#![feature(proc_macro_span_shrink)]
use proc_macro::{Diagnostic, Span, TokenStream, TokenTree};
use std::{collections::BTreeMap, path::PathBuf};

use sherpa_core::{
  pipeline::{tasks, BuildPipeline, SourceType},
  rt::{SherpaError, SherpaResult},
};

extern crate proc_macro;

#[derive(Debug)]
enum ParserType {
  LLVM,
  BYTECODE,
}

#[derive(Debug)]
struct GrammarInput {
  pub parser_type: ParserType,
  pub grammar:     String,
  pub span_lookup: BTreeMap<usize, proc_macro::Span>,
}

fn process_errors(errors: Vec<SherpaError>, offsets: Offsets) {
  use SherpaError::*;

  for err in errors {
    match err {
      grammar_err { message, inline_message, loc, path } => {
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
      }
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

fn parse_token_stream(stream: TokenStream, offset: &mut Offsets, output: &mut Vec<String>) {
  for tree in stream {
    match tree {
      TokenTree::Group(g) => {
        insert_data(&g.span_open(), "{".to_string(), offset, output);
        parse_token_stream(g.stream(), offset, output);
        insert_data(&g.span_close(), "}".to_string(), offset, output);
      }
      TokenTree::Ident(i) => {
        insert_data(&i.span(), i.to_string(), offset, output);
      }
      TokenTree::Literal(l) => {
        insert_data(&l.span(), l.to_string(), offset, output);
      }
      TokenTree::Punct(p) => {
        insert_data(&p.span(), p.to_string(), offset, output);
      }
    }
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
  let col_diff = (start.column - offset.col);
  let diff = line_diff + col_diff;
  offset.col = end.column;
  offset.line = end.line;
  let string = "\n".repeat(line_diff) + &" ".repeat(col_diff) + &in_string;
  offset.spans.insert(offset.pos + diff, span.before());
  offset.spans.insert(offset.pos + string.len(), span.after());
  offset.pos += string.len();
  output.push(string);
}

#[proc_macro]
pub fn compile_mod(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  // Convert input into a lookup table
  let mut offsets = Offsets { col: 0, line: 0, pos: 0, spans: Default::default() };
  let mut output = vec![];

  parse_token_stream(input, &mut offsets, &mut output);

  let grammar = output.join("");

  println!("{}", grammar);

  let root_dir = std::env::var("CARGO_MANIFEST_DIR").map(|d| PathBuf::from(&d)).unwrap();

  let mut pipeline = BuildPipeline::proc_context(&grammar, &root_dir, Default::default());

  pipeline.set_source_output_dir(&root_dir);

  match (ParserType::BYTECODE, std::env::var("OUT_DIR").map(|d| PathBuf::from(&d))) {
    (ParserType::LLVM, Ok(out_dir)) => pipeline
      .set_build_output_dir(&out_dir)
      .add_task(tasks::build_llvm_parser(None, true, false))
      .add_task(tasks::build_llvm_parser_interface()),
    _ => pipeline.add_task(tasks::build_ascript_types_and_functions(SourceType::Rust)),
  };

  //   pipeline = if input.build_disassembly {
  // pipeline.add_task(tasks::build_bytecode_disassembly())
  // } else {
  // pipeline
  // };

  match pipeline
    .add_task(tasks::build_ascript_types_and_functions(sherpa_core::pipeline::SourceType::Rust))
    .run(|errors| {
      process_errors(errors, offsets);
    }) {
    SherpaResult::Ok((_, artifacts, _)) => artifacts.join("\n").parse().unwrap(),
    _ => Default::default(),
  }
}
