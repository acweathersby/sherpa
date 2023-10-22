use clap::{arg, value_parser, ArgMatches, Command};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::{JournalReporter, ParserStore, SherpaError, SherpaGrammar, SherpaResult};
use sherpa_rust_build::compile_rust_bytecode_parser;
use std::{fs::File, io::Write, path::PathBuf};

#[derive(Clone, Debug)]
enum ParserType {
  LLVM,
  Bytecode,
}

pub fn command() -> ArgMatches {
  Command::new("Sherpa")
    .version("1.0.0-beta1")
    .author("Anthony Weathersby <acweathersby.codes@gmail.com>")
    .subcommand(
        Command::new("disassemble")
        .about("Produce a disassembly file representing the bytecode of a parser for a specific grammar.")
        .arg(
          arg!( -o --out <OUTPUT_PATH> "The path to the directory which the disassembly file(s) will be written to.\n    Defaults to the CWD" )
          .required(false)
          .value_parser(value_parser!(PathBuf))
        )
        .arg(
            arg!(<INPUTS>)
                .help("Path(s) to source grammar files")
                .required(true)
                .value_parser(value_parser!(PathBuf))
        )
    )
    .subcommand(
      Command::new("build")
        .about("Constructs a parser from a Sherpa grammar.")
        .arg(
          arg!( -t --type <TYPE> "The type of parser Sherpa will construct\n" )
          .required(false)
          .value_parser(|value: &str| -> Result<ParserType, &'static str> {match value {
            "llvm" => Ok(ParserType::LLVM),
            _ => Ok(ParserType::Bytecode)
          }})
          .default_value("bytecode")
        ) 
        .arg(
          arg!( -a --ast "Create AST code, in the target language, from AScripT definitions" )
          .required(false)
        )
        .arg(
          arg!( -l --lang <LANGUAGE>)
          .help("The target programming language the parser will be written in.\n")
          .value_parser(["rust", "r", "ts", "typescript"])
          .required(false)
          .default_value("rust")
        )
        .arg(
        arg!( -o --out <OUTPUT_PATH> "The path to the directory which the parser files will be written to.\n  Defaults to the CWD" )
          .required(false)
          .value_parser(value_parser!(PathBuf))
        ).arg(
          arg!( --libout <LIB_OUTPUT_PATH> "The path to the directory which library will be written to.\n  Defaults to the OUTPUT_PATH" )
          .required(false)
          .value_parser(value_parser!(PathBuf))
        ).arg(
          arg!( -n --name <NAME> "Alternate name to use for output files." )
          .required(false)
          .value_parser(value_parser!(String))
        ).arg(
          arg!( -d --debug "Outputs debugging files to the OUTPUT_PATH" )
          .required(false)
          .value_parser(value_parser!(bool))
        )
        .arg(
            arg!(<INPUTS>)
                .help("Path(s) to source grammar files")
                .required(true)
                .value_parser(value_parser!(PathBuf))
        )
    )
    .get_matches()
}

fn configure_matches(matches: &ArgMatches, pwd: &PathBuf) -> (ParserType, PathBuf, PathBuf) {
  let parser_type =
    matches.contains_id("type").then(|| matches.get_one::<ParserType>("type").cloned()).flatten().unwrap_or(ParserType::Bytecode);

  let out_dir = matches.get_one::<PathBuf>("out").unwrap_or(pwd);

  let lib_out_dir = matches.get_one::<PathBuf>("libout").unwrap_or(out_dir);

  (parser_type, out_dir.clone(), lib_out_dir.clone())
}

fn main() -> SherpaResult<()> {
  let pwd = std::env::current_dir().unwrap();

  let matches = command();

  if let Some(matches) = matches.subcommand_matches("build") {
    let (parser_type, out_dir, _lib_out_dir) = configure_matches(matches, &pwd);
    let grammar_sources = matches.get_many::<PathBuf>("INPUTS").unwrap_or_default().cloned().collect::<Vec<_>>();
    let name = matches.get_one::<String>("name").cloned();

    build_parser(grammar_sources.as_slice(), parser_type, name, _lib_out_dir, out_dir, matches)
  } else if matches.subcommand_matches("disassemble").is_some() {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Command Not Recognized"))
  }
}

fn build_parser(
  grammar_sources: &[PathBuf],
  parser_type: ParserType,
  name: Option<String>,
  _lib_out_dir: PathBuf,
  out_dir: PathBuf,
  matches: &ArgMatches
) -> SherpaResult<()> {
  let debug = matches.get_one::<bool>("debug").cloned().unwrap_or_default();

  let config = Default::default();

  debug_assert_eq!(debug, true);

  let mut grammar = SherpaGrammar::new();

  for path in grammar_sources {
    grammar.add_source(path)?;
  }

  if grammar.dump_errors() {
    panic!("Failed To parse due to the above errors")
  }

  let db = grammar.build_db(grammar_sources.first().unwrap(), config)?;

  if db.dump_errors() {
    panic!("Failed To parse due to the above errors")
  }

  let states = db.build_states(config)?;
  
  if debug {
    states.write_debug_file(&out_dir)?;
  }

  let parser = states.build_ir_parser(true, false)?;

  if debug {
    //parser.write_debug_file(&out_dir)?;
  }

  eprint!("Created a {} parser", parser.get_classification().to_string());


  if parser.dump_errors() {
    panic!("Failed To parse due to the above errors")
  }

  eprint!("{}", parser.report.to_string());

  let output = match parser_type {
    ParserType::LLVM => {
      #[cfg(feature = "llvm")]
      {
        sherpa_llvm::llvm_parser_build::build_llvm_parser(&parser, &name, &_lib_out_dir, None, true)?;
        sherpa_rust_build::compile_rust_llvm_parser(&parser, &name, &name)
      }
      #[cfg(not(feature = "llvm"))]
      SherpaResult::Err("Compilation not supported".into())
    }
    _ => {
      let pkg = compile_bytecode(&parser, false)?;
      compile_rust_bytecode_parser(&parser, &pkg)
    }
  };

  let output = output?;

  //Ensure out directory exists
  std::fs::create_dir_all(&out_dir)?;

  //Write to file
  let out_filepath = out_dir.join(name.unwrap_or(db.get_internal().friendly_name_string()) + ".rs");

  let mut file = File::create(out_filepath)?;

  file.write_all(output.as_bytes())?;

  SherpaResult::Ok(())
}

#[test]
fn test_sherpa_bytecode_bootstrap() -> SherpaResult<()> {
  let sherpa_grammar =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/sherpa/2.0.0/grammar.sg").canonicalize().unwrap();

  //build_parser(&[sherpa_grammar], ParserType::Bytecode, Some("test_sherpa".into()), std::env::temp_dir(), std::env::temp_dir())

  Ok(())
}
