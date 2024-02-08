use clap::{arg, value_parser, ArgMatches, Command};
use radlr_build::BuildConfig;
use radlr_bytecode::compile_bytecode;
use radlr_core::{JournalReporter, ParserStore, RadlrError, RadlrGrammar, RadlrResult, ParserConfig};
use std::{fs::File, io::Write, path::PathBuf, default};

#[derive(Clone, Debug)]
enum ParserType {
  LLVM,
  Bytecode,
}

pub fn command() -> ArgMatches {
  Command::new("Radlr")
    .version(env!("CARGO_PKG_VERSION"))
    .author("Anthony Weathersby <acweathersby.codes@gmail.com>")
    .about("A LL, LR, & RAD parser compiler for deterministic and non-deterministic grammers.")
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
        .arg_required_else_help(true)
    )
    .subcommand(
      Command::new("build")
        .about("Constructs a parser from a Radlr grammar.")
        .arg(
          arg!( -t --type <TYPE> "The type of parser Radlr will construct\n" )
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
        arg!( -o --out <OUTPUT_PATH> "The path to the directory which the parser files will be written to.\n  Defaults to the current directory" )
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
          arg!( -g --debug "Outputs debugging files to the OUTPUT_PATH" )
          .required(false)
          .value_parser(value_parser!(bool))
        )
        .arg(
            arg!(<INPUTS>)
                .help("Path(s) to source grammar files")
                .required(true)
                .value_parser(value_parser!(PathBuf))
        )
        .arg_required_else_help(true)
    )
    .arg_required_else_help(true)
    .get_matches()
}

fn configure_matches(matches: &ArgMatches, pwd: &PathBuf) -> (ParserType, PathBuf, PathBuf) {
  let parser_type =
    matches.contains_id("type").then(|| matches.get_one::<ParserType>("type").cloned()).flatten().unwrap_or(ParserType::Bytecode);

  let out_dir = matches.get_one::<PathBuf>("out").unwrap_or(pwd);

  let lib_out_dir = matches.get_one::<PathBuf>("libout").unwrap_or(out_dir);

  (parser_type, out_dir.clone(), lib_out_dir.clone())
}

fn main() -> RadlrResult<()> {
  let pwd = std::env::current_dir().unwrap();

  let matches = command();

  if let Some(matches) = matches.subcommand_matches("build") {
    let (parser_type, out_dir, _lib_out_dir) = configure_matches(matches, &pwd);
    let grammar_sources = matches.get_many::<PathBuf>("INPUTS").unwrap_or_default().cloned().collect::<Vec<_>>();
    let name = matches.get_one::<String>("name").cloned();

    let debug = matches.get_one::<bool>("debug").cloned().unwrap_or_default();
    let target_language = match true {
      _ => radlr_build::TargetLanguage::Rust
      };

    let mut build_config = BuildConfig::new(&grammar_sources.as_slice()[0]);
    
    build_config.include_debug_symbols = debug;
    build_config.build_ast = matches.get_one::<bool>("ast").cloned().unwrap_or_default();
    build_config.lib_out = &_lib_out_dir;
    build_config.source_out = &out_dir;

    build_config.parser_type = match true {
        _ => radlr_build::ParserType::Bytecode
    };
  
    let parser_config = ParserConfig::default();
  
    radlr_build::fs_build(build_config, parser_config, target_language)
  
  } else if matches.subcommand_matches("disassemble").is_some() {
    RadlrResult::Ok(())
  } else {
    RadlrResult::Err(RadlrError::from("Command Not Recognized"))
  }
}


#[test]
fn test_radlr_bytecode_bootstrap() -> RadlrResult<()> {
  let radlr_grammar =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/radlr/2.0.0/grammar.sg").canonicalize().unwrap();


    let mut build_config = BuildConfig::new(&radlr_grammar);
    let temp_dir = std::env::temp_dir().join("radlr");

    build_config.include_debug_symbols = false;
    build_config.build_ast = true;
    build_config.lib_out = &temp_dir;
    build_config.source_out = &temp_dir;

    build_config.parser_type = match true {
        _ => radlr_build::ParserType::Bytecode
    };
    let parser_config = ParserConfig::default();

  radlr_build::fs_build(build_config, parser_config, radlr_build::TargetLanguage::Rust);


  Ok(())
}
