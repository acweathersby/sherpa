use core::panic;
use std::path::PathBuf;

use clap::{arg, value_parser, ArgMatches, Command};
use sherpa::{
  pipeline::{tasks::*, BuildPipeline, SourceType},
  *,
};

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
          arg!( -o --out <OUTPUT_PATH> "The path to the directory which the parser files will be written to.\n    Defaults to the CWD" )
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
    .get_matches()
}

fn configure_matches(matches: &ArgMatches, pwd: &PathBuf) -> (Config, ParserType, PathBuf) {
  let mut config = Config::default();
  config.source_type = match matches
    .contains_id("lang")
    .then(|| matches.get_one::<String>("lang").map(|s| s.as_str()))
    .flatten()
  {
    Some("ts") | Some("typescript") => panic!("Source type [TypeScript] not yet supported"),
    _ => SourceType::Rust,
  };

  config.enable_ascript = matches.contains_id("ast");

  let parser_type = matches
    .contains_id("type")
    .then(|| matches.get_one::<ParserType>("type").cloned())
    .flatten()
    .unwrap_or(ParserType::Bytecode);

  let out_dir = matches.get_one::<PathBuf>("out").unwrap_or(pwd);

  (config, parser_type, out_dir.clone())
}

fn main() -> SherpaResult<()> {
  let pwd = std::env::current_dir().unwrap();

  let matches = command();

  if let Some(matches) = matches.subcommand_matches("build") {
    let (config, parser_type, out_dir) = configure_matches(matches, &pwd);

    for path in matches.get_many::<PathBuf>("INPUTS").unwrap_or_default() {
      let path =
        if !path.is_absolute() { pwd.join(path).canonicalize()? } else { path.canonicalize()? };

      let mut pipeline = BuildPipeline::from_source(path, config.clone());

      pipeline
        .set_source_output_dir(&out_dir)
        .set_build_output_dir(&out_dir)
        .add_task(build_rust_preamble());

      match config.source_type {
        _ => pipeline.set_source_file_name("%.rs"),
      };

      match parser_type {
        ParserType::LLVM => pipeline
          .add_task(build_llvm_parser(None, true, false))
          .add_task(build_llvm_parser_interface()),
        _ => pipeline.add_task(build_bytecode_parser()),
      };

      if config.enable_ascript {
        pipeline.add_task(build_ascript_types_and_functions(config.source_type));
      }
      match pipeline.run(|errors| {
        for error in &errors {
          eprintln!("{}", error);
        }
      }) {
        SherpaResult::Ok(_) => true,
        _ => false,
      };
    }

    SherpaResult::Ok(())
  } else if let Some(matches) = matches.subcommand_matches("disassemble") {
    let out_dir = matches.get_one::<PathBuf>("out").unwrap_or(&pwd);

    for path in matches.get_many::<PathBuf>("INPUTS").unwrap_or_default() {
      let path =
        if !path.is_absolute() { pwd.join(path).canonicalize()? } else { path.canonicalize()? };

      let mut pipeline = BuildPipeline::from_source(path, Default::default());

      pipeline
        .set_source_output_dir(&out_dir)
        .set_build_output_dir(&out_dir)
        .add_task(build_bytecode_disassembly());

      match pipeline.run(|errors| {
        for error in &errors {
          eprintln!("{}", error);
        }
      }) {
        SherpaResult::Ok(_) => true,
        _ => false,
      };
    }
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Command Not Recognized"))
  }
}
