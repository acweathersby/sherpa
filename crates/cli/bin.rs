use clap::{arg, value_parser, ArgMatches, Command};
use radlr_ascript::AscriptDatabase;
use radlr_build::BuildConfig;
use radlr_core::{ParserConfig, RadlrError, RadlrGrammar, RadlrResult};
use radlr_lab::run_lab_server;
use std::{io::BufWriter, path::PathBuf};

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
    .subcommand(Command::new("lab-mode").about("Starts the RADLR lab server, providing local resources to the RADLR lab browser app").arg(
      arg!(-p --port <PORT> "The port the server will liston on. Defaults to 15421")
      .value_parser(value_parser!(u16))
      .required(false)
    ))
    .subcommand(
      Command::new("build-ast").about("Output an ast file from a grammar and an *.atat ast script")  
      .arg(
        arg!( -a --atat <OUTPUT_PATH> "@@ ast script" )
        .required(true)
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
          arg!( --lab "Export the built parser to the RADLR online lab" )
          .required(false)
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
    process_build_command(matches, pwd)
  } else if matches.subcommand_matches("disassemble").is_some() {
    RadlrResult::Ok(())
  } else if let Some(matches) = matches.subcommand_matches("lab-mode") {
    run_lab_server(matches.get_one::<u16>("port").cloned())
  } else if let Some(matches) = matches.subcommand_matches("build-ast") {
    process_atat_script(matches)
  } else {
    RadlrResult::Err(RadlrError::from("Command Not Recognized"))
  }
}

fn process_atat_script(matches: &ArgMatches) -> Result<(), RadlrError> {
  let grammar_sources = matches.get_many::<PathBuf>("INPUTS").unwrap_or_default().cloned().collect::<Vec<_>>();

  let atat_path = matches.get_one::<PathBuf>("atat").cloned().unwrap_or_default();
  let atat_path = atat_path.canonicalize().expect("Could not resolve path to {atat_path}");

  let atat_script = std::fs::read_to_string(atat_path).expect("Failed to read {atat_path} as string");

  let resolved_root_path = RadlrGrammar::resolve_to_grammar_file(&grammar_sources[0])?;
  let db = RadlrGrammar::new().add_source(&resolved_root_path)?.build_db(resolved_root_path, Default::default())?;

  let adb: AscriptDatabase = db.into();


  if let Some(errors) = adb.get_errors() {
    for error in errors {
      eprintln!("{}", error);
      return RadlrResult::Err(RadlrError::StaticText("Could not build AST database"));
    }
  } else {
   //// let parser = OpenOptions::new().append(false).truncate(true).write(true).create(true).open(&ast_path)?;
   
   let buf = BufWriter::new(Vec::new());

    let buf = adb.format(atat_script.as_str(), buf, 100, "ast_struct_name", &[("RUST_NODE_WRAPPER", "std::sync::Arc")])?;

    println!("{}", String::from_utf8(buf.into_inner().expect("Could not read buffer bytes"))?);
  };

  //println!("Hello World {atat_script:?} {db:#?}");

  Ok(())
}

fn process_build_command(matches: &ArgMatches, pwd: PathBuf) -> Result<(), RadlrError> {
  let (_, out_dir, _lib_out_dir) = configure_matches(matches, &pwd);
  let grammar_sources = matches.get_many::<PathBuf>("INPUTS").unwrap_or_default().cloned().collect::<Vec<_>>();
  let name = matches.get_one::<String>("name").cloned();

  let debug = matches.get_one::<bool>("debug").cloned().unwrap_or_default();
  let target_language = match true {
    _ => radlr_build::TargetLanguage::Rust,
  };

  let mut build_config = BuildConfig::new(&grammar_sources.as_slice()[0]);

  build_config.include_debug_symbols = debug;
  build_config.build_ast = matches.get_one::<bool>("ast").cloned().unwrap_or_default();
  build_config.lib_out = &_lib_out_dir;
  build_config.source_out = &out_dir;

  if let Some(name) = &name {
    build_config.name_prefix = Some(name);
  }

  build_config.parser_type = match true {
    _ => radlr_build::ParserType::Bytecode,
  };

  let parser_config = ParserConfig::default();

  radlr_build::fs_build(build_config, parser_config, target_language)
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
    _ => radlr_build::ParserType::Bytecode,
  };
  let parser_config = ParserConfig::default();

  radlr_build::fs_build(build_config, parser_config, radlr_build::TargetLanguage::Rust)
}
