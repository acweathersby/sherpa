use clap::{arg, value_parser, ArgMatches, Command};
use radlr_build::BuildConfig;
use radlr_bytecode::compile_bytecode;
use radlr_core::{JournalReporter, ParserStore, RadlrError, RadlrGrammar, RadlrResult, ParserConfig};
use radlr_rust_build::compile_rust_bytecode_parser;
use std::{fs::File, io::Write, path::PathBuf, default};

#[derive(Clone, Debug)]
enum ParserType {
  LLVM,
  Bytecode,
}

pub fn command() -> ArgMatches {
  Command::new("Radlr")
    .version("1.0.0-beta1")
    
    .author("Anthony Weathersby <acweathersby.codes@gmail.com>")
    .about("A LL, LR, & RAD parser compiler for both deterministic and non-deterministic grammers.")
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
          arg!( --beta "Use the beta toolchain" )
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
    let (parser_type, out_dir, _lib_out_dir) = configure_matches(matches, &pwd);
    let grammar_sources = matches.get_many::<PathBuf>("INPUTS").unwrap_or_default().cloned().collect::<Vec<_>>();
    let name = matches.get_one::<String>("name").cloned();

    let use_beta = matches.get_one::<bool>("beta").cloned().unwrap_or(false);
    let debug = matches.get_one::<bool>("debug").cloned().unwrap_or_default();

    if use_beta {
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
    }else { 
      build_parser(grammar_sources.as_slice(), parser_type, name, _lib_out_dir, out_dir, debug )
    }
  } else if matches.subcommand_matches("disassemble").is_some() {
    RadlrResult::Ok(())
  } else {
    RadlrResult::Err(RadlrError::from("Command Not Recognized"))
  }
}


fn build_parser(
  grammar_sources: &[PathBuf],
  parser_type: ParserType,
  name: Option<String>,
  _lib_out_dir: PathBuf,
  out_dir: PathBuf,
  debug:bool
) -> RadlrResult<()> {

  let config = Default::default();

  //debug_assert_eq!(debug, true);

  let mut grammar = RadlrGrammar::new();

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
        radlr_llvm::llvm_parser_build::build_llvm_parser(&parser, &name, &_lib_out_dir, None, true)?;
        radlr_rust_build::compile_rust_llvm_parser(&parser, &name, &name)
      }
      #[cfg(not(feature = "llvm"))]
      RadlrResult::Err("Compilation not supported".into())
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

  RadlrResult::Ok(())
}

#[test]
fn test_radlr_bytecode_bootstrap() -> RadlrResult<()> {
  let radlr_grammar =
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammar/radlr/2.0.0/grammar.sg").canonicalize().unwrap();

  build_parser(&[radlr_grammar], ParserType::Bytecode, Some("test_radlr".into()), std::env::temp_dir(), std::env::temp_dir(), false)?;

  Ok(())
}
