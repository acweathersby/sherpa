use clap::{arg, value_parser, ArgMatches, Command};
use sherpa_core::{
  build_compile_db,
  compile_grammars_from_path,
  new_taskman,
  Config,
  GrammarSoup,
  Journal,
  SherpaError,
  SherpaResult,
};
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

fn configure_matches(
  matches: &ArgMatches,
  pwd: &PathBuf,
) -> (Config, ParserType, PathBuf, PathBuf) {
  let mut config = Config::default();
  config.enable_ascript = matches.contains_id("ast");

  let parser_type = matches
    .contains_id("type")
    .then(|| matches.get_one::<ParserType>("type").cloned())
    .flatten()
    .unwrap_or(ParserType::Bytecode);

  let out_dir = matches.get_one::<PathBuf>("out").unwrap_or(pwd);

  let lib_out_dir = matches.get_one::<PathBuf>("libout").unwrap_or(out_dir);

  (config, parser_type, out_dir.clone(), lib_out_dir.clone())
}

fn main() -> SherpaResult<()> {
  let pwd = std::env::current_dir().unwrap();

  let matches = command();

  if let Some(matches) = matches.subcommand_matches("build") {
    let (config, parser_type, out_dir, lib_out_dir) = configure_matches(matches, &pwd);

    let (executor, spawner) = new_taskman(1000);

    let output =
      matches.get_many::<PathBuf>("INPUTS").unwrap_or_default().cloned().collect::<Vec<_>>();

    let sp = spawner.clone();

    let name = matches.get_one::<String>("name").cloned();

    sp.clone().spawn(async move {
      let mut j = Journal::new(Some(config));
      let soup = GrammarSoup::new();
      let mut id = None;

      for path in output {
        match compile_grammars_from_path(
          j.transfer(),
          path.clone(),
          soup.as_ref(),
          &spawner.clone(),
        )
        .await
        {
          SherpaResult::Ok(id_) => {
            id = Some(id_);
          }
          SherpaResult::Err(err) => {
            println!("{err}");
          }
          SherpaResult::None => {}
        }
      }

      j.flush_reports();
      if j.debug_error_report() {
        panic!("Failed To parse due to the above errors")
      }

      let id: sherpa_core::GrammarIdentities = id?;

      let db = build_compile_db(j.transfer(), id, &soup);

      j.flush_reports();
      if j.debug_error_report() {
        panic!("Failed To parse due to the above errors")
      }

      let db = db.unwrap();

      let output = compile_rust_bytecode_parser(&mut j, &db).await;

      j.flush_reports();
      if j.debug_error_report() {
        panic!("Failed To parse due to the above errors")
      }

      let output = output?;

      //Write to file
      let out_filepath = out_dir.join(
        name.or(Some(id.name.to_string(db.string_store()))).unwrap_or("parser".to_string()) + ".rs",
      );

      let mut file = File::create(out_filepath)?;

      file.write_all(output.as_bytes())?;

      SherpaResult::Ok(())
    });

    drop(sp);

    executor.join();

    SherpaResult::Ok(())
  } else if let Some(matches) = matches.subcommand_matches("disassemble") {
    SherpaResult::Ok(())
  } else {
    SherpaResult::Err(SherpaError::from("Command Not Recognized"))
  }
}
