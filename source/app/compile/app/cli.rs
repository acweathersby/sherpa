use clap::{arg, Command};

pub fn build_cli() -> Command {
  Command::new("sherpa-compile")
    .about("A compiler for Sherpa Grammar files (.sg)")
    .subcommand_required(true)
    .arg_required_else_help(true)
    .allow_external_subcommands(false)
    .after_help("Some help information")
    .subcommand(
      Command::new("rust")
        .about("Compile a rust output")
        .arg(
          arg!(
              -a --ast "Produce AScripT output alongside parser data."
          )
          .required(false),
        )
        .arg(
          arg!(
              -o --output <OUTPUT> "The output directory for compiled artifacts.
              Defaults to the working directory."
          )
          .value_hint(clap::ValueHint::DirPath)
          .required(false),
        )
        .arg(
          arg!(
              -i --input <INPUT> "A path to a grammar file (.sg)"
          )
          .value_hint(clap::ValueHint::FilePath)
          .required(true),
        ),
    )
    .subcommand(
      Command::new("byte-asm").about("Output the bytecode disassembly for the grammar.").arg(
        arg!(
            -i --input <INPUT> "A path to a grammar file (.sg)"
        )
        .value_hint(clap::ValueHint::FilePath)
        .required(true),
      ),
    )
}
