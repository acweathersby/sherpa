#![feature(scoped_threads)]
#![feature(is_some_with)]
#![feature(const_format_args)]
#![feature(const_fmt_arguments_new)]
use std::path::PathBuf;
use std::str::FromStr;

mod cli;

fn main() {
  let cli = cli::build_cli();

  match cli.get_matches().subcommand() {
    Some(("rust", sub_matches)) => {
      let output_path = get_path_arg(sub_matches, "output").unwrap_or_default();

      let input_path = get_path_arg(sub_matches, "input").unwrap_or_default();

      let build_ast = sub_matches.contains_id("ast");

      if output_path == PathBuf::default() {
        println!("Could not read output directory")
      }

      if input_path == PathBuf::default() || !input_path.extension().is_some_and(|t| *t == "hcg") {
        println!("Could not read input file")
      }

      // if input_path.exists() {
      // compile_bytecode_files(&input_path, &output_path, build_ast);
      // } else {
      // println!("Unable to find path {:?}", input_path);
      // }
    }
    Some(("byte-asm", sub_matches)) => {
            let input_path =
                get_path_arg(sub_matches, "input").unwrap_or_default();

            /* if input_path == PathBuf::default()
                || !input_path.extension().is_some_and(|t| *t == "hcg")
            {
                eprintln!("Unable to read input:{:?}", input_path);
            } else {
                println!("{}", generate_disassembly(&input_path));
            } */
        }
    _ => unreachable!(),
  }
}

fn get_path_arg(sub_matches: &clap::ArgMatches, arg_name: &str) -> std::io::Result<PathBuf> {
  let path =
    PathBuf::from_str(&sub_matches.get_one::<String>(arg_name).cloned().unwrap_or_default())
      .unwrap();
  get_absolute_path(path).canonicalize()
}

fn get_absolute_path(output: PathBuf) -> PathBuf {
  let output = if !output.is_absolute() {
    if let Ok(cwd) = std::env::current_dir() {
      cwd.join(output)
    } else {
      output
    }
  } else {
    output
  };
  output
}
