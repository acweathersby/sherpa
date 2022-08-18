use std::error::Error;
use std::fmt::Display;
use std::io;
use std::path::PathBuf;

use hctk::ascript::compile::compile_reduce_function_expressions;
use hctk::bytecode::compile_bytecode;
use hctk::bytecode::BytecodeOutput;
use hctk::get_num_of_available_threads;
use hctk::grammar::compile_from_string;
use hctk::types::AScriptStore;
use hctk::types::GrammarStore;
use hctk::types::ParseError;
use std::thread;

pub use hctk::grammar::compile_from_path;

#[derive(Debug)]
pub struct CompileError
{
  message: String,
}
impl CompileError
{
  pub fn from_string(error: &str) -> Self
  {
    Self { message: error.to_string() }
  }

  pub fn from_errors(errors: &Vec<ParseError>) -> Self
  {
    let message =
      errors.into_iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n\n");

    Self { message }
  }

  pub fn from_io_error(error: &std::io::Error) -> Self
  {
    Self { message: format!("{}", error) }
  }
}

impl Display for CompileError
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
  {
    f.write_str(&self.message)
  }
}

impl Error for CompileError {}

pub type TaskFn = Box<dyn Fn(&BuildPipeline) -> Result<(), CompileError> + Sync + Send>;

pub struct PipelineTask
{
  pub(crate) fun: TaskFn,
  pub(crate) require_ascript: bool,
  pub(crate) require_bytecode: bool,
}

pub struct BuildPipeline
{
  /// The number of threads that can be used
  /// to split up tasks.
  threads: usize,
  grammar: GrammarStore,
  ascript: Option<AScriptStore>,
  bytecode: Option<BytecodeOutput>,
  source_output_dir: PathBuf,
  build_output_dir: PathBuf,
  tasks: Vec<PipelineTask>,
  parser_name: String,
  grammar_name: String,
  error_handler: Option<fn(errors: Vec<CompileError>)>,
}

impl<'a> BuildPipeline
{
  fn build_pipeline(
    number_of_threads: usize,
    grammar: GrammarStore,
  ) -> Result<Self, CompileError>
  {
    Ok(Self {
      parser_name: (grammar.friendly_name.clone()) + "_parser",
      grammar_name: grammar.friendly_name.clone(),
      threads: number_of_threads,
      grammar,
      tasks: vec![],
      ascript: None,
      bytecode: None,
      error_handler: None,
      source_output_dir: PathBuf::new(),
      build_output_dir: std::env::var("CARGO_MANIFEST_DIR")
        .map_or(std::env::temp_dir(), |d| PathBuf::from(&d)),
    })
  }

  /// Create a new build pipeline after constructing
  /// a grammar store from a source file. Returns Error
  /// if the grammar could not be created, otherwise, a
  /// BuildPipeline is returned.
  pub fn from_source(
    source_path: &PathBuf,
    number_of_threads: usize,
  ) -> Result<Self, CompileError>
  {
    let number_of_threads = if number_of_threads == 0 {
      get_num_of_available_threads()
    } else {
      get_num_of_available_threads().min(number_of_threads)
    };

    let (grammar, errors) = compile_from_path(source_path, number_of_threads);

    if let Some(grammar) = grammar {
      Self::build_pipeline(number_of_threads, grammar)
    } else {
      let message =
        errors.into_iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n\n");

      Err(CompileError { message })
    }
  }

  /// Create a new build pipeline after constructing
  /// a grammar store from a source string. Returns Error
  /// if the grammar could not be created, otherwise, a
  /// BuildPipeline is returned.
  pub fn from_string(
    grammar_source: &str,
    base_directory: &PathBuf,
  ) -> Result<Self, CompileError>
  {
    let (grammar, errors) = compile_from_string(grammar_source, base_directory);

    if let Some(grammar) = grammar {
      Self::build_pipeline(get_num_of_available_threads(), grammar)
    } else {
      let message =
        errors.into_iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n\n");

      Err(CompileError { message })
    }
  }

  /// Set the path for generated source files.
  pub fn set_source_output_dir(&mut self, output_path: &PathBuf) -> &mut Self
  {
    self.source_output_dir = output_path.clone();
    self
  }

  /// Set the path for the build directory.
  pub fn set_build_output_dir(&mut self, output_path: &PathBuf) -> &mut Self
  {
    self.build_output_dir = output_path.clone();
    self
  }

  pub fn add_task(&mut self, task: PipelineTask) -> &mut Self
  {
    if task.require_ascript && self.ascript.is_none() {
      let mut ascript = AScriptStore::new();

      let errors = compile_reduce_function_expressions(&self.grammar, &mut ascript);

      for error in &errors {
        println!("{}", error);
      }

      self.ascript = Some(ascript);
    }

    if task.require_bytecode && self.bytecode.is_none() {
      let bytecode_output = compile_bytecode(&self.grammar, 1);

      self.bytecode = Some(bytecode_output);
    }

    self.tasks.push(task);
    self
  }

  pub fn get_source_output_dir(&self) -> &PathBuf
  {
    &self.source_output_dir
  }

  pub fn get_build_output_dir(&self) -> &PathBuf
  {
    &self.build_output_dir
  }

  pub fn get_grammar(&self) -> &GrammarStore
  {
    &self.grammar
  }

  pub fn get_ascript(&self) -> &AScriptStore
  {
    if self.ascript.is_none() {
      panic!("Failed to construct Ascript data");
    } else {
      self.ascript.as_ref().unwrap()
    }
  }

  pub fn set_parser_name(&mut self, parser_name: String)
  {
    self.parser_name = parser_name;
  }

  pub fn set_grammar_name(&mut self, grammar_name: String)
  {
    self.grammar_name = grammar_name;
  }

  pub fn get_parser_name(&self) -> &String
  {
    &self.parser_name
  }

  pub fn get_grammar_name(&self) -> &String
  {
    &self.grammar_name
  }

  pub fn get_bytecode(&self) -> &BytecodeOutput
  {
    if self.bytecode.is_none() {
      panic!(
        "Failed to construct BytecodeOutput data. 
    Ensure all tasks that access `get_ascript` also `require_ascript`"
      );
    } else {
      self.bytecode.as_ref().unwrap()
    }
  }

  pub fn run<'b>(&'b self) -> &Self
  {
    let errors = thread::scope::<'b>(|scope| {
      let results = self.tasks.iter().map(|t| scope.spawn(move || ((t.fun)(self))));

      let errors = results
        .into_iter()
        .map(|r| r.join().unwrap())
        .map(|r| match r {
          Ok(_) => vec![],
          Err(e) => vec![e],
        })
        .flatten()
        .collect::<Vec<_>>();

      errors
    });

    if !errors.is_empty() {
      if let Some(error_handler) = self.error_handler {
        error_handler(errors);
      }
    }
    self
  }

  pub fn set_error_handler(&mut self, error_handler: fn(Vec<CompileError>)) -> &mut Self
  {
    self.error_handler = Some(error_handler);
    self
  }
}

pub fn TEST_TASK() -> PipelineTask
{
  PipelineTask {
    require_ascript: true,
    require_bytecode: true,
    fun: Box::new(|pipeline: &BuildPipeline| {
      println!("test task");
      Ok(())
    }),
  }
}
