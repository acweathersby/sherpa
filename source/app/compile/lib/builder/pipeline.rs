use std::error::Error;
use std::fmt::Display;
use std::fs::create_dir_all;
use std::fs::File;
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
pub struct CompileError {
  message: String,
}
impl CompileError {
  pub fn from_parse_error(error: &ParseError) -> Self {
    Self { message: error.to_string() }
  }

  pub fn from_string(error: &str) -> Self {
    Self { message: error.to_string() }
  }

  pub fn from_errors(errors: &Vec<ParseError>) -> Self {
    let message = errors.into_iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n\n");

    Self { message }
  }

  pub fn from_io_error(error: &std::io::Error) -> Self {
    Self { message: format!("{}", error) }
  }
}

impl Display for CompileError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self.message)
  }
}

impl Error for CompileError {}

pub type TaskFn = Box<dyn Fn(&mut PipelineContext) -> Result<(), CompileError> + Sync + Send>;

pub struct PipelineTask {
  pub(crate) fun: TaskFn,
  pub(crate) require_ascript: bool,
  pub(crate) require_bytecode: bool,
}

#[derive(Debug, Clone)]
pub enum CachedSource {
  Path(PathBuf),
  String(String, PathBuf),
}

pub struct BuildPipeline<'a> {
  /// The number of threads that can be used
  /// to split up tasks.
  threads: usize,
  ascript: Option<AScriptStore>,
  bytecode: Option<BytecodeOutput>,
  source_output_dir: PathBuf,
  build_output_dir: PathBuf,
  tasks: Vec<(PipelineTask, PipelineContext<'a>)>,
  parser_name: String,
  grammar_name: String,
  error_handler: Option<fn(errors: Vec<CompileError>)>,
  grammar: Option<GrammarStore>,
  cached_source: CachedSource,
}

impl<'a> BuildPipeline<'a> {
  fn build_pipeline(number_of_threads: usize, cached_source: CachedSource) -> Self {
    Self {
      parser_name: "undefined_parser".to_string(),
      grammar_name: "undefined".to_string(),
      threads: number_of_threads,
      tasks: vec![],
      grammar: None,
      ascript: None,
      bytecode: None,
      cached_source,
      error_handler: None,
      source_output_dir: PathBuf::new(),
      build_output_dir: std::env::var("CARGO_MANIFEST_DIR")
        .map_or(std::env::temp_dir(), |d| PathBuf::from(&d)),
    }
  }

  /// Create a new build pipeline after constructing
  /// a grammar store from a source file. Returns Error
  /// if the grammar could not be created, otherwise, a
  /// BuildPipeline is returned.
  pub fn from_source(source_path: &PathBuf, number_of_threads: usize) -> Self {
    Self::build_pipeline(number_of_threads, CachedSource::Path(source_path.to_owned()))
  }

  /// Create a new build pipeline after constructing
  /// a grammar store from a source string. Returns Error
  /// if the grammar could not be created, otherwise, a
  /// BuildPipeline is returned.
  pub fn from_string(grammar_source: &str, base_directory: &PathBuf) -> Self {
    Self::build_pipeline(
      get_num_of_available_threads(),
      CachedSource::String(grammar_source.to_string(), base_directory.to_owned()),
    )
  }

  /// Set the path for generated source files.
  pub fn set_source_output_dir(&mut self, output_path: &PathBuf) -> &mut Self {
    self.source_output_dir = output_path.clone();

    self
  }

  /// Set the path for the build directory.
  pub fn set_build_output_dir(&mut self, output_path: &PathBuf) -> &mut Self {
    self.build_output_dir = output_path.clone();
    self
  }

  pub fn add_task(&'a mut self, task: PipelineTask) -> &mut Self {
    self.tasks.push((task, PipelineContext::new(self)));
    self
  }

  pub fn set_parser_name(&mut self, parser_name: String) {
    self.parser_name = parser_name;
  }

  pub fn set_grammar_name(&mut self, grammar_name: String) {
    self.grammar_name = grammar_name;
  }

  pub fn run(&'a mut self) -> Self {
    if self.grammar.is_none() {
      match self.build_grammar() {
        Err(errors) => {
          if let Some(error_handler) = &self.error_handler {
            error_handler(errors);
          }
          return Self::build_pipeline(self.threads, self.cached_source.to_owned());
        }
        Ok(_) => {}
      }
    }

    if self.tasks.iter().any(|t| t.0.require_ascript) && self.ascript.is_none() {
      let mut ascript = AScriptStore::new();

      let errors =
        compile_reduce_function_expressions(&self.grammar.as_ref().unwrap(), &mut ascript);

      if errors.len() > 0 {
        if let Some(error_handler) = &self.error_handler {
          error_handler(
            errors.into_iter().map(|err| CompileError::from_parse_error(&err)).collect(),
          );
        }

        return Self::build_pipeline(self.threads, self.cached_source.to_owned());
      }

      self.ascript = Some(ascript);
    }

    self.bytecode = None;
    if self.tasks.iter().any(|t| t.0.require_bytecode) {
      let bytecode_output = compile_bytecode(&self.grammar.as_ref().unwrap(), 1);

      self.bytecode = Some(bytecode_output);
    }

    let errors = thread::scope::<'a>(|scope| {
      let results = self.tasks.iter().map(|(t, ctx)| {
        scope.spawn(|| {
          let mut ctx = ctx.clone();
          match ctx.ensure_paths_exists() {
            Err(err) => {
              return Err(CompileError::from_io_error(&err));
            }
            _ => {}
          }

          ctx.pipeline = Some(self);
          match (t.fun)(&mut ctx) {
            Ok(_) => Ok(()),
            Err(err) => {
              ctx.clear_artifacts();
              Err(err)
            }
          }
        })
      });

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

    Self::build_pipeline(self.threads, self.cached_source.to_owned())
  }

  pub fn set_error_handler(&mut self, error_handler: fn(Vec<CompileError>)) -> &mut Self {
    self.error_handler = Some(error_handler);
    self
  }

  fn build_grammar(&mut self) -> Result<(), Vec<CompileError>> {
    let (grammar, errors) = match &self.cached_source {
      CachedSource::Path(path) => compile_from_path(&path, self.threads),
      CachedSource::String(string, base_dir) => compile_from_string(&string, &base_dir),
    };

    match grammar {
      Some(grammar) => {
        self.grammar = Some(grammar);
        Ok(())
      }
      None => Err(errors.into_iter().map(|err| CompileError::from_parse_error(&err)).collect()),
    }
  }
}

#[derive(Clone)]
pub struct PipelineContext<'a> {
  pipeline:          Option<&'a BuildPipeline<'a>>,
  artifact_paths:    Vec<PathBuf>,
  source_output_dir: PathBuf,
  build_output_dir:  PathBuf,
  parser_name:       String,
  grammar_name:      String,
}

impl<'a> PipelineContext<'a> {
  fn new(pipeline: &BuildPipeline) -> Self {
    PipelineContext {
      source_output_dir: pipeline.source_output_dir.clone(),
      build_output_dir:  pipeline.build_output_dir.clone(),
      parser_name:       pipeline.parser_name.clone(),
      grammar_name:      pipeline.grammar_name.clone(),
      pipeline:          None,
      artifact_paths:    vec![],
    }
  }

  // Ensure output destinations exist.
  fn ensure_paths_exists(&self) -> std::io::Result<()> {
    create_dir_all(self.source_output_dir.clone())?;
    create_dir_all(self.build_output_dir.clone())
  }

  fn clear_artifacts(&self) -> std::io::Result<()> {
    for path in &self.artifact_paths {
      if path.exists() {
        std::fs::remove_file(path)?;
      }
    }
    Ok(())
  }

  pub fn create_file(&mut self, path: PathBuf) -> std::io::Result<File> {
    match std::fs::File::create(&path) {
      Ok(file) => {
        self.artifact_paths.push(path.clone());
        Ok(file)
      }
      Err(err) => Err(err),
    }
  }

  pub fn add_artifact_path(&mut self, path: PathBuf) {
    self.artifact_paths.push(path.clone());
  }

  pub fn get_source_output_dir(&self) -> &PathBuf {
    &self.source_output_dir
  }

  pub fn get_build_output_dir(&self) -> &PathBuf {
    &self.build_output_dir
  }

  pub fn get_grammar(&self) -> &GrammarStore {
    &self.pipeline.unwrap().grammar.as_ref().unwrap()
  }

  pub fn get_ascript(&self) -> &AScriptStore {
    if self.pipeline.unwrap().ascript.is_none() {
      panic!("Failed to construct Ascript data");
    } else {
      self.pipeline.unwrap().ascript.as_ref().unwrap()
    }
  }

  pub fn get_parser_name(&self) -> &String {
    &self.get_grammar_name()
  }

  pub fn get_grammar_name(&self) -> &String {
    &self.get_grammar().friendly_name
  }

  pub fn get_grammar_path(&self) -> &PathBuf {
    &self.get_grammar().source_path
  }

  pub fn get_bytecode(&self) -> &BytecodeOutput {
    if self.pipeline.unwrap().bytecode.is_none() {
      panic!(
        "Failed to construct BytecodeOutput data. 
    Ensure all tasks that access `get_ascript` also `require_ascript`"
      );
    } else {
      self.pipeline.unwrap().bytecode.as_ref().unwrap()
    }
  }
}
