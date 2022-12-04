use crate::{
  ascript::types::AScriptStore,
  compile::{compile_bytecode, compile_states, optimize_ir_states, BytecodeOutput},
  get_num_of_available_threads,
  journal::*,
  types::*,
};
use std::{
  fs::{create_dir_all, File},
  io::Write,
  num::NonZeroUsize,
  path::PathBuf,
  sync::Arc,
  thread,
  vec,
};

pub type TaskFn =
  Box<dyn Fn(&mut PipelineContext) -> Result<Option<String>, Vec<SherpaError>> + Sync + Send>;

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
  ascript_name: Option<String>,
  ascript: Option<AScriptStore>,
  bytecode: Option<BytecodeOutput>,
  source_output_dir: PathBuf,
  build_output_dir: PathBuf,
  tasks: Vec<(PipelineTask, PipelineContext<'a>)>,
  parser_name: String,
  grammar_name: String,
  journal: Journal,
  source_name: Option<String>,
  cached_source: CachedSource,
  proc_context: bool,
}

impl<'a> BuildPipeline<'a> {
  fn build_pipeline(number_of_threads: usize, cached_source: CachedSource) -> Self {
    let config = Some(Config { ..Default::default() });
    Self {
      parser_name: "undefined_parser".to_string(),
      grammar_name: "undefined".to_string(),
      threads: if number_of_threads == 0 {
        std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()).get()
      } else {
        number_of_threads
      },
      tasks: vec![],
      ascript_name: None,
      journal: Journal::new(config),
      ascript: None,
      bytecode: None,
      cached_source,
      source_output_dir: PathBuf::new(),
      build_output_dir: std::env::var("CARGO_MANIFEST_DIR")
        .map_or(std::env::temp_dir(), |d| PathBuf::from(&d)),
      proc_context: false,
      source_name: None,
    }
  }

  /// Create a new build pipeline from a string.
  pub fn proc_context(grammar_source: &str, base_directory: &PathBuf) -> Self {
    let mut self_ = Self::build_pipeline(
      get_num_of_available_threads(),
      CachedSource::String(grammar_source.to_string(), base_directory.to_owned()),
    );

    self_.proc_context = true;

    self_
  }

  /// Create a new build pipeline after constructing a grammar store from a source file. Returns
  /// Error if the grammar could not be created, otherwise, a BuildPipeline is returned.
  ///
  /// If `number_of_threads` is `0`, the compiler will use a number of threads equal to the
  /// the number of CPU cores reported by the system.
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

  pub fn set_ascript_ast_name(mut self, name: &str) -> Self {
    self.ascript_name = Some(name.to_string());

    self
  }

  /// Set the path for generated source files.
  pub fn set_source_output_dir(mut self, output_path: &PathBuf) -> Self {
    self.source_output_dir = output_path.clone();

    self
  }

  /// If set, a source file will be generated in the root of the build directory, containing
  /// the concatenated source output from the build steps.
  /// The % character serves as the place holder for the grammar name.
  pub fn set_source_file_name(mut self, name: &str) -> Self {
    self.source_name = Some(name.to_string());
    self
  }

  /// Set the path for the build directory.
  pub fn set_build_output_dir(mut self, output_path: &PathBuf) -> Self {
    self.build_output_dir = output_path.clone();
    self
  }

  pub fn add_task(mut self, task: PipelineTask) -> Self {
    self.tasks.push((task, PipelineContext::new(&self)));
    self
  }

  pub fn set_parser_name(mut self, parser_name: String) -> Self {
    self.parser_name = parser_name;
    self
  }

  pub fn set_grammar_name(mut self, grammar_name: String) -> Self {
    self.grammar_name = grammar_name;
    self
  }

  pub fn get_journal(&self) -> Journal {
    self.journal.transfer()
  }

  pub fn run<Function: FnOnce(Vec<SherpaError>)>(
    mut self,
    error_handler: Function,
  ) -> SherpaResult<(Self, Vec<String>, bool)> {
    let mut source_parts = vec![];
    let mut errors = vec![];

    self.build_grammar()?;

    self.ascript = if self.tasks.iter().any(|t| t.0.require_ascript) && self.ascript.is_none() {
      match AScriptStore::new(self.journal.grammar().unwrap()) {
        SherpaResult::MultipleErrors(mut e) => {
          errors.append(&mut e);
          None
        }
        SherpaResult::Ok(ascript) => Some(ascript),
        _ => unreachable!("Should not generate other invalid types"),
      }
    } else {
      self.ascript
    };

    // Build bytecode if needed.
    self.bytecode = if self.tasks.iter().any(|t| t.0.require_bytecode) {
      let ir_states = compile_states(&mut self.journal, self.threads)?;

      let ir_states = optimize_ir_states(&mut self.journal, ir_states);

      let bytecode_output = compile_bytecode(&mut self.journal, ir_states);

      Some(bytecode_output)
    } else {
      None
    };

    errors.append(&mut thread::scope(|scope| {
      let results = self.tasks.iter().map(|(t, ctx)| {
        scope.spawn(|| {
          let mut ctx = ctx.clone();
          match ctx.ensure_paths_exists() {
            Err(err) => {
              return Err(vec![SherpaError::from(err)]);
            }
            _ => {}
          }

          ctx.pipeline = Some(&self);

          match (t.fun)(&mut ctx) {
            Ok(Some(artifact)) => Ok(Some(artifact)),
            Ok(None) => Ok(None),
            Err(err) => {
              ctx.clear_artifacts().unwrap();
              Err(err)
            }
          }
        })
      });

      let errors = results
        .into_iter()
        .map(|r| r.join().unwrap())
        .map(|r| match r {
          Ok(Some(artifact)) => {
            source_parts.push(artifact);
            vec![]
          }
          Ok(_) => vec![],
          Err(e) => e,
        })
        .flatten()
        .collect::<Vec<_>>();

      errors
    }));

    if errors.have_errors() {
      error_handler(errors.clone());
    }

    if errors.have_critical() {
      // Critical errors indicate a breakdown in the build process. Thus, we should
      // not produce any artifacts and instead exit with an error message.
      eprintln!("Critical errors have occurred, could not complete build")
    } else {
      if let Some(source_name) = self.source_name.as_ref() {
        let source_name =
          source_name.to_string().replace("%", &self.journal.grammar().unwrap().id.name);
        let source_path = self.build_output_dir.join("./".to_string() + &source_name);
        eprintln!("{:?} {:?}", source_path, self.build_output_dir);
        if let Ok(mut parser_data_file) = std::fs::File::create(&source_path) {
          let data = source_parts.join("\n");
          parser_data_file.write_all(&data.as_bytes()).unwrap();
          parser_data_file.flush().unwrap();
        }
      }
    }

    SherpaResult::Ok((
      Self::build_pipeline(self.threads, self.cached_source.to_owned()),
      source_parts,
      !errors.have_critical(),
    ))
  }

  fn build_grammar(&mut self) -> SherpaResult<Arc<GrammarStore>> {
    match &self.cached_source {
      CachedSource::Path(path) => GrammarStore::from_path(&mut self.journal, path.clone()),
      CachedSource::String(string, base_dir) => {
        GrammarStore::from_str_with_base_dir(&mut self.journal, &string, &base_dir)
      }
    }
  }
}

#[derive(Clone)]
pub struct PipelineContext<'a> {
  pipeline:          Option<&'a BuildPipeline<'a>>,
  artifact_paths:    Vec<PathBuf>,
  source_output_dir: PathBuf,
  build_output_dir:  PathBuf,
  /// This is true if the is built within a proc macro context.
  is_proc:           bool,
}

impl<'a> PipelineContext<'a> {
  fn new(pipeline: &BuildPipeline) -> Self {
    PipelineContext {
      source_output_dir: pipeline.source_output_dir.clone(),
      build_output_dir:  pipeline.build_output_dir.clone(),
      pipeline:          None,
      artifact_paths:    vec![],
      is_proc:           pipeline.proc_context,
    }
  }

  pub fn in_proc_context(&self) -> bool {
    self.is_proc
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

  pub fn get_journal(&self) -> Journal {
    self.pipeline.unwrap().get_journal()
  }

  pub fn get_ascript(&self) -> Option<&AScriptStore> {
    self.pipeline.unwrap().ascript.as_ref()
  }

  pub fn get_bytecode(&self) -> Option<&BytecodeOutput> {
    self.pipeline.unwrap().bytecode.as_ref()
  }
}
