use crate::{
  ascript::types::AScriptStore,
  bytecode::{compile_bytecode, BytecodeOutput},
  journal::*,
  parser::{compile_parse_states, optimize_parse_states},
  types::*,
  util::get_num_of_available_threads,
};
use std::{
  collections::BTreeMap,
  fs::{create_dir_all, File},
  io::Write,
  path::PathBuf,
  sync::Arc,
  thread,
  vec,
};

use super::{
  ascript::build_ascript_types_and_functions,
  bytecode::build_bytecode_parser,
  disassembly::build_bytecode_disassembly,
  llvm::{build_llvm_parser, build_llvm_parser_interface},
  rust_preamble::build_rust_preamble,
};

pub type TaskFn = Box<
  dyn Fn(&mut PipelineContext) -> Result<Option<(u32, String)>, Vec<SherpaError>> + Sync + Send,
>;

pub struct PipelineTask {
  pub(crate) fun: TaskFn,
  pub(crate) require_ascript: bool,
  pub(crate) require_bytecode: bool,
  pub(crate) require_states: bool,
}

impl Default for PipelineTask {
  fn default() -> Self {
    Self {
      fun: Box::new(Self::_default_tsk_fun),
      require_ascript: false,
      require_bytecode: false,
      require_states: false,
    }
  }
}

impl PipelineTask {
  fn _default_tsk_fun(_: &mut PipelineContext) -> Result<Option<(u32, String)>, Vec<SherpaError>> {
    Err(vec![])
  }
}

#[derive(Debug, Clone)]
pub enum CachedSource {
  Path(PathBuf),
  String(String, PathBuf),
}

/// TODO:DOC

pub struct BuildPipeline<'a> {
  /// The number of threads that can be used
  /// to split up tasks.
  threads: usize,
  ascript_name: Option<String>,
  ascript: Option<AScriptStore>,
  bytecode: Option<BytecodeOutput>,
  states: Option<Vec<(String, Box<ParseState>)>>,
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
  fn build_pipeline(config: Config, cached_source: CachedSource) -> Self {
    let config = Some(config);
    Self {
      parser_name: "undefined_parser".to_string(),
      grammar_name: "undefined".to_string(),
      threads: get_num_of_available_threads(),
      tasks: vec![],
      ascript_name: None,
      journal: Journal::new(config),
      ascript: None,
      states: None,
      bytecode: None,
      cached_source,
      source_output_dir: PathBuf::new(),
      build_output_dir: std::env::var("CARGO_MANIFEST_DIR")
        .map_or(std::env::temp_dir(), |d| PathBuf::from(&d)),
      proc_context: false,
      source_name: None,
    }
  }

  /// Prevents source code artifact strings from being written to file
  pub fn make_proc(mut self) -> Self {
    self.proc_context = true;
    self
  }

  /// Create a new build pipeline based on a source grammar file.
  pub fn from_source(source_path: PathBuf, config: Config) -> Self {
    Self::build_pipeline(config, CachedSource::Path(source_path))
  }

  /// Create a new build pipeline based on a source grammar string.
  pub fn from_string(grammar_source: &str, base_directory: &PathBuf, config: Config) -> Self {
    Self::build_pipeline(
      config,
      CachedSource::String(grammar_source.to_string(), base_directory.to_owned()),
    )
  }

  /// TODO:DOC
  pub fn set_ascript_ast_name(&mut self, name: &str) -> &mut Self {
    self.ascript_name = Some(name.to_string());

    self
  }

  /// Set the path for generated source files.
  pub fn set_source_output_dir(&mut self, output_path: &PathBuf) -> &mut Self {
    self.source_output_dir = output_path.clone();

    self
  }

  /// If set, a source file will be generated in the root of the build directory, containing
  /// the concatenated source output from the build steps.
  /// The % character serves as the place holder for the grammar name.
  pub fn set_source_file_name(&mut self, name: &str) -> &mut Self {
    self.source_name = Some(name.to_string());
    self
  }

  /// Set the path for the build directory.
  pub fn set_build_output_dir(&mut self, output_path: &PathBuf) -> &mut Self {
    self.build_output_dir = output_path.clone();
    self
  }

  /// TODO:DOC
  pub fn add_task(&mut self, task: PipelineTask) -> &mut Self {
    self.tasks.push((task, PipelineContext::new(&self)));
    self
  }

  /// TODO:DOC

  pub fn set_parser_name(&mut self, parser_name: String) -> &mut Self {
    self.parser_name = parser_name;
    self
  }

  /// TODO:DOC
  pub fn set_grammar_name(&mut self, grammar_name: String) -> &mut Self {
    self.grammar_name = grammar_name;
    self
  }

  /// TODO:DOC
  pub fn get_journal(&self) -> Journal {
    self.journal.transfer()
  }

  /// TODO:DOC
  pub fn run<Function: FnOnce(Vec<SherpaError>)>(
    &mut self,
    error_handler: Function,
  ) -> SherpaResult<(Self, Vec<(u32, String)>, bool)> {
    let mut source_parts = vec![];
    let mut errors = vec![];

    match self.build_grammar() {
      SherpaResult::Ok(_) => {}
      _ => {
        self.journal.flush_reports();
        self.journal.debug_error_report();
        return SherpaResult::None;
      }
    }

    self.ascript = if self.tasks.iter().any(|t| t.0.require_ascript) && self.ascript.is_none() {
      match AScriptStore::new(&mut self.journal) {
        SherpaResult::Ok(ascript) => Some(ascript),
        _ => {
          self.journal.flush_reports();
          self.journal.debug_error_report();
          return SherpaResult::None;
        }
      }
    } else {
      self.ascript.take()
    };

    // Build bytecode if needed.
    self.states = if self.tasks.iter().any(|t| t.0.require_states || t.0.require_bytecode) {
      let parse_states = compile_parse_states(&mut self.journal, self.threads);
      self.journal.debug_error_report();
      Some(optimize_parse_states(&mut self.journal, parse_states?))
    } else {
      None
    };

    self.bytecode = if self.tasks.iter().any(|t| t.0.require_bytecode) {
      if let Some(parse_states) = &self.states {
        Some(compile_bytecode(&mut self.journal, parse_states).unwrap())
      } else {
        None
      }
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
          let data = source_parts
            .iter()
            .cloned()
            .collect::<BTreeMap<_, _>>()
            .into_values()
            .collect::<Vec<_>>()
            .join("\n");
          parser_data_file.write_all(&data.as_bytes()).unwrap();
          parser_data_file.flush().unwrap();
        }
      }
    }

    SherpaResult::Ok((
      Self::build_pipeline(self.journal.config().clone(), self.cached_source.to_owned()),
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
  _is_proc:          bool,
}

impl<'a> PipelineContext<'a> {
  fn new(pipeline: &BuildPipeline) -> Self {
    PipelineContext {
      source_output_dir: pipeline.source_output_dir.clone(),
      build_output_dir:  pipeline.build_output_dir.clone(),
      pipeline:          None,
      artifact_paths:    vec![],
      _is_proc:          pipeline.proc_context,
    }
  }

  pub fn _in_proc_context(&self) -> bool {
    self._is_proc
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

  pub fn get_states(&self) -> Option<&Vec<(String, Box<ParseState>)>> {
    self.pipeline.unwrap().states.as_ref()
  }
}

/// Convenience function for building a bytecode based parser. Use this in
/// build scripts to output a parser source file to `{OUT_DIR}/{grammar_name}.rs`.
pub fn compile_bytecode_parser(grammar_source_path: &PathBuf, config: Config) -> bool {
  let out_dir = std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap();

  create_dir_all(&out_dir).unwrap();

  let mut pipeline = BuildPipeline::from_source(grammar_source_path.to_owned(), config.clone());

  pipeline
    .set_source_output_dir(&out_dir)
    .set_build_output_dir(&out_dir)
    .set_source_file_name("%.rs")
    .add_task(build_rust_preamble())
    .add_task(build_bytecode_parser())
    .add_task(build_bytecode_disassembly());

  if config.enable_ascript {
    pipeline.add_task(build_ascript_types_and_functions(SourceType::Rust));
  }

  match pipeline.run(|errors| {
    for error in &errors {
      eprintln!("{}", error);
    }
  }) {
    SherpaResult::Ok(_) => true,
    _ => false,
  }
}

/// Convenience function for building a llvm machine code parser. Use this in
/// build scripts to output a parser source file to `{OUT_DIR}/{grammar_name}.rs`.
pub fn compile_llvm_parser(grammar_source_path: &PathBuf, config: Config) -> bool {
  let out_dir = std::env::var("OUT_DIR").map(|d| PathBuf::from(&d)).unwrap();

  create_dir_all(&out_dir).unwrap();

  let mut pipeline = BuildPipeline::from_source(grammar_source_path.to_owned(), config.clone());

  pipeline
    .set_source_output_dir(&out_dir)
    .set_build_output_dir(&out_dir)
    .set_source_file_name("%.rs")
    .add_task(build_rust_preamble())
    .add_task(build_bytecode_disassembly())
    .add_task(build_llvm_parser(None, true, true))
    .add_task(build_llvm_parser_interface());

  if config.enable_ascript {
    pipeline.add_task(build_ascript_types_and_functions(SourceType::Rust));
  }

  match pipeline.run(|errors| {
    for error in &errors {
      panic!("{}", error);
    }
  }) {
    SherpaResult::Ok(_) => true,
    SherpaResult::Err(err) => {
      eprintln!("{}", err);
      false
    }
    _ => false,
  }
}
/// TODO: DOC
#[derive(Debug, Clone, Copy)]
pub enum SourceType {
  /// TODO: DOC
  Rust,
  /// TODO: DOC
  TypeScript,
  /// TODO: DOC
  Go,
  /// TODO: DOC
  Cpp,
}
