#![allow(unused_mut, unused)]
use super::{
  finalize::finalize_grammar,
  load_2::{
    create_grammar_data,
    extract_productions,
    parse_grammar,
    GrammarData,
  },
  merge::merge_grammars,
  string::{CachedString, IString, StringStore},
  types::{Array, GrammarIdentity, GrammarSoup, Set},
};
use crate::{
  compile::{GrammarId, GrammarStore},
  grammar::{
    multitask::WorkVerifier,
    new::{
      load::{load, pre_load},
      load_2::{
        convert_grammar_data_to_header,
        process_parse_state,
        process_prod,
      },
      types::GrammarHeader,
    },
  },
  tasks::{new_taskman, Spawner, ThreadedFuture},
  Journal,
  ReportType,
  SherpaError,
  SherpaResult,
};
use std::{
  collections::{HashSet, VecDeque},
  path::PathBuf,
  sync::{mpsc::sync_channel, Arc, Mutex, RwLock},
  time::Duration,
};

/// Entrypoint for compiling a grammar from a source file.
pub(crate) async fn compile_grammars_from_path(
  mut j: Journal,
  grammar_source_path: PathBuf,
  grammar_cloud: &GrammarSoup,
  spawner: &Spawner<SherpaResult<()>>,
) -> SherpaResult<GrammarIdentity> {
  let root_id = GrammarIdentity::from_path(
    &grammar_source_path,
    &grammar_cloud.string_store,
  );

  let imports = vec![root_id];

  import_grammars(&mut j, imports, spawner, grammar_cloud).await?;

  SherpaResult::Ok(root_id)
}

async fn import_grammars(
  j: &mut Journal,
  imports: Vec<GrammarIdentity>,
  spawner: &Spawner<SherpaResult<()>>,
  grammar_cloud: &GrammarSoup,
) -> SherpaResult<()> {
  #[derive(Clone, Copy)]
  enum Task {
    Todo(GrammarIdentity),
    Complete,
  }

  let mut confirmed_imports = Set::new();

  let (import_loader, import_reader) = sync_channel(100);
  let mut pending_count = 0;

  imports.iter().for_each(|i| import_loader.send(Task::Todo(*i)).unwrap());

  let import_loader = Arc::new(Mutex::new(import_loader));
  let import_reader = Arc::new(Mutex::new(import_reader));
  let mut pending_tasks = Array::new();

  loop {
    if let Ok(task) = {
      let import_reader = import_reader.clone();
      let result =
        import_reader.lock().unwrap().recv_timeout(Duration::from_millis(2));
      drop(import_reader);
      result
    } {
      match task {
        Task::Todo(import_id) => {
          if confirmed_imports.insert(import_id.guid) {
            pending_count += 1;
            let import_loader = import_loader.clone();
            let spawner = spawner.clone();
            let g_c = (*grammar_cloud).clone();
            let mut j = j.transfer();
            let task = ThreadedFuture::new(
              async move {
                let GrammarIdentity { guid, name, path } = import_id;

                let grammar_source_path =
                  PathBuf::from(path.to_str(&g_c.string_store).as_str());

                match load_from_path(&mut j, grammar_source_path, &g_c) {
                  SherpaResult::Ok(g_data) => {
                    {
                      let mut local_loader = import_loader.lock().unwrap();
                      if g_data.imports.is_empty() == false {
                        /* Scope for Mutex lock */
                        g_data.imports.iter().for_each(|(_, i)| {
                          local_loader.send(Task::Todo(*i)).unwrap()
                        });
                      }
                      local_loader.send(Task::Complete).unwrap();
                    }

                    let (mut prods, mut parse_states) =
                      extract_productions(&mut j, &g_data, &g_c.string_store)?;

                    for prod in prods {
                      let prod =
                        process_prod(prod, &g_data, &g_c.string_store)?;

                      g_c.productions.write().unwrap().insert(prod.id, prod);
                    }

                    for state in parse_states {
                      let state =
                        process_parse_state(state, &g_data, &g_c.string_store)?;

                      g_c
                        .custom_states
                        .write()
                        .unwrap()
                        .insert(state.id, state);
                    }

                    {
                      g_c.grammar_headers.write().unwrap().insert(
                        guid,
                        convert_grammar_data_to_header(import_id, g_data),
                      );
                    }

                    SherpaResult::Ok(import_id)
                  }
                  SherpaResult::Err(err) => {
                    let mut local_loader = import_loader.lock().unwrap();
                    local_loader.send(Task::Complete).unwrap();

                    #[cfg(debug_assertions)]
                    println!("{}", err);

                    SherpaResult::Err(err)
                  }
                  _ => unreachable!(),
                }
              },
              &spawner,
            );
            pending_tasks.push(task);
          }
        }
        Task::Complete => {
          pending_count -= 1;
          if pending_count == 0 {
            break;
          }
        }
      }
    } else {
      for task in pending_tasks {
        // Ensure some progress can be made.
        task.await?;
      }
      pending_tasks = Array::new();
    }
  }

  // Wait for any extra tasks.
  for task in pending_tasks {
    task.await?;
  }

  SherpaResult::Ok(())
}

fn load_from_path(
  j: &mut Journal,
  grammar_source_path: PathBuf,
  grammar_cloud: &GrammarSoup,
) -> SherpaResult<GrammarData> {
  match std::fs::read_to_string(&grammar_source_path) {
    Ok(source) => {
      let grammar_source = std::fs::read_to_string(&grammar_source_path)?;

      let root_grammar = parse_grammar(&grammar_source)?;

      let g_data = create_grammar_data(
        j,
        root_grammar,
        &grammar_source_path,
        &grammar_cloud.string_store,
      )?;
      SherpaResult::Ok(g_data)
    }
    Err(_) => SherpaResult::Err(
      ("Unable to retrieve a grammar source from this path: ".to_string()
        + grammar_source_path.as_os_str().to_str().unwrap())
      .into(),
    ),
  }
}

#[test]
fn load_grammar_from_str() -> SherpaResult<()> {
  let grammar_cloud = GrammarSoup::new();
  let grammar_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../../../test/grammars/load.sg")
    .canonicalize()
    .unwrap();

  //let grammar_source_path = PathBuf::from("/test.sg");

  let mut j = Journal::new(None);
  j.set_active_report("test", ReportType::Any);

  let (executor, spawner) = new_taskman(1000);

  let local_spawner = spawner.clone();
  let local_grammar_cloud = grammar_cloud.clone();
  let mut local_j = j.transfer();

  spawner.spawn(async move {
    compile_grammars_from_path(
      local_j.transfer(),
      grammar_source_path,
      &local_grammar_cloud,
      &local_spawner,
    )
    .await;

    local_j.flush_reports();

    SherpaResult::Ok(())
  });

  drop(spawner);

  executor.join();

  println!("{:#?}", grammar_cloud);

  j.flush_reports();

  SherpaResult::Ok(())
}
