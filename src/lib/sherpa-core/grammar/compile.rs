#![allow(unused_mut, unused)]
use super::build_grammar::{create_grammar_data, extract_productions, parse_grammar, GrammarData};
use crate::{
  grammar::build_grammar::{
    convert_grammar_data_to_header,
    process_parse_state,
    process_production,
  },
  journal::{Journal, ReportType},
  tasks::{new_taskman, Spawner, ThreadedFuture},
  types::*,
};
use std::{
  collections::{HashSet, VecDeque},
  path::PathBuf,
  sync::{mpsc::sync_channel, Arc, Mutex, RwLock},
  time::Duration,
};

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
      let result = import_reader.lock().unwrap().recv_timeout(Duration::from_millis(2));
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

                let grammar_source_path = PathBuf::from(path.to_str(&g_c.string_store).as_str());

                let grammar_data = load_from_path(&mut j, grammar_source_path, &g_c);

                match grammar_data {
                  SherpaResult::Ok(g_data) => {
                    {
                      let mut local_loader = import_loader.lock().unwrap();
                      if g_data.imports.is_empty() == false {
                        /* Scope for Mutex lock */
                        g_data
                          .imports
                          .iter()
                          .for_each(|(_, i)| local_loader.send(Task::Todo(*i)).unwrap());
                      }
                      local_loader.send(Task::Complete).unwrap();
                    }

                    compile_grammar_data(&mut j, g_data, &g_c)
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

pub fn compile_grammar_data(
  j: &mut Journal,
  g_data: GrammarData,
  g_c: &GrammarSoup,
) -> SherpaResult<GrammarIdentity> {
  let id = g_data.id;

  let (mut prods, mut parse_states) = extract_productions(j, &g_data, &g_c.string_store)?;

  for prod in prods {
    let prod = process_production(prod, &g_data, &g_c.string_store)?;

    //g_c.productions.write().unwrap().insert(prod.id, prod);
    g_c.productions.write().unwrap().push(prod);
  }

  for state in parse_states {
    let state = process_parse_state(state, &g_data, &g_c.string_store)?;

    g_c.custom_states.write().unwrap().insert(state.id, state);
  }

  {
    g_c
      .grammar_headers
      .write()
      .unwrap()
      .insert(g_data.id.guid, convert_grammar_data_to_header(g_data.id, g_data));
  }

  SherpaResult::Ok(id)
}

fn load_from_path(
  j: &mut Journal,
  source_path: PathBuf,
  soup: &GrammarSoup,
) -> SherpaResult<GrammarData> {
  dbg!(&source_path);
  match std::fs::read_to_string(&source_path) {
    Ok(source) => {
      let source = std::fs::read_to_string(&source_path)?;
      load_from_str(j, source.as_str(), source_path, soup)
    }
    Err(_) => SherpaResult::Err(
      ("Unable to retrieve a grammar source from this path: ".to_string()
        + source_path.as_os_str().to_str().unwrap())
      .into(),
    ),
  }
}

fn load_from_str(
  j: &mut Journal,
  source: &str,
  source_path: PathBuf,
  soup: &GrammarSoup,
) -> SherpaResult<GrammarData> {
  let root_grammar = parse_grammar(&source)?;

  let g_data = create_grammar_data(j, root_grammar, &source_path, &soup.string_store)?;

  SherpaResult::Ok(g_data)
}

/// Entrypoint for compiling a single of grammar from a source file.
pub fn compile_grammar_from_str(
  j: &mut Journal,
  source: &str,
  source_path: PathBuf,
  soup: &GrammarSoup,
) -> SherpaResult<GrammarIdentity> {
  let root_id = GrammarIdentity::from_path(&source_path, &soup.string_store);

  let g_data = load_from_str(j, source, source_path, soup)?;

  compile_grammar_data(j, g_data, soup)
}

/// Entrypoint for compiling a collection of grammars from a source file.
pub async fn compile_grammars_from_path(
  mut j: Journal,
  grammar_source_path: PathBuf,
  grammar_cloud: &GrammarSoup,
  spawner: &Spawner<SherpaResult<()>>,
) -> SherpaResult<GrammarIdentity> {
  let root_id = GrammarIdentity::from_path(&grammar_source_path, &grammar_cloud.string_store);

  let imports = vec![root_id];

  import_grammars(&mut j, imports, spawner, grammar_cloud).await?;

  SherpaResult::Ok(root_id)
}
