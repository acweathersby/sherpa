#![allow(unused_mut, unused)]
use super::build_grammar::{create_grammar_data, extract_nonterminals, parse_grammar, GrammarData};
use crate::{
  grammar::build_grammar::{convert_grammar_data_to_header, process_nonterminals, process_parse_state},
  journal::{Journal, ReportType},
  types::*,
};
use std::{
  collections::{HashSet, VecDeque},
  path::PathBuf,
  sync::{mpsc::sync_channel, Arc, Mutex, RwLock},
  time::Duration,
};

/// Entrypoint for compiling a single of grammar from a source file.
pub fn compile_grammar_from_str(
  j: &mut Journal,
  source: &str,
  source_path: PathBuf,
  s_store: IStringStore,
) -> SherpaResult<(Arc<GrammarSoup>, Vec<GrammarIdentities>)> {
  let mut soup = GrammarSoup::from_string_store(s_store);

  let root_id = GrammarIdentities::from_path(&source_path, &soup.string_store);

  let g_data = load_from_str(j, source, source_path, &soup)?;

  let imports = g_data.imports.values().cloned().collect();

  let grammar_id = compile_grammar_data(j, g_data, &soup)?;

  Ok((soup, imports))
}

/// Imports a single grammar from a source location and merges it into the soup.
/// Returns a list of grammars that are imported by the source grammar
pub fn load_grammar(
  j: &mut Journal,
  import_id: GrammarIdentities,
  s_store: IStringStore,
) -> SherpaResult<(Arc<GrammarSoup>, Vec<GrammarIdentities>)> {
  let mut g_c: Arc<GrammarSoup> = GrammarSoup::from_string_store(s_store);
  let GrammarIdentities { guid, guid_name, path, .. } = import_id;

  j.set_active_report("Load Grammar From Path", ReportType::GrammarCompile(guid));
  j.report_mut().add_note("File Path", path.to_string(&g_c.string_store));

  let grammar_source_path = PathBuf::from(path.to_str(&g_c.string_store).as_str());

  match load_from_path(j, grammar_source_path, &g_c) {
    SherpaResult::Ok(g_data) => {
      let imports = g_data.imports.values().cloned().collect();

      match compile_grammar_data(j, g_data, &g_c) {
        Ok(result) => Some(result),
        Err(err) => {
          j.report_mut().add_error(err);
          None
        }
      };

      Ok((g_c, imports))
    }
    SherpaResult::Err(err) => {
      j.report_mut().add_error(err.clone());
      Err(err)
    }
  }
}

fn compile_grammar_data(j: &mut Journal, g_data: GrammarData, g_s: &GrammarSoup) -> SherpaResult<GrammarIdentities> {
  let id = g_data.id;

  let (mut nterms, mut parse_states) = extract_nonterminals(j, &g_data, &g_s.string_store)?;

  for nterm in nterms {
    g_s.nonterminals.write().unwrap().push(process_nonterminals(nterm, &g_data, &g_s.string_store)?);
  }

  for state in parse_states {
    g_s.custom_states.write().unwrap().insert(state.id, process_parse_state(state, &g_data, &g_s.string_store)?);
  }

  {
    g_s.grammar_headers.write().unwrap().insert(g_data.id.guid, convert_grammar_data_to_header(g_data.id, g_data));
  }

  SherpaResult::Ok(id)
}

fn load_from_path(j: &mut Journal, source_path: PathBuf, soup: &GrammarSoup) -> SherpaResult<GrammarData> {
  match std::fs::read_to_string(&source_path) {
    Ok(source) => {
      let source = std::fs::read_to_string(&source_path)?;
      load_from_str(j, source.as_str(), source_path, soup)
    }
    Err(_) => SherpaResult::Err(if source_path.as_os_str().is_empty() {
      ("Source path for grammar is empty").into()
    } else {
      ("Unable to retrieve a grammar source from this path: ".to_string() + source_path.as_os_str().to_str().unwrap()).into()
    }),
  }
}

fn load_from_str(j: &mut Journal, source: &str, source_path: PathBuf, soup: &GrammarSoup) -> SherpaResult<GrammarData> {
  j.set_active_report("Load From String", ReportType::GrammarParse);

  let root_grammar = match parse_grammar(&source) {
    SherpaResult::Ok(root_grammar) => root_grammar,
    SherpaResult::Err(err) => {
      j.report_mut().add_error(err.clone());
      return Err(err);
    }
    _ => return SherpaResult::Err(SherpaError::from("Failed Parse")),
  };

  let g_data = create_grammar_data(j, root_grammar, &source_path, &soup.string_store)?;

  SherpaResult::Ok(g_data)
}
