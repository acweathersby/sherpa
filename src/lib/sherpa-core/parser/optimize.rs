//! The goal here is to reduce the number of states in the IR,
//! with the ultimate result being each state should perform at
//! least one "advancing" action (REDUCE, SHIFT, ACCEPT), and
//! preferably more than one. The focus is to remove states
//! that are comprised only of PushGoto statements, folding these
//! into the respective states that reference them.
use crate::{
  compile::SymbolID,
  grammar::{
    compile::parser::sherpa::{self, *},
    hash_id_value_u64,
  },
  journal::{Journal, ReportType},
  types::{BranchType, ExportedProduction, GrammarStore, ParseState},
  SherpaResult,
};
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

use super::hash_group_btreemap;

/// Attempts to reduce the number of IR states through merging states, and
/// reduce and reduce bytecode complexity by transforming instructions where
/// appropriate.
pub fn optimize_parse_states(
  j: &mut Journal,
  mut states: BTreeMap<String, Box<ParseState>>,
) -> Vec<(String, Box<ParseState>)> {
  let grammar = j.grammar().unwrap();
  let g = &grammar;

  j.set_active_report("Optimize States", ReportType::Optimize);
  j.report_mut().start_timer("Duration");

  let starting_states = states.len();

  // Setup config flags
  let inline_redundant_assertions = true;
  let remove_gotos_to_pass_states = true;
  let merge_non_transitive_branches = true;

  // Preform rounds -------------------------------------
  let entry_states: BTreeSet<String> = get_entry_state_names(g);

  let mut non_scanner_states = BTreeSet::new();

  // Join cyclic states.

  loop {
    // Find states with identical bodies

    let mut changes = merge_identical_states(&mut states);
    // Maps a pure PushGoto state id, in which  the state is only comprised of
    // PushGoto actions, to a list of that state's actions.
    let mut pure_goto_replacements = BTreeMap::new();
    // States whose instructions can be inlined within the FIRST PushGoto of a
    // caller production branch.
    let mut goto_replacements = BTreeMap::new();
    let mut state_branches = BTreeMap::new();
    let mut pass_states = BTreeSet::new();
    let mut fail_states = BTreeSet::new();
    let mut branch_references = HashMap::new();

    for state in states.values() {
      if inline_redundant_assertions {
        for (branch_data, instructions) in get_branches(state) {
          if let Some(branch_data) = branch_data {
            if !branch_data.default {
              branch_references
                .insert((state.get_name(), branch_data), instructions.clone());
            }
          }
        }
      }

      if let SherpaResult::Ok(state) = &state.ast {
        // PushGoto instructions are always placed at the end of any
        // instruction sequence, so if the first node is a PushGoto
        // then all subsequent nodes must also be PushGoto.
        match (
          state.instructions.len(),
          &state.instructions[0],
          state.instructions.last(),
        ) {
          (1, ASTNode::Pass(_), _) => {
            if remove_gotos_to_pass_states {
              pass_states.insert(state.id.clone());
            }
          }
          (1, ASTNode::Fail(_), _) => {
            fail_states.insert(state.id.clone());
          }
          (1, ASTNode::Goto(_), _)
          | (_, ASTNode::PushGoto(_), Some(ASTNode::Goto(_))) => {
            pure_goto_replacements
              .insert(state.id.clone(), state.instructions.clone());
          }
          (1, ASTNode::Reduce(_), _)
          | (2, ASTNode::TokenAssign(_), Some(&ASTNode::Pass(_)))
          | (1, ASTNode::TokenAssign(_), _) => {
            goto_replacements
              .insert(state.id.clone(), state.instructions.clone());
          }
          (_, ASTNode::ASSERT(box ASSERT { .. }), _) => {
            for branch in state.instructions.iter() {
              let ASTNode::ASSERT(box ASSERT { instructions, ids, mode, .. }) = branch else { continue;};
              state_branches.insert(
                (
                  state.id.clone(),
                  ids.val as u32,
                  BranchType::from(mode.as_str()),
                ),
                instructions.clone(),
              );
            }
          }
          _ => {}
        }
      }
    }

    // For each state, try to lower any state that is a pure
    // PushGoto state into the respective reference states.
    for state in states.values_mut() {
      let state_name = state.get_name();
      // Convert trivial scanner
      replace_trivial_scanner(state, g, &mut non_scanner_states);

      // Removes branch states that are identical to the default branch

      // Lower default only branch state into a branchless state.
      refactor_default_only(state, &mut changes);

      // Lower to pass when all branches are a pass state and default is present
      lower_allpass_branch_state(state, &mut changes);

      // Removes all branches whose bodies are identical to the default
      // branch's.
      remove_default_shadows(state, &mut changes);

      for (data, branch) in get_branches_mut(state.as_mut()) {
        if let Some(ASTNode::Goto(box sherpa::Goto { state })) = branch.last() {
          let val = &state.val;
          match (
            goto_replacements.get(val),
            pure_goto_replacements.get(val),
            fail_states.get(val),
            pass_states.contains(val),
          ) {
            (Some(instructions), ..) => {
              branch.pop();
              branch.append(&mut instructions.clone());
              changes = true;
            }
            // Replace PushGoto if it points to a pure PushGoto state.
            (_, Some(instructions), ..) => {
              branch.pop();
              branch.append(&mut instructions.clone());
              changes = true;
            }
            (.., true) if remove_gotos_to_pass_states => {
              if branch.len() == 1 {
                // Replace the PushGoto with pass to prevent an empty state.
                branch[0] = ASTNode::Pass(Box::new(Pass {}));
              } else {
                branch.pop();
              }
              changes = true;
            }
            (_, _, Some(_), _) => {
              branch.clear();
              branch.push(ASTNode::Fail(Box::new(sherpa::Fail::new())));
            }
            _ => {}
          }
        } else if !branch_has_terminal_end_node(branch) {
          if let Some((index, ASTNode::PushGoto(box PushGoto { state }))) =
            branch
              .iter()
              .enumerate()
              .filter(|(_, s)| s.get_type() == ASTNodeType::PushGoto)
              .last()
          {
            let state = state.clone();
            branch.remove(index);
            branch.push(ASTNode::Goto(Box::new(sherpa::Goto::new(state))));
            changes = true;
          }
        }

        if let Some(data) = data {
          if let Some(ASTNode::Goto(box sherpa::Goto { state })) = branch.last()
          {
            let other_state = state.val.clone();
            if merge_non_transitive_branches {
              // instructions from branches of different states that have the
              // same id can be merged
              if state_name != other_state {
                if let Some(instructions) =
                  state_branches.get(&(other_state, data.id, data.id_type))
                {
                  let self_is_transitive = is_transitive(&branch);
                  let other_is_transitive = is_transitive(&branch);

                  match (self_is_transitive, other_is_transitive) {
                    (true, true) => {
                      // Cannot combine two transitive branches.
                    }
                    _ => {
                      branch.pop();
                      branch.append(&mut instructions.clone());
                      changes = true;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    states = garbage_collect(j, states, &entry_states, &non_scanner_states);

    if !changes {
      break;
    }
  }

  let result: Vec<(String, Box<ParseState>)> =
    garbage_collect(j, states, &entry_states, &non_scanner_states);

  j.report_mut().stop_timer("Duration");

  let ending_states = result.len();

  if ending_states < starting_states {
    j.report_mut().add_note(
      "Results",
      format!(
        "Reduced {} states to {}, {}% reduction",
        starting_states,
        ending_states,
        (1. / ((ending_states as f64) / (starting_states as f64))) * 100.
      ),
    )
  } else {
    j.report_mut().add_note("Results", format!("Unable to reduce state count."))
  }

  result
}

fn branch_has_terminal_end_node(branch: &mut Vec<ASTNode>) -> bool {
  matches!(
    branch.last().unwrap().get_type(),
    ASTNodeType::Fail
      | ASTNodeType::Goto
      | ASTNodeType::Pass
      | ASTNodeType::Accept
  )
}

fn merge_identical_states(
  states: &mut BTreeMap<String, Box<ParseState>>,
) -> bool {
  let mut change = false;
  let merge_candidates = hash_group_btreemap(
    states.values().cloned().collect::<Vec<_>>(),
    |_, s| hash_id_value_u64(&s.get_ast().unwrap().instructions),
  );

  let mut merge_maps = BTreeMap::new();
  for (_, mut candidates) in merge_candidates {
    if candidates.len() > 1 {
      let root_candidate = candidates.pop().unwrap();
      let root_name = root_candidate.get_name();
      merge_maps.insert(root_name.clone(), root_name.clone());
      // println!("--------------------------------------------\nThese states
      // can be merged\n");
      for state in candidates {
        /*    debug_assert!(
          root_candidate.get_code_body().trim() == state.get_code_body().trim(),
          "Merged states should contain identical code\n{}\n{}",
          state.get_code_body().trim(),
          root_candidate.get_code_body().trim()
        ); */
        // println!("{}", state.to_string());
        merge_maps.insert(state.get_name(), root_name.clone());
      }
    } /* else {
        let state = candidates.pop().unwrap();
        merge_maps.insert(state.get_name(), state.get_name());
      } */
  }

  // Update gotos based on merge maps

  for state in states.values_mut() {
    for (_, branch) in get_branches_mut(state.as_mut()) {
      for node in
        branch.iter_mut().filter(|i| is_state_reference(i)).collect::<Vec<_>>()
      {
        match node {
          ASTNode::PushExceptHandler(box sherpa::PushExceptHandler {
            state,
          })
          | ASTNode::Goto(box sherpa::Goto { state })
          | ASTNode::PushGoto(box sherpa::PushGoto { state }) => {
            if let Some(name) = merge_maps.get(&state.val) {
              debug_assert!(
                merge_maps.get(&state.val).is_some(),
                "State [{}] does not exist",
                state.val
              );
              if state.val != *name {
                change = true;
                state.val = name.clone();
              }
            }
          }
          _ => unreachable!(),
        }
      }
    }
  }

  change
}

// Transitive actions perform a major operation that significantly changes the
// state of the parser context, by either changing the state of the current
// token or by changing the ordering of the goto stack (namely through pushing
// an exception handler or by popping off a production goto).
//
// Branches that contain transitive actions can be merged into referring
// branches, but once performed, the referring branch can no longer merge with
// other reference branches that themselves contain transitive actions.
fn is_transitive(instructions: &Vec<ASTNode>) -> bool {
  use ASTNodeType::*;
  instructions.iter().any(|n| {
    matches!(
      n.get_type(),
      ScanShift
        | PeekToken
        | ShiftToken
        | SkipToken
        | SkipPeekToken
        | PeekTokenScanless
        | ShiftTokenScanless
        | SkipTokenScanless
        | SkipPeekTokenScanless
        | PeekReset
        | Pop
    )
  })
}

fn replace_trivial_scanner(
  state: &mut Box<ParseState>,
  g: &std::sync::Arc<GrammarStore>,
  non_scanner_states: &mut BTreeSet<String>,
) {
  if !state.is_scanner() {
    if let Some(symbols) = state.get_scanner_symbol_set() {
      if all_symbols_are_a_single_codepoint(&symbols, g) {
        let lookup = map_bytecode_id_to_sym_id(symbols, g);

        for instruction in &mut state.ast.as_mut().unwrap().instructions {
          match instruction {
            ASTNode::ASSERT(box ASSERT { ids, instructions, mode, .. }) => {
              let sym_id = *lookup.get(&(ids.val as u32)).unwrap();
              let (id, bc_type) = sym_id.shift_info(&g);
              *mode = bc_type.to_string();
              ids.val = id as i64;

              match instructions[0] {
                ASTNode::ShiftToken(..) => {
                  instructions[0] = ASTNode::ShiftTokenScanless(Box::new(
                    ShiftTokenScanless::new(),
                  ));
                }
                ASTNode::PeekToken(..) => {
                  instructions[0] = ASTNode::PeekTokenScanless(Box::new(
                    PeekTokenScanless::new(),
                  ));
                }
                ASTNode::SkipPeekToken(..) => {
                  instructions[0] = ASTNode::SkipPeekTokenScanless(Box::new(
                    SkipPeekTokenScanless::new(),
                  ));
                }
                ASTNode::SkipToken(..) => {
                  instructions[0] = ASTNode::SkipTokenScanless(Box::new(
                    SkipTokenScanless::new(),
                  ));
                }
                _ => {}
              }
            }
            _ => {}
          }
        }
        // Now we're just direct accessing the raw binary data so we remove the
        // reliance on scanner states and symbols

        state.normal_symbols.clear();
        state.skip_symbols.clear();
        non_scanner_states.insert(state.get_name());
      }
    }
  }
}

fn lower_allpass_branch_state(state: &mut Box<ParseState>, changes: &mut bool) {
  if let SherpaResult::Ok(state) = &mut state.ast {
    if state.instructions.iter().all(|i| match i {
      ASTNode::DEFAULT(box DEFAULT { instructions, .. })
      | ASTNode::ASSERT(box ASSERT { instructions, .. }) => {
        instructions.len() == 1
          && instructions[0].get_type() == ASTNodeType::Pass
      }
      _ => false,
    }) && state
      .instructions
      .iter()
      .any(|s| s.get_type() == ASTNodeType::DEFAULT)
    {
      state.instructions = vec![ASTNode::Pass(Box::new(Pass::new()))];
      *changes = true;
    }
  }
}

fn remove_default_shadows(state: &mut Box<ParseState>, changes: &mut bool) {
  if let SherpaResult::Ok(state) = &mut state.ast {
    // Ensure homogeneity amongst branch types.
    let Some(default) = state.instructions.iter().filter(|s| matches!(s.get_type(), ASTNodeType::DEFAULT) ).last() else {return;};

    // Ensure homogeneity
    if state
      .instructions
      .iter()
      .filter_map(|s| match s {
        ASTNode::ASSERT(box ASSERT { mode, .. }) => Some(mode.to_string()),
        ASTNode::DEFAULT(..) => None,
        _ => unreachable!("All states that have a default branch should only contain additional assert branches. Found {:?}", s.get_type())
      })
      .collect::<BTreeSet<_>>()
      .len()
      != 1
    {
      return;
    }

    let default_id =
      hash_id_value_u64(&default.as_DEFAULT().unwrap().instructions);
    let old_len = state.instructions.len();
    let new_set = state.instructions.clone().into_iter().filter(|s| match s {
      ASTNode::DEFAULT(..) => true,
      ASTNode::ASSERT(box ASSERT { instructions, .. }) => {
        default_id != hash_id_value_u64(instructions)
      }
      _ => unreachable!("All states that have a default branch should only contain additional assert branches. Found {:?}", s.get_type())
    });

    state.instructions = new_set.collect();

    *changes |= old_len > state.instructions.len();
  }
}

fn refactor_default_only(state: &mut Box<ParseState>, changes: &mut bool) {
  if let SherpaResult::Ok(state) = &mut state.ast {
    if matches!(state.instructions[0], ASTNode::DEFAULT(_))
      && state.instructions.len() == 1
    {
      match state.instructions[0].clone() {
        ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => {
          state.instructions = instructions
        }
        _ => unreachable!(
          "Expected only ASSERT and DEFAULT nodes in instruction vector."
        ),
      }
      *changes = true;
    }
  }
}

/// Remove unreferenced states, when tracking linkage from the root entry
/// states, and reorders states in an attempt to cluster closely associated
/// states.
fn garbage_collect<T>(
  j: &mut Journal,
  mut states: BTreeMap<String, Box<ParseState>>,
  entry_states: &BTreeSet<String>,
  non_scanner_states: &BTreeSet<String>,
) -> T
where
  T: FromIterator<(String, Box<ParseState>)>,
{
  let mut reg_states = Vec::with_capacity(states.len());
  let mut scanner_states = Vec::with_capacity(states.len());
  let mut trace_queue = VecDeque::with_capacity(states.len());

  let mut references = BTreeSet::from_iter(entry_states.iter().cloned());

  trace_queue.append(&mut entry_states.iter().cloned().collect());

  // Starting at the entry states, proceed to trace PushGoto references
  // by inserting a referenced strings in the references stet
  while let Some(state_id) = trace_queue.pop_front() {
    if let Some(state) = states.get(&state_id) {
      if !state.is_scanner() {
        reg_states.push(state_id);
      } else {
        scanner_states.push(state_id);
      }

      if !non_scanner_states.contains(&state.get_name()) {
        if let Some(scanner_state_id) = state.get_scanner_state_name() {
          if references.insert(scanner_state_id.clone()) {
            trace_queue.push_back(scanner_state_id);
          }
        }
      }

      for (_, branch) in get_branches(state) {
        for node in branch.iter().filter(|i| is_state_reference(*i)) {
          match node {
            ASTNode::PushExceptHandler(box sherpa::PushExceptHandler {
              state,
            })
            | ASTNode::Goto(box sherpa::Goto { state })
            | ASTNode::PushGoto(box sherpa::PushGoto { state }) => {
              if references.insert(state.val.clone()) {
                trace_queue.push_back(state.val.clone());
              }
            }
            _ => unreachable!(),
          }
        }
      }
    }
  }

  let output = reg_states
    .into_iter()
    .chain(scanner_states.into_iter())
    .map(|r| (r.clone(), states.remove(&r).unwrap()))
    .collect::<T>();

  if !states.is_empty() {
    j.report_mut().add_note(
      "Garbage Collect",
      format!(
        "Removed the following states\n{}",
        states.into_iter().map(|s| s.0).collect::<Vec<_>>().join("\n")
      ),
    );
  }

  output
}

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub(crate) struct BranchData {
  /// The bytecode id of the discriminator symbol of this branch.
  id:      u32,
  /// The type of the discriminator symbol. This is the empty string in the
  /// case of a default branch.
  id_type: BranchType,
  /// Whether this branch is a default action. There should only be one of
  /// these per branching state.
  default: bool,
}

/// Returns a vector of referenced instruction vectors
/// which may either either contain the root instruction vector, or the
/// the vectors of individual branches in the case of a branch state
pub(crate) fn get_branches<'a>(
  parse_state: &'a ParseState,
) -> Vec<(Option<BranchData>, &'a Vec<ASTNode>)> {
  if let SherpaResult::Ok(ast) = &parse_state.ast {
    #[cfg(debug_assertions)]
    debug_assert!(
      !ast.instructions.is_empty(),
      "Expected parse state to contain instructions
source code:
{}
original_ast:
{:#?}
modified_ast:
{:#?}
",
      parse_state.to_string(),
      parse_state.compile_ast(),
      ast,
    );

    if matches!(ast.instructions[0], ASTNode::ASSERT(_) | ASTNode::DEFAULT(_)) {
      ast
        .instructions
        .iter()
        .map(|i| match i {
          ASTNode::ASSERT(box ASSERT { ids, instructions, mode, .. }) => (
            Some(BranchData {
              id:      ids.val as u32,
              default: false,
              id_type: mode.as_str().into(),
            }),
            instructions,
          ),
          ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => (
            Some(BranchData {
              id:      0,
              default: true,
              id_type: BranchType::UNKNOWN,
            }),
            instructions,
          ),
          _ => unreachable!(
            "Expected only ASSERT and DEFAULT nodes in instruction vector."
          ),
        })
        .collect()
    } else {
      vec![(None, &ast.instructions)]
    }
  } else {
    vec![]
  }
}

/// Returns a vector of mutable referenced instruction vector
/// which may either contain the root instruction vector, or the
/// the vectors of individual branches in the case of a branch state
fn get_branches_mut<'a>(
  state: &'a mut ParseState,
) -> Vec<(Option<BranchData>, &'a mut Vec<ASTNode>)> {
  if let SherpaResult::Ok(state) = &mut state.ast {
    if matches!(state.instructions[0], ASTNode::ASSERT(_) | ASTNode::DEFAULT(_))
    {
      state
        .instructions
        .iter_mut()
        .map(|i| match i {
          ASTNode::ASSERT(box ASSERT { ids, instructions, mode, .. }) => (
            Some(BranchData {
              id:      ids.val as u32,
              default: false,
              id_type: mode.as_str().into(),
            }),
            instructions,
          ),
          ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => (
            Some(BranchData {
              id:      0,
              default: true,
              id_type: BranchType::UNKNOWN,
            }),
            instructions,
          ),
          _ => unreachable!(
            "Expected only ASSERT and DEFAULT nodes in instruction vector."
          ),
        })
        .collect()
    } else {
      vec![(None, &mut state.instructions)]
    }
  } else {
    vec![]
  }
}

fn map_bytecode_id_to_sym_id(
  symbols: BTreeSet<crate::types::SymbolID>,
  g: &GrammarStore,
) -> BTreeMap<u32, crate::types::SymbolID> {
  symbols
    .into_iter()
    .chain(vec![SymbolID::EndOfFile].into_iter())
    .map(|s| {
      let sym = g.get_symbol(&s).unwrap();
      (sym.bytecode_id, s)
    })
    .collect::<BTreeMap<_, _>>()
}

fn all_symbols_are_a_single_codepoint(
  symbols: &BTreeSet<crate::types::SymbolID>,
  g: &GrammarStore,
) -> bool {
  symbols.iter().all(|s| match g.get_symbol(s) {
    Some(sym) => sym.cp_len <= 1,
    None => false,
  })
}

fn is_state_reference(i: &ASTNode) -> bool {
  matches!(
    i,
    ASTNode::PushGoto(..) | ASTNode::Goto(..) | ASTNode::PushExceptHandler(..)
  )
}

pub fn get_entry_state_names(g: &GrammarStore) -> BTreeSet<String> {
  g.get_exported_productions()
    .iter()
    .map(|ExportedProduction { guid_name, .. }| {
      guid_name.to_string() + "_enter"
    })
    .collect()
}
