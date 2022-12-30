//! The goal here is to reduce the number of states in the IR,
//! with the ultimate result being each state should perform at
//! least one "advancing" action (REDUCE, SHIFT, ACCEPT), and
//! preferably more than one. The focus is to remove states
//! that are comprised only of GOTO statements, folding these
//! into the respective states that reference them.
use crate::{
  grammar::data::ast::{ASTNode, Fail, Goto, Num, ASSERT, AST_NUMBER, DEFAULT, HASH_NAME},
  journal::{Journal, ReportType},
  types::{BranchType, ExportedProduction, GrammarStore, IRState},
};
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

/// Attempts to reduce the number of IR states through merging states, and reduce
/// and reduce bytecode complexity by transforming instructions where appropriate.
///
pub fn optimize_ir_states(
  j: &mut Journal,
  mut states: BTreeMap<String, Box<IRState>>,
) -> Vec<(String, Box<IRState>)> {
  let grammar = j.grammar().unwrap();
  let g = &grammar;

  j.set_active_report("Optimize States", ReportType::Optimize);
  j.report_mut().start_timer("Duration");

  let starting_states = states.len();

  // Setup config flags
  let inline_redundant_assertions = true;
  let remove_gotos_to_pass_states = true;

  // Preform rounds -------------------------------------
  let entry_states: BTreeSet<String> = get_entry_states(g);
  let mut non_scanner_states = BTreeSet::new();

  loop {
    // Maps a pure goto state id, in which  the state is only comprised of goto actions,
    // to a list of that state's actions.
    let mut pure_goto_replacements = BTreeMap::new();
    // States whose instructions can be inlined within the FIRST goto of a caller
    // production branch.
    let mut goto_replacements = BTreeMap::new();
    let mut redundant_assert_replacements = BTreeMap::new();
    let mut pass_states = BTreeSet::new();
    let mut fail_states = BTreeSet::new();
    let mut branch_references = HashMap::new();

    for state in states.values() {
      if inline_redundant_assertions {
        for (branch_data, instructions) in get_branches(state) {
          if let Some(branch_data) = branch_data {
            if !branch_data.default {
              branch_references.insert((state.get_name(), branch_data), instructions.clone());
            }
          }
        }
      }

      if let Ok(state) = &state.ast {
        // Goto instructions are always placed at the end of any
        // instruction sequence, so if the first node is a GOTO
        // then all subsequent nodes must also be GOTO.
        match (state.instructions.len(), &state.instructions[0], state.instructions.last()) {
          (1, ASTNode::Pass(_), _) => {
            if remove_gotos_to_pass_states {
              pass_states.insert(state.id.clone());
            }
          }
          (1, ASTNode::Fail(_), _) => {
            fail_states.insert(state.id.clone());
          }
          (_, ASTNode::Goto(_), Some(ASTNode::Goto(_))) => {
            pure_goto_replacements.insert(state.id.clone(), state.instructions.clone());
          }
          (2, ASTNode::SetProd(_), Some(&ASTNode::Reduce(_)))
          | (2, ASTNode::TokenAssign(_), Some(&ASTNode::SetProd(_)))
          | (2, ASTNode::TokenAssign(_), Some(&ASTNode::Pass(_)))
          | (1, ASTNode::SetProd(_), _) => {
            goto_replacements.insert(state.id.clone(), state.instructions.clone());
          }
          (1, ASTNode::ASSERT(box ASSERT { instructions, ids, .. }), _)
          | (_, ASTNode::ASSERT(box ASSERT { instructions, ids, .. }), Some(ASTNode::ASSERT(_)))
            if {
              state.instructions[1..].iter().all(|i| match i {
                ASTNode::ASSERT(box ASSERT { is_skip, .. }) => *is_skip,
                _ => false,
              })
            } =>
          {
            redundant_assert_replacements
              .insert(state.id.clone(), (get_branch_id(&ids), instructions.clone()));
          }
          _ => {}
        }
      }
    }

    let mut changes = false;

    // For each state, try to lower any state that is a pure
    // GOTO state into the respective reference states.
    for state in states.values_mut() {
      let state_name = state.get_name();
      let is_scanner = state.is_scanner();

      // Convert trivial scanner
      if !is_scanner {
        if let Some(symbols) = state.get_scanner_symbol_set() {
          if all_symbols_are_a_single_codepoint(&symbols, g) {
            let lookup = map_bytecode_id_to_sym_id(symbols, g);

            for instruction in &mut state.ast.as_mut().unwrap().instructions {
              match instruction {
                ASTNode::ASSERT(box assert) => match assert.ids[0].clone() {
                  ASTNode::AST_NUMBER(box AST_NUMBER { value, .. }) => {
                    let sym_id = *lookup.get(&(value as u32)).unwrap();
                    let (id, bc_type) = sym_id.shift_info(&g);
                    assert.mode = bc_type.to_string();
                    assert.ids = vec![ASTNode::AST_NUMBER(AST_NUMBER::new(id as f64))];
                  }
                  _ => {}
                },
                _ => {}
              }
            }
            non_scanner_states.insert(state.get_name());
          }
        }
      }

      for (data, branch) in get_branches_mut(state.as_mut()) {
        'outerloop: loop {
          let mut goto_count = 0;
          for (index, goto) in
            branch.iter().cloned().enumerate().filter(|(_, i)| is_goto(i)).collect::<Vec<_>>()
          {
            if let ASTNode::Goto(box Goto { state, .. }) = &goto {
              if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &state {
                match (
                  goto_count,
                  goto_replacements.get(val),
                  pure_goto_replacements.get(val),
                  fail_states.get(val),
                ) {
                  (0, Some(instructions), ..) => {
                    branch.splice(index..=index, instructions.iter().cloned());
                    changes = true;
                    continue 'outerloop;
                  }
                  // Replace goto if it points to a pure goto state.
                  (_, _, Some(instructions), ..) => {
                    branch.splice(index..=index, instructions.iter().cloned());
                    changes = true;
                    continue 'outerloop;
                  }
                  (_, _, _, Some(_)) => {
                    branch.clear();
                    branch.push(ASTNode::Fail(Fail::new()));
                    break;
                  }
                  _ => {}
                }
              }
            }
            goto_count += 1;
          }
          break;
        }

        match data {
          Some(data) => {
            if let Some((index, goto)) =
              branch.iter().cloned().enumerate().find(|(_, i)| is_goto(i))
            {
              if let ASTNode::Goto(box Goto { state, .. }) = &goto {
                if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &state {
                  if let Some((id, instructions)) = redundant_assert_replacements.get(val) {
                    if *id == data.id && !data.peek && !matches!(branch[0], ASTNode::Shift(_)) {
                      branch.splice(index..=index, instructions.iter().cloned());
                      changes = true;
                    }
                  }
                }
              }

              if inline_redundant_assertions {
                // If a branch of a branching state contains no transitive action, namely SHIFT,
                // and the first action within that branch is a goto to a state that is itself
                // a branching, and which contains a branch that matches the same assert type & symbol
                // of the originating branch in the first state, then the contents of the second
                // branch should be merged into the first branch, replacing the goto statement
                // in the first branch.

                match branch.first() {
                  Some(ASTNode::Goto(box Goto { state: goto_state, .. })) => {
                    if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &goto_state {
                      // Grab the other branch. Make sure it's not a circular reference.
                      if *val != state_name {
                        if let Some(instructions) = branch_references.get(&(val.clone(), data)) {
                          branch.splice(0..=0, instructions.iter().cloned());
                          changes = true;
                        }
                      }
                    }
                  }
                  _ => {}
                }
              }
            }
          }
          _ => {}
        }

        if remove_gotos_to_pass_states {
          // Remove the last goto if it points to a pass state
          if let Some(goto) = branch.last() {
            if let ASTNode::Goto(box Goto { state, .. }) = &goto {
              if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &state {
                if pass_states.contains(val) {
                  branch.remove(branch.len() - 1);
                  changes = true;
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

  let result: Vec<(String, Box<IRState>)> =
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

/// Remove unreferenced states, when tracking linkage from the root entry states,
/// and reorders states in an attempt to cluster closely associated states.
fn garbage_collect<T>(
  j: &mut Journal,
  mut states: BTreeMap<String, Box<IRState>>,
  entry_states: &BTreeSet<String>,
  non_scanner_states: &BTreeSet<String>,
) -> T
where
  T: FromIterator<(String, Box<IRState>)>,
{
  let mut reg_states = Vec::with_capacity(states.len());
  let mut scanner_states = Vec::with_capacity(states.len());
  let mut trace_queue = VecDeque::with_capacity(states.len());

  let mut references = BTreeSet::from_iter(entry_states.iter().cloned());

  trace_queue.append(&mut entry_states.iter().cloned().collect());

  // Starting at the entry states, proceed to trace goto references
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
        for goto in branch.iter().filter(|i| is_goto(*i)) {
          if let ASTNode::Goto(box Goto { state, .. }) = &goto {
            if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &state {
              if references.insert(val.clone()) {
                trace_queue.push_back(val.clone());
              }
            }
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
  /// The type of the discriminator symbol. This is the empty string in the case
  /// of a default branch.
  id_type: BranchType,
  /// Whether this branch evaluates and automatically increments the peek cursor.
  peek:    bool,
  /// Whether this branch is a default action. There should only be one of these
  /// per branching state.
  default: bool,
}

/// Returns a vector of referenced instruction vectors
/// which may either either contain the root instruction vector, or the
/// the vectors of individual branches in the case of a branch state
pub(crate) fn get_branches<'a>(state: &'a IRState) -> Vec<(Option<BranchData>, &'a Vec<ASTNode>)> {
  if let Ok(state) = &state.ast {
    if matches!(state.instructions[0], ASTNode::ASSERT(_) | ASTNode::DEFAULT(_)) {
      state
        .instructions
        .iter()
        .map(|i| match i {
          ASTNode::ASSERT(box ASSERT { ids, instructions, is_peek, mode, .. }) => (
            Some(BranchData {
              id:      get_branch_id(ids),
              peek:    *is_peek,
              default: false,
              id_type: BranchType::from(mode.clone()),
            }),
            instructions,
          ),
          ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => (
            Some(BranchData {
              id:      0,
              peek:    false,
              default: true,
              id_type: BranchType::UNKNOWN,
            }),
            instructions,
          ),
          _ => unreachable!("Expected only ASSERT and DEFAULT nodes in instruction vector."),
        })
        .collect()
    } else {
      vec![(None, &state.instructions)]
    }
  } else {
    vec![]
  }
}

/// Returns a vector of mutable referenced instruction vector
/// which may either contain the root instruction vector, or the
/// the vectors of individual branches in the case of a branch state
fn get_branches_mut<'a>(state: &'a mut IRState) -> Vec<(Option<BranchData>, &'a mut Vec<ASTNode>)> {
  if let Ok(state) = &mut state.ast {
    if matches!(state.instructions[0], ASTNode::ASSERT(_) | ASTNode::DEFAULT(_)) {
      state
        .instructions
        .iter_mut()
        .map(|i| match i {
          ASTNode::ASSERT(box ASSERT { ids, instructions, is_peek, mode, .. }) => (
            Some(BranchData {
              id:      get_branch_id(ids),
              peek:    *is_peek,
              default: false,
              id_type: BranchType::from(mode.clone()),
            }),
            instructions,
          ),
          ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => (
            Some(BranchData {
              id:      0,
              peek:    false,
              default: true,
              id_type: BranchType::UNKNOWN,
            }),
            instructions,
          ),
          _ => unreachable!("Expected only ASSERT and DEFAULT nodes in instruction vector."),
        })
        .collect()
    } else {
      vec![(None, &mut state.instructions)]
    }
  } else {
    vec![]
  }
}

fn get_branch_id(ids: &Vec<ASTNode>) -> u32 {
  match ids[0] {
    ASTNode::Num(box Num { val, .. }) => val as u32,
    _ => unreachable!("Expect only one numeric value per branch"),
  }
}

fn map_bytecode_id_to_sym_id(
  symbols: BTreeSet<crate::types::SymbolID>,
  g: &GrammarStore,
) -> BTreeMap<u32, crate::types::SymbolID> {
  symbols
    .into_iter()
    .map(|s| {
      let sym = g.symbols.get(&s).unwrap();
      (sym.bytecode_id, s)
    })
    .collect::<BTreeMap<_, _>>()
}

fn all_symbols_are_a_single_codepoint(
  symbols: &BTreeSet<crate::types::SymbolID>,
  g: &GrammarStore,
) -> bool {
  symbols.iter().all(|s| match g.symbols.get(s) {
    Some(sym) => sym.cp_len == 1,
    None => false,
  })
}

fn is_goto(i: &ASTNode) -> bool {
  matches!(i, ASTNode::Goto(..))
}

fn get_entry_states(g: &GrammarStore) -> BTreeSet<String> {
  g.get_exported_productions()
    .iter()
    .map(|ExportedProduction { guid_name, .. }| guid_name.to_string())
    .collect()
}
