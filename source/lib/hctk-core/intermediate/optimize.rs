//! The goal here is to reduce the number of states in the IR,
//! with the ultimate result being each state should perform at
//! least one "advancing" action (REDUCE, SHIFT, ACCEPT), and
//! preferably more than one. The focus is to remove states
//! that are comprised only of GOTO statements, folding these
//! into the respective states that reference them.
use std::collections::btree_map;
use std::collections::btree_map::OccupiedEntry;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::VecDeque;

use crate::bytecode::compile::build_byte_code_buffer;
use crate::bytecode::compile_bytecode;
use crate::debug::print_bytecode_states;
use crate::debug::print_ir_states;
use crate::debug::BytecodeGrammarLookups;
use crate::grammar;
use crate::grammar::data::ast::ASTNode;
use crate::grammar::data::ast::Fail;
use crate::grammar::data::ast::Goto;
use crate::grammar::data::ast::Num;
use crate::grammar::data::ast::ASSERT;
use crate::grammar::data::ast::AST_NUMBER;
use crate::grammar::data::ast::DEFAULT;
use crate::grammar::data::ast::HASH_NAME;
use crate::grammar::data::ast::IR_STATE;
use crate::types::GrammarStore;
use crate::types::IRState;

use crate::intermediate::state::compile_states;

/// Attempts to reduce the number of IR states through merging states, and reduce
/// and reduce bytecode complexity by transforming instructions where appropriate.
///
/// # Example
/// ```
/// # use hctk_core::debug::compile_test_grammar;
/// # use hctk_core::intermediate::state::compile_states;
/// # use hctk_core::intermediate::optimize::optimize_ir_states;
///
/// let g = &compile_test_grammar("<> A > \\h \\e \\l \\l \\o");
/// let mut states = compile_states(&g, 1);
///
/// let unoptimized_state_count = states.len() as f64;
///
/// let optimized_state_count = optimize_ir_states(states, &g).len() as f64;
///
/// println!(
///   "pre opt {} post opt {} reduced to {}% size",
///   unoptimized_state_count,
///   optimized_state_count,
///   100.0 * (optimized_state_count  / unoptimized_state_count)
/// );
///
/// assert!(unoptimized_state_count > optimized_state_count);
/// ```
pub fn optimize_ir_states(
  mut states: BTreeMap<String, Box<IRState>>,
  g: &GrammarStore,
) -> BTreeMap<String, Box<IRState>> {
  // Preform rounds -------------------------------------
  let entry_states: BTreeSet<String> = get_entry_states(g);
  let mut non_scanner_states = BTreeSet::new();

  loop {
    let mut goto_replacements = BTreeMap::new();
    let mut redundant_assert_replacements = BTreeMap::new();
    let mut pass_states = BTreeSet::new();
    let mut fail_states = BTreeSet::new();

    for state in states.values() {
      if let Ok(state) = &state.ast {
        // Goto instructions are always placed at the end of any
        // instruction sequence, so if the first node is a GOTO
        // then all subsequent nodes must also be GOTO.
        match (state.instructions.len(), &state.instructions[0], state.instructions.last()) {
          (1, ASTNode::Pass(_), _) => {
            pass_states.insert(state.id.clone());
          }
          (1, ASTNode::Fail(_), _) => {
            fail_states.insert(state.id.clone());
          }
          (2, ASTNode::SetProd(_), Some(&ASTNode::Reduce(_)))
          | (2, ASTNode::TokenAssign(_), Some(&ASTNode::SetProd(_)))
          | (1, ASTNode::SetProd(_), _)
          | (_, ASTNode::Goto(_), Some(ASTNode::Goto(_))) => {
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
      if goto_replacements.contains_key(&state.id) {
        continue;
      }

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
                    let (id, bc_type) = sym_id.shift_type(&g);
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
        // Replace the first goto if it points to a pure goto state.
        if let Some((index, goto)) = branch.iter().cloned().enumerate().find(|(_, i)| is_goto(i)) {
          if let ASTNode::Goto(box Goto { state, .. }) = &goto {
            if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &state {
              if let Some(instructions) = goto_replacements.get(val) {
                branch.splice(index..=index, instructions.iter().cloned());
                changes = true;
              } else if let Some((id, instructions)) = redundant_assert_replacements.get(val) {
                if *id == data.id && !data.peek && !matches!(branch[0], ASTNode::Shift(_)) {
                  branch.splice(index..=index, instructions.iter().cloned());
                  changes = true;
                }
              } else if let Some(_) = fail_states.get(val) {
                branch.clear();
                branch.push(ASTNode::Fail(Fail::new()));
              }
            }
          }
        }
        // Remove the last goto if it points to a pass state
        if let Some(goto) = branch.last() {
          if let ASTNode::Goto(box Goto { state, .. }) = &goto {
            if let ASTNode::HASH_NAME(box HASH_NAME { val, .. }) = &state {
              if let Some(instructions) = pass_states.get(val) {
                branch.remove(branch.len() - 1);
                changes = true;
              }
            }
          }
        }
      }
    }

    states = garbage_collect(states, &entry_states, &non_scanner_states);

    if !changes {
      break;
    }
  }
  states
}

fn garbage_collect(
  mut states: BTreeMap<String, Box<IRState>>,
  entry_states: &BTreeSet<String>,
  non_scanner_states: &BTreeSet<String>,
) -> BTreeMap<String, Box<IRState>> {
  // Create a structure to store the number of references
  // a givin state has
  let mut references = BTreeSet::from_iter(entry_states.iter().cloned());
  let mut trace_queue = VecDeque::from_iter(entry_states.iter().cloned());

  // Starting at the entry states, proceed to trace goto references
  // by inserting a referenced strings in the references stet
  while let Some(state_id) = trace_queue.pop_front() {
    if let Some(state) = states.get(&state_id) {
      if !non_scanner_states.contains(&state.get_name()) {
        if let Some(scanner_state_id) = state.get_scanner_state_name() {
          if references.insert(scanner_state_id.clone()) {
            trace_queue.push_back(scanner_state_id);
          }
        }
      }

      for branch in get_branches(state) {
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

  states.drain_filter(|s, _| !references.contains(s));

  states
}

/// Returns a vector of referenced instruction vectors
/// which may either either contain the root instruction vector, or the
/// the vectors of individual branches in the case of a branch state
fn get_branches<'a>(state: &'a IRState) -> Vec<&'a Vec<ASTNode>> {
  if let Ok(state) = &state.ast {
    if matches!(state.instructions[0], ASTNode::ASSERT(_) | ASTNode::DEFAULT(_)) {
      state
        .instructions
        .iter()
        .map(|i| match i {
          ASTNode::ASSERT(box ASSERT { instructions, .. }) => instructions,
          ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => instructions,
          _ => unreachable!("Expected only ASSERT and DEFAULT nodes in instruction vector."),
        })
        .collect()
    } else {
      vec![&state.instructions]
    }
  } else {
    vec![]
  }
}

struct BranchData {
  id:      u32,
  peek:    bool,
  default: bool,
}

/// Returns a vector of mutable referenced instruction vector
/// which may either contain the root instruction vector, or the
/// the vectors of individual branches in the case of a branch state
fn get_branches_mut<'a>(state: &'a mut IRState) -> Vec<(BranchData, &'a mut Vec<ASTNode>)> {
  if let Ok(state) = &mut state.ast {
    if matches!(state.instructions[0], ASTNode::ASSERT(_) | ASTNode::DEFAULT(_)) {
      state
        .instructions
        .iter_mut()
        .map(|i| match i {
          ASTNode::ASSERT(box ASSERT { ids, instructions, is_peek, .. }) => {
            (BranchData { id: get_branch_id(ids), peek: *is_peek, default: false }, instructions)
          }
          ASTNode::DEFAULT(box DEFAULT { instructions, .. }) => {
            (BranchData { id: 0, peek: false, default: true }, instructions)
          }
          _ => unreachable!("Expected only ASSERT and DEFAULT nodes in instruction vector."),
        })
        .collect()
    } else {
      vec![(BranchData { id: 0, peek: false, default: false }, &mut state.instructions)]
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
  g.exports.iter().map(|(name, ..)| g.get_production_guid_name(name).to_string()).collect()
}

#[test]
fn optimize_grammar() {
  let g = GrammarStore::from_str("
  
  @NAME hc_symbol

@IGNORE g:sp g:nl


<> annotated_symbol > 
        
        symbol^s [unordered tk:reference?^r \\? ?^o ]

            f:ast {{ t_AnnotatedSymbol, symbol:$s, is_optional:bool($o), reference:str($r), tok  }}
        
        | symbol


<> symbol >

        terminal

        | non_terminal

        | list

        | terminal_non_terminal

        | class


<> class >

        t:c: ( \\num | \\nl | \\sp | \\id | \\sym | \\any )
        
            f:ast { { t_Class, c_Symbol , c_Terminal, val:str($2),  tok } }
        

<> terminal_non_terminal >

        t:tk: non_terminal

             f:ast { { t_Production_Terminal , c_Symbol , c_Terminal, production:$2, tok } }


<> non_terminal > 

        tk:identifier_syms
        
             f:ast { { t_Production_Symbol , c_Symbol, name:str($1),   tok } }

        | tk:identifier_syms \\:: tk:identifier_syms
        
             f:ast { { t_Production_Import_Symbol , c_Symbol , module:str($1), name:str($3), tok } } 


<> list >

        symbol \\(+  terminal?  \\)
        
            f:ast { { t_List_Production, c_Symbol, terminal_symbol:$3, symbols:$1, tok } }

        | symbol \\(* terminal?  \\)
        
            f:ast {{ t_List_Production, c_Symbol, terminal_symbol:$3, symbols:$1, tok, optional:true }}


<> terminal > 
    
        ( g:sym | g:num )(+\\\" )

            f:ast { { t_Terminal , c_Symbol , c_Terminal, val:str($1),  tok } }

        | t:\\ tk:defined_symbol
    
            f:ast { { t_Terminal , c_Symbol , c_Terminal, val:str($2),  tok } } 


<> defined_symbol > 

        ( g:id | g:sym | g:num )(+)


<> reference > 

        t:^ tk:identifier_syms


<> identifier > 

        tk:identifier_syms 


<> identifier_syms >  

        identifier_syms g:id

        | identifier_syms \\_

        | identifier_syms \\-

        | identifier_syms g:num      

        | \\_ 

        | \\- 

        | g:id
  
  ").unwrap();

  let mut states = compile_states(&g, 10);
  let pre_opt_length = states.len();

  let state_refs = states.iter().collect::<Vec<_>>();

  // print_bytecode_states(&compile_bytecode(g, &mut states), Some(&BytecodeGrammarLookups::new(&g)));

  let mut states = optimize_ir_states(states, &g);
  let post_opt_length = states.len();

  // print_bytecode_states(&compile_bytecode(g, &mut states), Some(&BytecodeGrammarLookups::new(&g)));

  println!(
    "pre opt {} post opt {}. The optimized states are {:.2}% of original count",
    pre_opt_length,
    post_opt_length,
    100.0 * (post_opt_length as f64 / pre_opt_length as f64)
  );
  assert!(pre_opt_length > post_opt_length);
}
