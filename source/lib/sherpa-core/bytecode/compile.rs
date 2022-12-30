#![warn(clippy::borrowed_box)]
use crate::{
  grammar::data::ast::{
    ASTNode,
    ForkTo,
    Goto,
    NotInScope,
    Num,
    Reduce,
    ScanUntil,
    SetProd,
    SetScope,
    Shift,
    TokenAssign,
    ASSERT,
    HASH_NAME,
    IR_STATE,
  },
  types::*,
};
use std::{
  collections::{hash_map::Entry, BTreeMap, HashMap, VecDeque},
  vec,
};

pub(crate) fn build_byte_code_buffer(states: Vec<&IR_STATE>) -> (Vec<u32>, BTreeMap<String, u32>) {
  let states_iter = states
    .iter()
    .flat_map(|s| {
      if s.fail.is_some() {
        vec![(s, s.id.clone(), true), (s, s.id.clone() + "_internal", false)]
      } else {
        vec![(s, s.id.clone(), false)]
      }
    })
    .enumerate();

  let mut goto_bookmarks_to_offset = states_iter.clone().map(|_| 0).collect::<Vec<_>>();

  let state_name_to_bookmark =
    states_iter.clone().map(|(i, (_, name, _))| (name, i as u32)).collect::<HashMap<_, _>>();

  let mut bc = vec![
    INSTRUCTION::I15_FALL_THROUGH,
    INSTRUCTION::I00_PASS,
    INSTRUCTION::I15_FAIL,
    INSTRUCTION::I08_EAT_CRUMBS,
    NORMAL_STATE_FLAG,
    INSTRUCTION::I00_PASS,
  ];

  for (i, (state, name, is_fail)) in states_iter {
    if is_fail {
      let fail_state = state.fail.as_deref().unwrap();
      let fail_state_address = bc.len();

      let mut fail_addition = compile_ir_state_to_bytecode(
        &fail_state.instructions,
        default_get_branch_selector,
        &state_name_to_bookmark,
        &state.scanner,
        &name,
      );

      bc.append(&mut fail_addition);
      goto_bookmarks_to_offset[i as usize] = bc.len() as u32;
      bc.push(INSTRUCTION::I02_GOTO | FAIL_STATE_FLAG | fail_state_address as u32);
    } else {
      if let Some(_) = &state.fail {
      } else {
        goto_bookmarks_to_offset[i as usize] = bc.len() as u32;
      }

      let mut addition = compile_ir_state_to_bytecode(
        &state.instructions,
        default_get_branch_selector,
        &state_name_to_bookmark,
        &state.scanner,
        &state.id,
      );
      goto_bookmarks_to_offset[i as usize] = bc.len() as u32;
      bc.append(&mut addition);
    }
  }

  patch_goto_offsets(&mut bc, &goto_bookmarks_to_offset);

  let state_name_to_offset = state_name_to_bookmark
    .into_iter()
    .map(|(name, bookmark)| (name, goto_bookmarks_to_offset[bookmark as usize]))
    .collect::<BTreeMap<_, _>>();

  (bc, state_name_to_offset)
}

/// Converts Goto location bookmarks to bytecode offsets.
fn patch_goto_offsets(bc: &mut Vec<u32>, goto_to_off: &[u32]) {
  use INSTRUCTION as I;

  let mut i = 0;

  while i < bc.len() {
    let instruction = bc[i];
    i += match instruction & 0xF000_0000 {
      I::I02_GOTO => {
        let bc_i = instruction & STATE_ADDRESS_MASK;
        let state_header = instruction & (!STATE_ADDRESS_MASK);
        if (state_header & FAIL_STATE_FLAG) == 0 {
          bc[i] = state_header | goto_to_off[bc_i as usize];
        }
        1
      }
      I::I06_FORK_TO => 1,
      I::I09_VECTOR_BRANCH | I::I10_HASH_BRANCH => {
        let table_length = bc[i + 2] >> 16 & 0xFFFF;

        let pointer = bc[i + 1];

        if pointer != 0 {
          bc[i + 1] = goto_to_off[pointer as usize];
        }

        table_length as usize + 4
      }
      _ => 1,
    }
  }
}

pub fn compile_ir_state_to_bytecode(
  states: &Vec<ASTNode>,
  get_branch_selector: GetBranchSelector,
  state_to_bookmark: &HashMap<String, u32>,
  scanner_name: &str,
  state_name: &str,
) -> Vec<u32> {
  // Determine if we are dealing with a branch state or a single line
  // state. Branch states will always have more than one assert
  // statement.

  if is_branch_state(states) {
    // We are dealing with a branching state

    // For each branch we compile new vectors separately
    // Then we derive offset for each branch, build a suitable
    // storage structure (either HASH or JUMP) and combine all
    // that data into a single file

    // We first order the statements into blocks based on
    // on the assertion type. The order is
    // 1. Production
    // 2. Token
    // 3. Byte
    // 4. Class
    // 5. CodePoint
    build_branching_bytecode(
      states,
      get_branch_selector,
      scanner_name,
      state_to_bookmark,
      state_name,
    )
  } else {
    // We are dealing with a standard non-branching state
    build_branchless_bytecode(states, state_to_bookmark, state_name)
  }
}

fn is_branch_state(instr: &Vec<ASTNode>) -> bool {
  instr.iter().all(|i| matches!(i, ASTNode::ASSERT(_) | ASTNode::DEFAULT(_)))
}

fn build_branching_bytecode(
  instr: &[ASTNode],
  get_branch_selector: GetBranchSelector,
  scanner_name: &str,
  state_to_bookmark: &HashMap<String, u32>,
  state_name: &str,
) -> Vec<u32> {
  let branches = instr
    .iter()
    .filter_map(|n| match n {
      ASTNode::ASSERT(assert) => Some(assert),
      _ => None,
    })
    .collect::<Vec<_>>();

  let default_branches = instr
    .iter()
    .filter_map(|n| match n {
      ASTNode::DEFAULT(default) => Some(default),
      _ => None,
    })
    .collect::<Vec<_>>();

  if default_branches.len() > 1 {
    panic!("Too many default branches found in state!")
  }

  // Extract the default branch if it exists.
  let o = if !default_branches.is_empty() {
    build_branchless_bytecode(&default_branches[0].instructions, state_to_bookmark, state_name)
  } else {
    vec![INSTRUCTION::I15_FAIL]
  };

  let o = make_table(
    &branches
      .iter()
      .cloned()
      .filter_map(|p| if p.mode == "CODEPOINT" { Some(p.as_ref()) } else { None })
      .collect::<Vec<_>>(),
    InputType::T04_CODEPOINT,
    &String::new(),
    o,
    get_branch_selector,
    state_to_bookmark,
    state_name,
  );

  let o = make_table(
    &branches
      .iter()
      .cloned()
      .filter_map(|p| if p.mode == "CLASS" { Some(p.as_ref()) } else { None })
      .collect::<Vec<_>>(),
    InputType::T03_CLASS,
    &String::new(),
    o,
    get_branch_selector,
    state_to_bookmark,
    state_name,
  );

  let o = make_table(
    &branches
      .iter()
      .cloned()
      .filter_map(|p| if p.mode == "BYTE" { Some(p.as_ref()) } else { None })
      .collect::<Vec<_>>(),
    InputType::T05_BYTE,
    &String::new(),
    o,
    get_branch_selector,
    state_to_bookmark,
    state_name,
  );

  let o = make_table(
    &branches
      .iter()
      .cloned()
      .filter_map(|p| if p.mode == "TOKEN" || p.is_skip { Some(p.as_ref()) } else { None })
      .collect::<Vec<_>>(),
    InputType::T02_TOKEN,
    scanner_name,
    o,
    get_branch_selector,
    state_to_bookmark,
    state_name,
  );

  let o = make_table(
    &branches
      .iter()
      .cloned()
      .filter_map(|p| if p.mode == "PRODUCTION" { Some(p.as_ref()) } else { None })
      .collect::<Vec<_>>(),
    InputType::T01_PRODUCTION,
    &String::new(),
    o,
    get_branch_selector,
    state_to_bookmark,
    state_name,
  );

  o
}

fn make_table(
  branches: &[&ASSERT],
  input_type_key: u32,
  scanner_name: &str,
  mut default: Vec<u32>,
  get_branch_selector: GetBranchSelector,
  state_to_bookmark: &HashMap<String, u32>,
  state_name: &str,
) -> Vec<u32> {
  if branches.is_empty() {
    return default;
  }

  use INSTRUCTION as I;

  let lexer_type: u32 = if branches[0].is_peek { LexerType::PEEK } else { LexerType::ASSERT };

  let scanner_pointer = if input_type_key == InputType::T02_TOKEN {
    if scanner_name.is_empty() {
      panic!("Scanner name should not be empty! {}", state_name);
    }

    if let Some(bookmark) = state_to_bookmark.get(scanner_name) {
      *bookmark
    } else {
      0
    }
  } else {
    0
  };

  let mut val_offset_map = branches
    .iter()
    .enumerate()
    .flat_map(|(i, assert)| {
      assert.ids.iter().filter_map(move |node| match node {
        ASTNode::Num(box Num { val }) => Some((*val as u32, i as u32)),
        _ => None,
      })
    })
    .collect::<BTreeMap<_, _>>();

  let max_span = {
    let mut val = 0;
    let mut prev = *val_offset_map.first_key_value().unwrap().0;
    for id in val_offset_map.keys().cloned() {
      val = u32::max(id - prev, val);
      prev = id;
    }
    val
  };

  let mut branch_instructions = vec![];
  let mut branch_instructions_length = 0;
  let mut existing_instructions = HashMap::<Vec<u32>, u32>::new();

  for branch in branches {
    if branch.is_skip {
      for id in &branch.ids {
        if let ASTNode::Num(box Num { val }) = id {
          val_offset_map.insert(*val as u32, 0xFFFF_FFFF);
        }
      }
    } else {
      let instructions =
        build_branchless_bytecode(&branch.instructions, state_to_bookmark, state_name);
      match existing_instructions.entry(instructions) {
        Entry::Occupied(e) => {
          let offset = e.get();
          for id in &branch.ids {
            if let ASTNode::Num(box Num { val }) = id {
              val_offset_map.insert(*val as u32, *offset as u32);
            }
          }
        }
        Entry::Vacant(e) => {
          let offset = branch_instructions_length;
          let instructions = e.key();
          branch_instructions_length += instructions.len() as u32;
          branch_instructions.push(instructions.clone());
          e.insert(offset);
          for id in &branch.ids {
            if let ASTNode::Num(box Num { val }) = id {
              val_offset_map.insert(*val as u32, offset as u32);
            }
          }
        }
      }
    }
  }

  let mut o = vec![];

  match get_branch_selector(
    &val_offset_map.keys().cloned().collect::<Vec<_>>(),
    max_span,
    &branch_instructions,
  ) {
    BranchSelector::Hash => {
      let offset_lookup_table_length = val_offset_map.len() as u32;

      let instruction_field_start = 4 + offset_lookup_table_length;

      let default_offset = branch_instructions_length + instruction_field_start;

      let mut pending_pairs = val_offset_map
        .clone()
        .into_iter()
        .map(|(k, v)| (k, if v == 0xFFFF_FFFF { 0x7FF } else { v + instruction_field_start }))
        .collect::<VecDeque<_>>();

      let mod_base = f64::log2(val_offset_map.len() as f64) as u32;

      let mod_mask = (1 << mod_base) - 1;

      let mut hash_entries = (0..pending_pairs.len()).into_iter().map(|_| 0).collect::<Vec<_>>();

      let mut leftover_pairs = vec![];

      // Distribute keys-values with unique hashes into hash table
      // slots.

      while let Some(pair) = pending_pairs.pop_front() {
        let (val, offset) = pair;
        let hash_index = (val & mod_mask) as usize;
        if hash_entries[hash_index] == 0 {
          hash_entries[hash_index] = (val & 0x7FF) | ((offset & 0x7FF) << 11) | (512 << 22);
        } else {
          leftover_pairs.push(pair);
        }
      }

      // What remains are hash collisions. We use simple linear
      // probing to find the next available slot, and
      // attach it to the probing chain using a signed
      // delta index.
      for (val, offset) in leftover_pairs {
        let mut pointer;
        let mut prev_node = (val & mod_mask) as usize;

        loop {
          pointer = (((hash_entries[prev_node] >> 22) & 0x3FF) as i32) - 512;

          if pointer == 0 {
            break;
          } else {
            prev_node = (prev_node as i32 + pointer as i32) as usize;
          }
        }

        for i in 0..hash_entries.len() {
          if hash_entries[i] == 0 {
            // Update the previous node in the chain with the
            // diff pointer to the new node.
            hash_entries[prev_node] = ((((i as i32 - prev_node as i32) + 512) as u32 & 0x3FF)
              << 22)
              | (hash_entries[prev_node] & ((1 << 22) - 1));
            // Add data for the new node.
            hash_entries[i] = ((val) & 0x7FF) | ((offset & 0x7FF) << 11) | (512 << 22);

            break;
          }
        }
      }

      // First word header
      o.push(I::I10_HASH_BRANCH | (input_type_key << 22) | (lexer_type << 26));

      // Second word header
      o.push(scanner_pointer);

      // Third word header
      o.push((offset_lookup_table_length << 16) | mod_base);

      o.push(default_offset);

      o.append(&mut hash_entries);
    }
    BranchSelector::Vector => {
      let mut values = val_offset_map.keys().peekable();
      let (start, end) = (**values.peek().unwrap(), *values.last().unwrap());
      let value_offset = start;
      let offset_lookup_table_length = val_offset_map.len() as u32;
      let instruction_field_start = 4 + offset_lookup_table_length;
      let default_offset = branch_instructions_length + instruction_field_start;

      // First word header
      o.push(I::I09_VECTOR_BRANCH | (input_type_key << 22) | (lexer_type << 26));

      // Second word header
      o.push(scanner_pointer);

      // Third word header
      o.push((offset_lookup_table_length << 16) | value_offset);

      // Default Location
      o.push(default_offset);

      for branch in start..=end {
        if let Some(offset) = val_offset_map.get(&branch) {
          if *offset == 0xFFFF_FFFF {
            o.push(*offset);
          } else {
            o.push(*offset + instruction_field_start);
          }
        } else {
          o.push(default_offset)
        }
      }
    }
  }

  o.append(&mut branch_instructions.into_iter().flatten().collect());

  o.append(&mut default);

  o
}

fn is_goto(state_name: &str) -> bool {
  state_name.ends_with("_goto")
}

fn build_branchless_bytecode(
  instructions: &Vec<ASTNode>,
  state_name_to_bookmark: &HashMap<String, u32>,
  current_state_name: &str,
) -> Vec<u32> {
  let mut byte_code: Vec<u32> = vec![];
  use INSTRUCTION as I;

  // reverse gotos so jumps operate correctly in a stack structure.
  let gotos = instructions.iter().filter(|i| matches!(i, ASTNode::Goto(_))).rev();
  let non_gotos = instructions.iter().filter(|i| !matches!(i, ASTNode::Goto(_)));

  for instruction in non_gotos.chain(gotos) {
    match instruction {
      ASTNode::TokenAssign(box TokenAssign { ids }) => {
        if let ASTNode::Num(id) = &ids[0] {
          byte_code.push(I::I05_TOKEN_ASSIGN | ((id.val as u32) & 0x00FF_FFFF))
        }
      }
      ASTNode::Shift(box Shift { EMPTY }) => byte_code.push(I::I01_SHIFT | *EMPTY as u32),
      ASTNode::Goto(box Goto { state }) => match state {
        ASTNode::HASH_NAME(box HASH_NAME { val }) => {
          let state_pointer_val = match (val == current_state_name)
            .then_some(state_name_to_bookmark.get(&(val.to_string() + "_internal")))
          {
            Some(Some(v)) => *v,
            _ => match state_name_to_bookmark.get(val) {
              Some(v) => *v,
              None => 0,
            },
          };
          byte_code.push(I::I02_GOTO | NORMAL_STATE_FLAG | state_pointer_val);
        }
        _ => {
          panic!("Invalid state type in goto instruction");
        }
      },
      ASTNode::ScanUntil(box ScanUntil { .. }) => {}
      ASTNode::ForkTo(box ForkTo { states, production_id }) => {
        byte_code.push(I::I06_FORK_TO | ((states.len() << 16) as u32) | (production_id.val as u32));
        for state in states {
          if let ASTNode::HASH_NAME(box HASH_NAME { val }) = state {
            let state_pointer_val =
              if let Some(v) = state_name_to_bookmark.get(val) { *v } else { 0 };
            byte_code.push(I::I02_GOTO | NORMAL_STATE_FLAG | state_pointer_val);
          } else {
            panic!("Invalid state type in goto instruction");
          }
        }
      }
      ASTNode::Skip(_) => {}
      ASTNode::Pass(_) => byte_code.push(I::I00_PASS),
      ASTNode::Fail(_) => byte_code.push(I::I15_FAIL),
      ASTNode::NotInScope(box NotInScope { .. }) => {}
      ASTNode::SetScope(box SetScope { .. }) => {}
      ASTNode::SetProd(box SetProd { id: ASTNode::Num(box Num { val }) }) => {
        byte_code.push(I::I03_SET_PROD | (*val as u32 & INSTRUCTION_CONTENT_MASK))
      }
      ASTNode::Reduce(box Reduce { body_id: rule_id, len, .. }) => byte_code.push(
        I::I04_REDUCE
          | if *len as u32 == IR_REDUCE_NUMERIC_LEN_ID {
            0xFFFF0000 & INSTRUCTION_CONTENT_MASK
          } else {
            (*len as u32) << 16
          }
          | (*rule_id as u32),
      ),

      _ => {}
    }
  }

  if let Some(last) = byte_code.last() {
    if *last != I::I00_PASS && *last != I::I15_FAIL {
      byte_code.push(I::I00_PASS);
    }
  }

  byte_code
}
