#![warn(clippy::borrowed_box)]

use crate::{
  grammar::compile::parser::sherpa::{
    ASTNode,
    Goto,
    Num,
    Reduce,
    SetProd,
    ShiftToken,
    TokenAssign,
    ASSERT,
    IR_STATE,
  },
  types::*,
};
use sherpa_runtime::types::bytecode::{insert_op, Opcode, Opcode as Op, *};
use std::{
  collections::{hash_map::Entry, BTreeMap, HashMap, VecDeque},
  vec,
};

pub(crate) fn build_byte_code_buffer(states: Vec<&IR_STATE>) -> (Vec<u8>, BTreeMap<String, u32>) {
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
    0,
    ('S' as u8) | 0x80,
    ('H' as u8) | 0x80,
    ('E' as u8) | 0x80,
    ('R' as u8) | 0x80,
    ('P' as u8) | 0x80,
    ('A' as u8) | 0x80,
    Op::Fail as u8,
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
      insert_op(&mut bc, Op::PushGoto);
      insert_u8(&mut bc, FAIL_STATE_FLAG as u8);
      insert_u32_le(&mut bc, fail_state_address as u32);
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

  remap_goto_addresses(&mut bc, &goto_bookmarks_to_offset);

  let state_name_to_offset = state_name_to_bookmark
    .into_iter()
    .map(|(name, bookmark)| (name, goto_bookmarks_to_offset[bookmark as usize]))
    .collect::<BTreeMap<_, _>>();

  (bc, state_name_to_offset)
}

/// Converts Goto location bookmarks to bytecode addresses.
fn remap_goto_addresses(bc: &mut Vec<u8>, _goto_to_off: &[u32]) {
  let mut i = "SHERPA".len();

  while i < bc.len() {
    let instruction = bc[i];
    let op = instruction.into();
    i += match op {
      Op::HashBranch | Op::VectorBranch => {
        let i: Instruction = (bc.as_slice(), i).into();
        let TableHeaderData {
          scan_block_instruction: scanner_address, parse_block_address, ..
        } = i.into();
        let default_delta = parse_block_address - i.address();

        if scanner_address.address() != 0 {
          set_goto_address(bc, _goto_to_off, i.address() + 6);
        }

        default_delta
      }
      Op::Goto | Op::PushGoto | Op::PushExceptionHandler => {
        set_goto_address(bc, _goto_to_off, i + 2);
        op.len()
      }
      op => op.len(),
    }
  }
}

fn set_goto_address(bc: &mut Vec<u8>, _goto_to_off: &[u32], offset: usize) {
  let mut iter: ByteCodeIterator = (bc.as_slice(), offset).into();
  let val = iter.next_u32_le().unwrap();
  set_u32_le(bc, offset, _goto_to_off[val as usize]);
}

pub fn compile_ir_state_to_bytecode(
  states: &Vec<ASTNode>,
  get_branch_selector: GetBranchSelector,
  state_to_bookmark: &HashMap<String, u32>,
  scanner_name: &str,
  state_name: &str,
) -> Vec<u8> {
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
) -> Vec<u8> {
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
    vec![Opcode::Fail as u8]
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
      .filter_map(|p| if p.mode == "TOKEN" { Some(p.as_ref()) } else { None })
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
  mut default: Vec<u8>,
  get_branch_selector: GetBranchSelector,
  state_to_bookmark: &HashMap<String, u32>,
  state_name: &str,
) -> Vec<u8> {
  if branches.is_empty() {
    return default;
  }

  let scanner_address = if input_type_key == InputType::T02_TOKEN {
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
    .map(|(i, assert)| (assert.ids.val as u32, i as u32))
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
  let mut existing_instructions = HashMap::<Vec<u8>, u32>::new();

  for branch in branches {
    let instructions =
      build_branchless_bytecode(&branch.instructions, state_to_bookmark, state_name);
    match existing_instructions.entry(instructions) {
      Entry::Occupied(e) => {
        let offset = e.get();
        let id = &branch.ids;
        val_offset_map.insert(id.val as u32, *offset as u32);
      }
      Entry::Vacant(e) => {
        let offset = branch_instructions_length;
        let instructions = e.key();
        branch_instructions_length += instructions.len() as u32;
        branch_instructions.push(instructions.clone());
        e.insert(offset);
        let id = &branch.ids;
        val_offset_map.insert(id.val as u32, offset as u32);
      }
    }
  }

  let mut o = vec![];
  let bc = &mut o;

  match get_branch_selector(
    &val_offset_map.keys().cloned().collect::<Vec<_>>(),
    max_span,
    &branch_instructions,
  ) {
    BranchSelector::Hash => {
      let offset_lookup_table_length = val_offset_map.len() as u32;

      let instruction_field_start = 18 + offset_lookup_table_length * 4;

      let default_offset = branch_instructions_length + instruction_field_start;

      let mut pending_pairs = val_offset_map
        .clone()
        .into_iter()
        .map(|(k, v)| (k, v + instruction_field_start))
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
      insert_op(bc, Op::HashBranch); // 1
      insert_u8(bc, input_type_key as u8); // 2
      insert_u32_le(bc, default_offset); // 6
      insert_u32_le(bc, scanner_address); // 10
      insert_u32_le(bc, offset_lookup_table_length); // 14
      insert_u32_le(bc, mod_base); // 18
      for instruction in hash_entries {
        insert_u32_le(bc, instruction)
      }
    }
    BranchSelector::Vector => {
      let mut values = val_offset_map.keys().peekable();
      let (start, end) = (**values.peek().unwrap(), *values.last().unwrap());
      let value_offset = start;
      let offset_lookup_table_length = val_offset_map.len() as u32;
      let instruction_field_start = 18 + offset_lookup_table_length * 4;
      let default_offset = branch_instructions_length + instruction_field_start;

      insert_op(bc, Op::VectorBranch);
      insert_u8(bc, input_type_key as u8);
      insert_u32_le(bc, default_offset);
      insert_u32_le(bc, scanner_address);
      insert_u32_le(bc, offset_lookup_table_length);
      insert_u32_le(bc, value_offset);

      for branch in start..=end {
        if let Some(offset) = val_offset_map.get(&branch) {
          insert_u32_le(bc, *offset + instruction_field_start);
        } else {
          insert_u32_le(bc, default_offset);
        }
      }
    }
  }

  o.append(&mut branch_instructions.into_iter().flatten().collect());

  o.append(&mut default);

  o
}

fn build_branchless_bytecode(
  instructions: &Vec<ASTNode>,
  state_name_to_bookmark: &HashMap<String, u32>,
  current_state_name: &str,
) -> Vec<u8> {
  let mut byte_code: Vec<u8> = vec![];
  let bc = &mut byte_code;

  // reverse gotos so jumps operate correctly in a stack structure.
  let gotos = instructions.iter().filter(|i| matches!(i, ASTNode::Goto(_))).rev();
  let non_gotos = instructions.iter().filter(|i| !matches!(i, ASTNode::Goto(_)));
  let mut active_prod = 0;
  for instruction in non_gotos.chain(gotos) {
    match instruction {
      ASTNode::TokenAssign(box TokenAssign { ids }) => {
        insert_op(bc, Op::AssignToken);
        insert_u32_le(bc, ids[0].val as u32);
      }
      ASTNode::ShiftScanner(..) => insert_op(bc, Op::ScanShift),
      ASTNode::ShiftScanToken(_) => insert_op(bc, Op::ShiftTokenScanless),
      ASTNode::ShiftToken(box ShiftToken { .. }) => insert_op(bc, Op::ShiftToken),
      ASTNode::Goto(box Goto { state }) => {
        let state_pointer_val = match (state.val == current_state_name)
          .then_some(state_name_to_bookmark.get(&(state.val.to_string() + "_internal")))
        {
          Some(Some(v)) => *v,
          _ => match state_name_to_bookmark.get(&state.val) {
            Some(v) => *v,
            None => panic!("State is undefined"),
          },
        };
        insert_op(bc, Op::PushGoto);
        insert_u8(bc, NORMAL_STATE_FLAG as u8);
        insert_u32_le(bc, state_pointer_val);
      }
      ASTNode::PeekScanToken(_) => insert_op(bc, Op::PeekTokenScanless),
      ASTNode::PeekToken(_) => insert_op(bc, Op::PeekToken),
      ASTNode::PeekReset(_) => insert_op(bc, Op::PeekReset),
      ASTNode::Skip(_) => insert_op(bc, Op::SkipToken),
      ASTNode::SkipScanToken(_) => insert_op(bc, Op::SkipTokenScanless),
      ASTNode::Pop(_) => insert_op(bc, Op::PopGoto),
      ASTNode::Pass(_) => insert_op(bc, Op::Pass),
      ASTNode::Fail(_) => insert_op(bc, Op::Fail),
      // TODO: Remove this kludge when production is set by the reduce ast type
      ASTNode::SetProd(box SetProd { id: box Num { val } }) => {
        active_prod = *val as u32;
      }
      ASTNode::Reduce(box Reduce { rule_id, len, .. }) => {
        insert_op(bc, Op::Reduce);
        insert_u32_le(bc, active_prod);
        insert_u32_le(bc, *rule_id as u32);
        insert_u16_le(bc, *len as u16);
      }

      _ => {}
    }
  }

  if let Some(last) = bc.last() {
    // Ensure the last op in this block is a sentinel type.
    match (*last).into() {
      Op::Pass
      | Op::Accept
      | Op::Fail
      | Op::Goto
      | Op::SkipPeekToken
      | Op::SkipToken
      | Op::SkipTokenScanless
      | Op::SkipPeekTokenScanless => {}
      _ => insert_op(bc, Op::Pass),
    }
  }

  byte_code
}
