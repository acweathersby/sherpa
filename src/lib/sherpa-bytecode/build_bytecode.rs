use sherpa_core::{proxy::*, *};
use sherpa_rust_runtime::types::{
  bytecode::{insert_op, Opcode as Op, *},
  BytecodeParserDB,
  TableHeaderData,
  Token,
};
use std::collections::VecDeque;

/// Compiles a bytecode parser database.
///
/// # Example
///
/// ```
/// # use sherpa_core::*;
/// # use std::path::PathBuf;
/// # use sherpa_bytecode::{compile_bytecode};
/// #
/// # fn main() -> SherpaResult<()> {
///
/// let parser = SherpaGrammarBuilder::new()
///   .add_source_from_string( "<> A > 'Hello' 'World' ", &PathBuf::default(), false)?
///   .build_db(&PathBuf::default())?
///   .build_parser(Default::default())?
///   .optimize(false)?;
///
/// let bytecode_db = compile_bytecode(&parser, true)?;
///
/// assert_eq!(bytecode_db.bytecode.len(), 148);
///
/// # SherpaResult::Ok(())
/// # }
/// ```
pub fn compile_bytecode<T: ParserStore>(store: &T, add_debug_symbols: bool) -> SherpaResult<BytecodeParserDB> {
  let states = store.get_states();
  let db = store.get_db();

  let mut state_name_to_proxy = OrderedMap::new();

  let mut pkg = BytecodeParserDB {
    bytecode: Array::from_iter(bytecode_header()),
    nonterm_name_to_id: Default::default(),
    ir_token_lookup: Default::default(),
    state_name_to_address: Default::default(),
    address_to_state_name: Default::default(),
    nonterm_id_to_address: Default::default(),
    state_to_token_ids_map: Default::default(),
    token_id_to_str: db.tokens().iter().map(|tok| (tok.tok_id.to_val(), tok.name.to_string(db.string_store()))).collect(),
  };

  for (name, state) in states {
    let state_address = pkg.bytecode.len() as u32;

    if let Some(scanner) = state.get_scanner() {
      pkg
        .state_to_token_ids_map
        .insert(state_address, scanner.symbols.iter().filter(|s| !s.is_skipped()).map(|s| s.tok().to_val()).collect());
    }

    pkg.state_name_to_address.insert(name.to_string(db.string_store()), state_address);

    build_state(db, state, &mut pkg, &mut state_name_to_proxy, add_debug_symbols)?;
  }

  let proxy_to_address = state_name_to_proxy
    .into_iter()
    .map(|(name, proxy_address)| {
      (proxy_address as u32, *pkg.state_name_to_address.get(&name.to_string(db.string_store())).unwrap())
    })
    .collect::<OrderedMap<_, _>>()
    .into_values()
    .collect();

  remap_goto_addresses(&mut pkg.bytecode, &proxy_to_address);

  for ep in db.entry_points().iter().filter(|i| store.get_config().EXPORT_ALL_NONTERMS || i.is_export) {
    pkg.nonterm_name_to_id.insert(ep.entry_name.to_string(db.string_store()), ep.nonterm_key.to_val());
    if let Some(address) = pkg.state_name_to_address.get(&ep.nonterm_entry_name.to_string(db.string_store())).cloned() {
      pkg.nonterm_id_to_address.insert(ep.nonterm_key.to_val(), address);
    }
  }

  SherpaResult::Ok(pkg)
}

/// Converts Goto location bookmarks to bytecode addresses.
fn remap_goto_addresses(bc: &mut Array<u8>, _goto_to_off: &Array<u32>) {
  let mut i = bytecode_header().len();

  while i < bc.len() {
    let instruction = bc[i];
    let op = instruction.into();
    i += match op {
      Op::HashBranch => {
        let i: Instruction = (bc.as_slice(), i).into();
        let TableHeaderData { scan_block_instruction: scanner_address, parse_block_address, .. } = i.into();
        let default_delta = parse_block_address - i.address();

        if scanner_address.address() != u32::MAX as usize {
          set_goto_address(bc, _goto_to_off, i.address() + 6);
        }

        default_delta
      }
      Op::Goto | Op::PushGoto | Op::PushExceptionHandler => {
        set_goto_address(bc, _goto_to_off, i + 2);
        op.len()
      }
      Op::Fork => {
        let instr: Instruction = (bc.as_slice(), i).into();
        let len = instr.iter().next_u16_le().unwrap() as usize;
        for off in 0..len {
          set_goto_address(bc, _goto_to_off, i + 3 + (off << 2));
        }
        3 + len + 4
      }
      Op::ByteSequence => Instruction::from((bc.as_slice(), i)).next().unwrap().address() - i,
      op => op.len(),
    }
  }
}

fn set_goto_address(bc: &mut Vec<u8>, _goto_to_off: &[u32], offset: usize) {
  let mut iter: ByteCodeIterator = (bc.as_slice(), offset).into();
  let val = iter.next_u32_le().unwrap();
  set_u32_le(bc, offset, _goto_to_off[val as usize]);
}

fn build_state<'db>(
  db: &'db ParserDatabase,
  state: &ParseState,
  pkg: &mut BytecodeParserDB,
  state_name_to_proxy: &mut OrderedMap<IString, usize>,
  add_debug_symbols: bool,
) -> SherpaResult<()> {
  let state = state.get_ast()?;
  let stmt = &state.statement;
  build_statement(db, stmt.as_ref(), pkg, state_name_to_proxy, add_debug_symbols)?;
  SherpaResult::Ok(())
}

fn build_statement<'db>(
  db: &'db ParserDatabase,
  stmt: &parser::Statement,
  pkg: &mut BytecodeParserDB,
  state_name_to_proxy: &mut OrderedMap<IString, usize>,
  add_debug_symbols: bool,
) -> SherpaResult<()> {
  let parser::Statement { branch, non_branch, transitive, pop } = stmt;

  if let Some(transitive) = transitive {
    insert_tok_debug(pkg, transitive.to_token(), add_debug_symbols);

    match transitive {
      parser::ASTNode::Shift(shift) => match shift.ptr_type.as_str() {
        "char" => {
          if shift.skip {
            unimplemented!("shift-skip char not implemented")
          } else {
            insert_op(&mut pkg.bytecode, Op::ShiftChar)
          }
        }
        "tok" => {
          if shift.skip {
            insert_op(&mut pkg.bytecode, Op::SkipToken)
          } else {
            insert_op(&mut pkg.bytecode, Op::ShiftToken)
          }
        }
        _ => unreachable!(),
      },
      parser::ASTNode::Peek(peek) => match peek.ptr_type.as_str() {
        "char" => {
          if peek.skip {
            unimplemented!("peek-skip char not implemented")
          } else {
            unimplemented!("peek char not implemented")
          }
        }
        "tok" => {
          if peek.skip {
            insert_op(&mut pkg.bytecode, Op::PeekSkipToken)
          } else {
            insert_op(&mut pkg.bytecode, Op::PeekToken)
          }
        }
        _ => unreachable!(),
      },
      parser::ASTNode::Reset(..) => insert_op(&mut pkg.bytecode, Op::PeekReset),
      _ => {
        unreachable!();
      }
    }
  }

  for non_branch in non_branch {
    insert_tok_debug(pkg, non_branch.to_token(), add_debug_symbols);

    match non_branch {
      parser::ASTNode::ReduceRaw(r) => {
        insert_op(&mut pkg.bytecode, Op::Reduce);
        insert_u32_le(&mut pkg.bytecode, r.nonterminal_id as u32);
        insert_u32_le(&mut pkg.bytecode, r.rule_id as u32);
        insert_u16_le(&mut pkg.bytecode, r.len as u16);
      }
      parser::ASTNode::SetTokenId(st) => {
        insert_op(&mut pkg.bytecode, Op::AssignToken);
        insert_u32_le(&mut pkg.bytecode, st.id);
      }
      parser::ASTNode::SetLine(_) => { /* ignored in bytecode parsers */ }
      _ => {
        unreachable!();
      }
    }
  }

  if let Some(pop) = pop {
    for _ in 0..(pop.count).max(1) {
      insert_op(&mut pkg.bytecode, Op::PopGoto)
    }
  }

  if let Some(branch) = branch {
    if add_debug_symbols && !matches!(branch, parser::ASTNode::Gotos(..)) {
      insert_tok_debug(pkg, branch.to_token(), add_debug_symbols);
    }

    match branch {
      parser::ASTNode::Pass(..) => insert_op(&mut pkg.bytecode, Op::Pass),
      parser::ASTNode::Fail(..) => insert_op(&mut pkg.bytecode, Op::Fail),
      parser::ASTNode::Accept(..) => insert_op(&mut pkg.bytecode, Op::Accept),
      parser::ASTNode::Gotos(gotos) => {
        for push in &gotos.pushes {
          insert_tok_debug(pkg, push.tok.clone(), add_debug_symbols);
          let proxy_address = get_proxy_address(push.name.to_token(), state_name_to_proxy);

          insert_op(&mut pkg.bytecode, Op::PushGoto);
          insert_u8(&mut pkg.bytecode, NORMAL_STATE_FLAG as u8);
          insert_u32_le(&mut pkg.bytecode, proxy_address);
        }

        if let Some(goto) = &gotos.goto {
          insert_tok_debug(pkg, goto.tok.clone(), add_debug_symbols);
          let proxy_address = get_proxy_address(goto.name.to_token(), state_name_to_proxy);

          insert_op(&mut pkg.bytecode, Op::Goto);
          insert_u8(&mut pkg.bytecode, NORMAL_STATE_FLAG as u8);
          insert_u32_le(&mut pkg.bytecode, proxy_address);
        }

        if let Some(fork) = &gotos.fork {
          insert_op(&mut pkg.bytecode, Op::Fork);
          insert_u16_le(&mut pkg.bytecode, fork.paths.len() as u16);

          for init in &fork.paths {
            insert_tok_debug(pkg, init.tok.clone(), add_debug_symbols);
            let proxy_address = get_proxy_address(init.name.to_token(), state_name_to_proxy);
            insert_u32_le(&mut pkg.bytecode, proxy_address);
          }
        }
      }
      matches => {
        build_match(db, matches, pkg, state_name_to_proxy, add_debug_symbols)?;
      }
    }
  }

  if let Some(last) = pkg.bytecode.last() {
    // Ensure the last op in this block is a sentinel type.
    match (*last).into() {
      Op::Pass
      | Op::Accept
      | Op::Fail
      | Op::Goto
      | Op::PeekSkipToken
      | Op::SkipToken
      | Op::SkipTokenScanless
      | Op::PeekSkipTokenScanless => {}
      _ => insert_op(&mut pkg.bytecode, Op::Pass),
    }
  }

  SherpaResult::Ok(())
}

fn get_proxy_address(name: IString, state_name_to_proxy: &mut OrderedMap<IString, usize>) -> u32 {
  let val = state_name_to_proxy.len();
  let proxy_address = (*state_name_to_proxy.entry(name).or_insert(val)) as u32;
  proxy_address
}

fn build_match<'db>(
  db: &'db ParserDatabase,
  matches: &parser::ASTNode,
  pkg: &mut BytecodeParserDB,
  state_name_to_proxy: &mut OrderedMap<IString, usize>,
  add_debug_symbols: bool,
) -> SherpaResult<()> {
  let mut default = None;
  let mut match_branches = Array::new();
  let mut scanner_address = u32::MAX;
  let input_type_key;

  match matches {
    parser::ASTNode::Matches(m) => {
      input_type_key = match m.mode.as_str() {
        MatchInputType::TOKEN_STR => {
          scanner_address = get_proxy_address(m.scanner.to_token(), state_name_to_proxy);
          MatchInputType::Token
        }
        s => MatchInputType::from(s),
      } as u32;
      for m in m.matches.iter().rev() {
        match m {
          parser::ASTNode::DefaultMatch(d) => {
            default = Some(d.statement.as_ref());
          }
          parser::ASTNode::IntMatch(im) => match_branches.push((&im.vals, im.statement.as_ref())),
          _ => {}
        }
      }
    }
    _ => {
      unreachable!();
    }
  };

  if match_branches.is_empty() {
    if let Some(stmt) = default {
      insert_op(&mut pkg.bytecode, Op::ReadCodepoint);
      build_statement(db, stmt, pkg, state_name_to_proxy, add_debug_symbols)?;
    } else {
      unreachable!("Match statement is empty")
    }
  } else {
    if input_type_key == MatchInputType::ByteSequence as u32 {
      debug_assert_eq!(match_branches.len(), 1, "Expected match statement _BYTE_SEQUENCE_  to have exactly 1 branch");

      let (bytes, stmt) = match_branches[0];
      debug_assert!(bytes.len() < (u16::MAX - 1) as usize, "Too many bytes need to be matched in _BYTE_SEQUENCE_");

      let base_len = 1 + 2 + 4 + bytes.len();

      insert_op(&mut pkg.bytecode, Op::ByteSequence);
      insert_u16_le(&mut pkg.bytecode, bytes.len() as u16);

      let mut stmt_bc = BytecodeParserDB::default();
      let mut default_bc = BytecodeParserDB::default();

      build_statement(db, stmt, &mut stmt_bc, state_name_to_proxy, add_debug_symbols)?;

      let default_offset = if let Some(default) = default {
        build_statement(db, default, &mut default_bc, state_name_to_proxy, add_debug_symbols)?;
        base_len + stmt_bc.bytecode.len()
      } else {
        0
      };

      insert_u32_le(&mut pkg.bytecode, default_offset as u32);

      for byte in bytes {
        let byte = *byte as u8;
        insert_u8(&mut pkg.bytecode, byte);
      }

      pkg.bytecode.extend(stmt_bc.bytecode);
      pkg.bytecode.extend(default_bc.bytecode);
    } else {
      let mut offset = 0;
      let mut val_offset_map = OrderedMap::new();
      let mut sub_bcs = Array::new();

      for (ids, stmt) in match_branches {
        for id in ids {
          val_offset_map.insert(*id as u32, offset);
        }

        let mut sub_bc = BytecodeParserDB::default();
        build_statement(db, stmt, &mut sub_bc, state_name_to_proxy, add_debug_symbols)?;
        offset += sub_bc.bytecode.len() as u32;
        sub_bcs.push(sub_bc);
      }

      let offset_lookup_table_length = val_offset_map.len() as u32;
      let instruction_field_start = 18 + offset_lookup_table_length * 4;
      let default_offset = offset + instruction_field_start;

      let mut pending_pairs =
        val_offset_map.clone().into_iter().map(|(k, v)| (k, v + instruction_field_start)).collect::<VecDeque<_>>();

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
          debug_assert!(
            offset <= 0x7FF,
            "Hash table offset overflow. Offsets [{}] overflow bounds by [{}]",
            offset,
            offset - 0x7FF
          );
          debug_assert!(val <= 0x7FF, "Hash table value overflow. Value [{}] overflow bounds by [{}]", val, val - 0x7FF);
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
            hash_entries[prev_node] =
              ((((i as i32 - prev_node as i32) + 512) as u32 & 0x3FF) << 22) | (hash_entries[prev_node] & ((1 << 22) - 1));
            // Add data for the new node.
            hash_entries[i] = ((val) & 0x7FF) | ((offset & 0x7FF) << 11) | (512 << 22);
            break;
          }
        }
      }

      insert_op(&mut pkg.bytecode, Op::HashBranch); // 1
      insert_u8(&mut pkg.bytecode, input_type_key as u8); // 2
      insert_u32_le(&mut pkg.bytecode, default_offset); // 6
      insert_u32_le(&mut pkg.bytecode, scanner_address); // 10
      insert_u32_le(&mut pkg.bytecode, offset_lookup_table_length); // 14
      insert_u32_le(&mut pkg.bytecode, mod_base); // 18

      for instruction in hash_entries {
        insert_u32_le(&mut pkg.bytecode, instruction)
      }

      for sub_bc in sub_bcs {
        let len = pkg.bytecode.len() as u32;

        pkg.bytecode.extend(sub_bc.bytecode);

        pkg.ir_token_lookup.extend(sub_bc.ir_token_lookup.into_iter().map(|(address, token)| (address + len, token)));
      }

      if let Some(stmt) = default {
        build_statement(db, stmt, pkg, state_name_to_proxy, add_debug_symbols)?;
      } else {
        insert_op(&mut pkg.bytecode, Op::Fail)
      }
    }
  }
  SherpaResult::Ok(())
}

fn insert_tok_debug(pkg: &mut BytecodeParserDB, tok: Token, add_debug_symbols: bool) {
  if !add_debug_symbols {
    return;
  }

  pkg.ir_token_lookup.insert(pkg.bytecode.len() as u32, tok);
}

const fn bytecode_header() -> [u8; 8] {
  [
    0,
    ('S' as u8) | 0x80,
    ('H' as u8) | 0x80,
    ('E' as u8) | 0x80,
    ('R' as u8) | 0x80,
    ('P' as u8) | 0x80,
    ('A' as u8) | 0x80,
    Op::Fail as u8,
  ]
}
