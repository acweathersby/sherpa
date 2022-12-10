//! Functions for constructing and leveraging transition tables from
//! Hydrocarbon bytecode.

use crate::{compile::BytecodeOutput, types::*};
use std::collections::{btree_map, BTreeMap, VecDeque};

#[derive(Debug, Clone, Copy)]
struct TableCell<'a> {
  _state: u32,
  _debug_state_name: &'a String,
  _goto_state: u32,
  _input_byte: u32,
  _shift_length: u32,
}

pub(crate) fn _create_table<'a>(
  entry_state: u32,
  g: &GrammarStore,
  output: &'a BytecodeOutput,
) -> Option<()> {
  // let states = BTreeMap::new();
  let d: String = String::from("unknown");

  let mut state_offset_queue = VecDeque::<u32>::from_iter(vec![entry_state]);

  let bytecode = &output.bytecode;

  let mut table = BTreeMap::new();

  while let Some(state_address) = state_offset_queue.pop_front() {
    let state_address = state_address as usize;

    if let btree_map::Entry::Vacant(e) = table.entry(state_address) {
      let state_name = output.offset_to_state_name.get(&(state_address as u32)).unwrap();

      let state = output.state_data.get(state_name).unwrap();

      if !state.is_scanner() {
        return None;
      }

      let instruction = INSTRUCTION::from(&output.bytecode, state_address);

      use InstructionType::*;

      match instruction.to_type() {
        VECTOR_BRANCH | HASH_BRANCH => {
          let table_data = BranchTableData::from_bytecode(instruction, g, output).unwrap();

          let TableHeaderData { .. } = table_data.data;

          let mut index_map = table_data
            .branches
            .iter()
            .map(|(_, s)| TableCell {
              _state: state_address as u32,
              _shift_length: 0,
              _debug_state_name: output.offset_to_state_name.get(&(s.address as u32)).unwrap_or(&d),
              _goto_state: s.address as u32,
              _input_byte: s.value,
            })
            .collect::<Vec<_>>();

          // Grab a mapping between the value of the item and
          // the state that leads to the item.

          for cell in &mut index_map {
            // load the next state and see if it
            let index = cell._goto_state as usize;

            let instruction = INSTRUCTION::from(bytecode, index);

            if instruction.is_SHIFT() {
              let instruction = instruction.next(bytecode);
              cell._shift_length = 1;
              if instruction.is_GOTO() {
                let instruction2 = instruction.next(bytecode);
                if instruction2.is_GOTO() {
                  // replace the cells state with the goto value
                  cell._goto_state = instruction2.get_contents() & GOTO_STATE_ADDRESS_MASK;

                  state_offset_queue.push_back(cell._goto_state);
                } else {
                  // replace the cells state with the goto value
                  cell._goto_state = instruction.get_contents() & GOTO_STATE_ADDRESS_MASK;

                  state_offset_queue.push_back(cell._goto_state);
                }
              }
            } else if instruction.is_TOKEN() {
              cell._goto_state = state_address as u32;
            }
            println!(
              "{} {} {}",
              cell._goto_state,
              (bytecode[cell._goto_state as usize] & INSTRUCTION_HEADER_MASK) >> 28,
              INSTRUCTION::from(bytecode, cell._goto_state as usize).to_str()
            )
          }

          e.insert(index_map);

          // Each state in the index map may either be a skip or a state offset.
          // - if it is a skip then we simply point back to this state.
          // - if it points a to a legitimate state, then we shall pull up that
          // legit state, ascertain its action, and in the event the action leads
          // to another goto, follow that goto to a branch or end state
        }
        TOKEN => {
          e.insert(vec![TableCell {
            _state: state_address as u32,
            _debug_state_name: &d,
            _input_byte: 0,
            _goto_state: state_address as u32,
            _shift_length: 0,
          }]);
        }
        _ => {}
      }
    }
  }

  // Convert each row to an integer
  let state_offset_to_enumuration =
    table.keys().enumerate().map(|(i, o)| (*o, i + 1)).collect::<BTreeMap<_, _>>();

  let mut byte_major_table: BTreeMap<u32, Vec<&TableCell>> = BTreeMap::new();
  let mut keys = state_offset_to_enumuration.keys();

  let _min_state = *state_offset_to_enumuration.get(keys.next().unwrap()).unwrap();
  let max_state = *state_offset_to_enumuration.get(keys.last().unwrap()).unwrap();

  for state in &mut table {
    for cell in state.1 {
      if let Some(remapped_name) = state_offset_to_enumuration.get(&(cell._goto_state as usize)) {
        cell._goto_state = *remapped_name as u32;
      }

      if let Some(remapped_name) = state_offset_to_enumuration.get(&(cell._state as usize)) {
        cell._state = *remapped_name as u32;
      }
    }
  }
  for state in &table {
    for cell in state.1 {
      match byte_major_table.entry(cell._input_byte) {
        btree_map::Entry::Occupied(mut e) => {
          e.get_mut().push(&*cell);
        }
        btree_map::Entry::Vacant(e) => {
          e.insert(vec![&*cell]);
        }
      }
    }
  }

  print!("       ");
  for state in table.keys() {
    if let Some(remapped_name) = state_offset_to_enumuration.get(&state) {
      print!(" {: <7}", remapped_name);
    } else {
      print!(" {: <7}", state);
    }
  }

  print!("\n");

  for row in &byte_major_table {
    if *row.0 > 32 {
      print!(
        "{: >3} [{:_>1}] ",
        row.0,
        String::from_utf8(vec![*row.0 as u8]).unwrap_or("[]".to_string())
      );
    } else {
      print!("{: >3} [_] ", row.0,);
    }
    let mut base = 1;

    for cell in row.1 {
      while base < cell._state {
        print!("{: <8}", 1);
        base += 1;
      }
      print!("[{: >2}|{: >2}] ", cell._shift_length, cell._goto_state);
      base += 1;
    }

    while base <= (max_state as u32) {
      print!("{: <8}", 0);
      base += 1;
    }

    print!("\n")
  }

  //     let remapped_table = table
  // .into_iter()
  // .enumerate()
  // .map(|(i, (_, cells))| (i + 1, cells))
  // .collect::<BTreeMap<_, _>>();
  println!("{:#?}", byte_major_table);
  Some(())
}
/// Information on byte code branches
#[derive(Debug, Clone)]
pub(crate) struct BranchTableData {
  /// Stores an offset of the branch that is taken given
  /// a particular symbol, and the symbol that activates
  /// the branch.
  pub symbols:    BTreeMap<u32, Symbol>,
  pub data:       TableHeaderData,
  /// All branches defined within the branch table,
  /// keyed by their bytecode address or by the indexed
  /// of the branch's location in the table should the
  /// branch be a skip.
  pub branches:   BTreeMap<usize, BranchData>,
  pub table_type: TableType,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BranchData {
  // pub table_index:  usize,
  pub is_skipped: bool,
  pub address:    usize,
  /// The value of the branch's key, be it the token type,
  /// production id, byte, class, or codepoint
  pub value:      u32,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum TableType {
  Hash,
  Vector,
}

impl BranchTableData {
  pub fn from_bytecode(
    instruction: INSTRUCTION,
    _: &GrammarStore,
    output: &BytecodeOutput,
  ) -> Option<Self> {
    let address = instruction.get_address();
    if instruction.is_HASH_BRANCH() || instruction.is_VECTOR_BRANCH() {
      let data = TableHeaderData::from_bytecode(address, &output.bytecode);

      let TableHeaderData { input_type, table_length, table_meta, .. } = data;

      let branches = match instruction.to_type() {
        InstructionType::VECTOR_BRANCH => output.bytecode
          [(address + 4)..(address + 4 + table_length as usize)]
          .iter()
          .enumerate()
          .map(|(i, offset_delta)| {
            let discriminant = (i as u32) + table_meta;
            let address = (*offset_delta as usize) + address;
            let is_skipped = *offset_delta == 0xFFFF_FFFF;
            (if is_skipped { i } else { address }, BranchData {
              value: discriminant,
              address: if is_skipped { 0 } else { address },
              is_skipped,
            })
          })
          .collect::<BTreeMap<_, _>>(),
        InstructionType::HASH_BRANCH => output.bytecode
          [(address + 4)..(address + 4 + table_length as usize)]
          .iter()
          .enumerate()
          .map(|(i, cell)| {
            let discriminant = cell & 0x7FF;
            let offset_delta = (cell >> 11) & 0x7FF;
            let address = (offset_delta as usize) + address;
            let is_skipped = offset_delta == 0x7FF;
            (if is_skipped { i } else { address }, BranchData {
              value: discriminant,
              address: if is_skipped { 0 } else { address },
              is_skipped,
            })
          })
          .collect::<BTreeMap<_, _>>(),
        _ => BTreeMap::new(),
      };

      let symbol_lookup = &output.bytecode_id_to_symbol_lookup;

      let symbols = match input_type {
        INPUT_TYPE::T02_TOKEN => {
          let mut symbols = BTreeMap::new();

          for (_, branch) in &branches {
            if let Some(symbol) = symbol_lookup.get(&branch.value) {
              symbols.insert(branch.value, symbol.clone());
            } else {
              panic!("Missing symbol!")
            }
          }
          symbols
        }
        _ => BTreeMap::new(),
      };

      Some(BranchTableData {
        table_type: if instruction.is_HASH_BRANCH() { TableType::Hash } else { TableType::Vector },
        symbols,
        data,
        branches,
      })
    } else {
      None
    }
  }

  pub fn has_trivial_comparisons(&self) -> bool {
    
    self.branches.iter().all(|(_, b)| self.min_comparison_bytes(b).is_some())
  }

  /// Return the number of bytes required to compare
  /// the discriminant value with a byte array.
  /// `None` indicates the value is incapable of being directly
  /// compared with a byte array.
  pub fn min_comparison_bytes(&self, branch: &BranchData) -> Option<u32> {
    if let Some(symbol) = self.get_branch_symbol(branch) {
      match symbol.guid {
        SymbolID::GenericHorizontalTab
        | SymbolID::GenericSpace
        | SymbolID::GenericNewLine
        | SymbolID::DefinedIdentifier(_)
        | SymbolID::DefinedNumeric(_)
        | SymbolID::DefinedSymbol(_) => Some(symbol.byte_length),
        _ => None,
      }
    } else {
      None
    }
  }

  pub fn get_branch_symbol(&self, branch: &BranchData) -> Option<&Symbol> {
    if let Some(sym) = self.symbols.get(&branch.value) {
      Some(sym)
    } else {
      None
    }
  }
}
