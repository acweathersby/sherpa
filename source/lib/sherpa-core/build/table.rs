//! Functions for constructing and leveraging transition tables from
//! Hydrocarbon bytecode.

use crate::{compile::BytecodeOutput, types::*};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
struct TableCell<'a> {
  _state: u32,
  _debug_state_name: &'a String,
  _goto_state: u32,
  _input_byte: u32,
  _shift_length: u32,
}

/// Information on byte code branches
#[derive(Debug, Clone)]
pub(crate) struct BranchTableData {
  /// Stores an offset of the branch that is taken given
  /// a particular symbol, and the symbol that activates
  /// the branch.
  pub symbols:  BTreeMap<u32, Symbol>,
  pub data:     TableHeaderData,
  /// All branches defined within the branch table,
  /// keyed by their bytecode address or by the indexed
  /// of the branch's location in the table should the
  /// branch be a skip.
  pub branches: BTreeMap<usize, BranchData>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BranchData {
  pub is_skipped: bool,
  pub address:    usize,
  /// The value of the branch's key, be it the token type,
  /// production id, byte, class, or codepoint
  pub value:      u32,
}

impl BranchTableData {
  pub fn from_bytecode(
    instruction: INSTRUCTION,
    g: &GrammarStore,
    output: &BytecodeOutput,
  ) -> Option<Self> {
    let address = instruction.get_address();
    if instruction.is_hash_branch() || instruction.is_vector_branch() {
      let data = TableHeaderData::from_bytecode(address, &output.bytecode);

      let TableHeaderData { input_type, table_length, table_meta, .. } = data;

      let branches = match instruction.to_type() {
        InstructionType::VectorBranch => output.bytecode
          [(address + 4)..(address + 4 + table_length as usize)]
          .iter()
          .enumerate()
          .map(|(i, offset_delta)| {
            let discriminant = (i as u32) + table_meta;
            let address = (*offset_delta as usize) + address;
            let is_skipped = *offset_delta == 0xFFFF_FFFF;
            (discriminant as usize, BranchData {
              value: discriminant,
              address: if is_skipped { 0 } else { address },
              is_skipped,
            })
          })
          .collect::<BTreeMap<_, _>>(),
        InstructionType::HashBranch => output.bytecode
          [(address + 4)..(address + 4 + table_length as usize)]
          .iter()
          .enumerate()
          .map(|(_, cell)| {
            let discriminant = cell & 0x7FF;
            let offset_delta = (cell >> 11) & 0x7FF;
            let address = (offset_delta as usize) + address;
            let is_skipped = offset_delta == 0x7FF;
            (discriminant as usize, BranchData {
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
        InputType::T02_TOKEN => {
          let mut symbols = BTreeMap::new();

          for (_, branch) in &branches {
            if let Some(symbol) = symbol_lookup.get(&branch.value) {
              symbols.insert(branch.value, symbol.clone());
            } else {
              dbg!(&g.productions);
              dbg!(symbol_lookup);
              dbg!(branch.value);
              panic!("Missing symbol!")
            }
          }
          symbols
        }
        _ => BTreeMap::new(),
      };

      Some(BranchTableData { symbols, data, branches })
    } else {
      None
    }
  }

  /// Returns true if the combination of token values can be compared using
  /// symbol bitwise expressions. This implies that the tokens are all fixed
  /// length strings.
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
