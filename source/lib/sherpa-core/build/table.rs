//! Functions for constructing and leveraging transition tables from
//! Hydrocarbon bytecode.

use sherpa_runtime::utf8::lookup_table::CodePointClass;

use crate::{compile::BytecodeOutput, types::*};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct LineData {
  pub(crate) num_of_lines: u32,
  pub(crate) last_offset:  u32,
  pub(crate) first_offset: u32,
}

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
  pub symbols:  BTreeMap<u32, (Symbol, Option<String>)>,
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
  // The number of line characters in this value and the offset
  // of the last line character.
  //pub line_offset: Option<(u32, u32)>,
}

impl BranchTableData {
  pub fn from_bytecode(
    instruction: Instruction,
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
              symbols.insert(
                branch.value,
                (symbol.clone(), g.symbol_strings.get(&symbol.guid).cloned()),
              );
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

  pub fn get_line_data(&self, branch: &BranchData) -> LineData {
    match self.data.input_type {
      InputType::T05_BYTE | InputType::T04_CODEPOINT => {
        if branch.value == 10 {
          LineData { first_offset: 0, last_offset: 0, num_of_lines: 1 }
        } else {
          Default::default()
        }
      }
      InputType::T03_CLASS => {
        if branch.value == CodePointClass::NewLine as u32 {
          LineData { first_offset: 0, last_offset: 0, num_of_lines: 1 }
        } else {
          Default::default()
        }
      }
      InputType::T02_TOKEN => {
        if let Some((symbol, string)) = self.symbols.get(&branch.value) {
          match symbol.guid {
            SymbolID::GenericNewLine => {
              LineData { first_offset: 0, last_offset: 0, num_of_lines: 1 }
            }
            SymbolID::DefinedIdentifier(_)
            | SymbolID::DefinedNumeric(_)
            | SymbolID::DefinedSymbol(_) => {
              let mut first_offset = u32::MAX;
              let mut last_offset = 0;
              let mut num_of_lines = 0;

              for (off, char) in string.as_ref().unwrap().chars().enumerate() {
                if char == '\n' {
                  first_offset = first_offset.min(off as u32);
                  last_offset = off as u32;
                  num_of_lines += 1;
                };
              }

              LineData { first_offset, last_offset, num_of_lines }
            }
            _ => Default::default(),
          }
        } else {
          Default::default()
        }
      }
      _ => Default::default(),
    }
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
    if let Some((sym, _)) = self.symbols.get(&branch.value) {
      Some(sym)
    } else {
      None
    }
  }
}
