use core::num;
use std::any::Any;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::format;

use crate::bytecode::constants::GOTO_INSTRUCTION_OFFSET_MASK;
use crate::bytecode::constants::GOTO_STATE_MASK;
use crate::bytecode::constants::INPUT_TYPE;
use crate::bytecode::constants::INSTRUCTION;
use crate::bytecode::constants::INSTRUCTION_CONTENT_MASK;
use crate::bytecode::constants::INSTRUCTION_HEADER_MASK;
use crate::bytecode::constants::TOKEN_ASSIGN_FLAG;
use crate::grammar;
use crate::types::Body;
use crate::types::GrammarStore;
use crate::types::Production;
use crate::types::Symbol;
