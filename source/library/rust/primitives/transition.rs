use bitmask_enum::bitmask;
use std::{hash::Hash, vec};

use super::{Item, SymbolID};

pub type TransitionGraphNodeId = usize;

#[bitmask]
pub enum TransitionStateType {
    UNDEFINED,
    START,
    ///
    ///Transition has occurred from
    ///the consumption of a terminal
    ///symbol. All transition should
    ///have this set except for the
    ///initial state and the goal state.
    O_TERMINAL,
    ///
    ///Transition has occurred from the
    ///completion of non-terminal symbol.
    O_PRODUCTION,
    ///
    ///Node represents a branch of one or
    ///more sub-nodes. Each sub-node should
    ///should be gated by an assert, peek, or
    ///consume verification instruction.
    MULTI,
    ///
    ///Transition has occurred from the
    ///accepting of a completed root item.
    ACCEPT,

    ///
    ///Transition has occurred from the
    ///accepting of a completed root item.
    AMBIGUOUS,

    ///
    ///State includes items out of the scope of the current production
    ///that should be used for disambiguating states that would
    ///cause a reduction to a production ID other than the current
    ///production.
    I_OUT_OF_SCOPE,
    ///
    ///Consumption of tokens is not allowed
    O_PEEK,

    I_FORK,

    ///
    ///Transition has occurred from the
    ///accepting of a root item.
    I_END,

    ///
    ///The current state represents a completed
    ///production. Used by scanner to determine
    ///when to apply token assignments
    COMPLETED,

    O_GOTO,

    LOOP,

    I_GOTO_START,

    I_DESCENT_START,

    I_CONSUME,

    I_SCANNER,

    I_PASS,

    I_FAIL,

    I_TEST,

    ///
    ///This state is set when the nodes item has a skipped symbol
    ///that occludes another item that consumes that symbol.
    I_SKIPPED_COLLISION,

    I_COMPLETE,
}

pub struct TransitionGraphNode {
    /// The symbols that lead to the
    /// transition to this state.
    pub sym: SymbolID,
    pub transition_type: TransitionStateType,
    pub items: Vec<Item>,
    pub parent: TransitionGraphNodeId,
    pub root: TransitionGraphNodeId,
    pub children: Vec<TransitionGraphNode>,
    pub depth: u32,
    pub id: usize,
}

impl TransitionGraphNode {
    pub fn new() -> Self {
        TransitionGraphNode {
            sym: SymbolID::Undefined,
            transition_type: TransitionStateType::UNDEFINED,
            items: vec![],
            children: vec![],
            parent: 0,
            root: 0,
            depth: 0,
            id: 0,
        }
    }
}
