use std::fmt::{Debug, Display};

use crate::grammar::hash_id_value_u64;

pub struct IRStateString {
    comment: String,
    ir_code: String,
    hash: u64,
    state_name: String,
}

impl IRStateString {
    pub fn new(comment: &str, ir_code: &str, state_name: String) -> Self {
        let hash = hash_id_value_u64(ir_code);
        IRStateString {
            comment: comment.to_string(),
            ir_code: ir_code.to_string(),
            hash,
            state_name,
        }
    }

    pub fn get_hash(&self) -> u64 {
        self.hash.clone()
    }

    pub fn get_code<'a>(&'a self) -> &'a String {
        &self.ir_code
    }

    pub fn get_comment<'a>(&'a self) -> &'a String {
        &self.comment
    }

    pub fn get_state_name_from_hash(hash: u64) -> String {
        format!("s{:02x}", hash)
    }
}

impl Debug for IRStateString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = if self.state_name.is_empty() {
            Self::get_state_name_from_hash(self.hash)
        } else {
            self.state_name.clone()
        };
        f.write_fmt(format_args!(
            "state [ {} ] \n\\*\n {} \n*\\\n{}\n\n\n",
            name, self.comment, self.ir_code,
        ))
    }
}

impl Display for IRStateString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = if self.state_name.is_empty() {
            Self::get_state_name_from_hash(self.hash)
        } else {
            self.state_name.clone()
        };
        f.write_fmt(format_args!(
            "\nstate [ {} ] \n\\*\n {} \n*\\\n{}\n\n\n",
            name, self.comment, self.ir_code,
        ))
    }
}
