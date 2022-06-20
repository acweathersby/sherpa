use std::fmt::format;
use std::fmt::Debug;
use std::fmt::Display;

use crate::grammar::hash_id_value_u64;

use super::SymbolID;

pub struct IRStateString
{
    comment:        String,
    ir_code:        String,
    hash:           u64,
    state_name:     String,
    normal_symbols: Vec<SymbolID>,
    peek_symbols:   Vec<SymbolID>,
}

impl IRStateString
{
    pub fn get_state_name_from_hash(hash: u64) -> String
    {
        format!("s{:02x}", hash)
    }

    pub fn new(
        comment: &str,
        ir_code: &str,
        state_name: String,
        normal_symbols: Option<Vec<SymbolID>>,
        peek_symbols: Option<Vec<SymbolID>>,
    ) -> Self
    {
        let hash = hash_id_value_u64(ir_code);

        IRStateString {
            comment: comment.to_string(),
            ir_code: ir_code.to_string(),
            hash,
            state_name,
            normal_symbols: if let Some(syms) = normal_symbols {
                syms
            } else {
                vec![]
            },
            peek_symbols: if let Some(syms) = peek_symbols {
                syms
            } else {
                vec![]
            },
        }
    }

    pub fn get_name(&self) -> String
    {
        if self.state_name.is_empty() {
            Self::get_state_name_from_hash(self.hash)
        } else {
            self.state_name.clone()
        }
    }

    pub fn get_hash(&self) -> u64
    {
        self.hash.clone()
    }

    pub fn get_code<'a>(&self) -> String
    {
        format!("{}\n{}\n", self.get_state_header(), self.ir_code,)
    }

    pub fn get_comment<'a>(&'a self) -> &'a String
    {
        &self.comment
    }

    pub fn get_state_header(&self) -> String
    {
        format!("state [ {} ] \n", self.get_name())
    }

    pub fn get_symbols<'a>(&'a self) -> (&'a Vec<SymbolID>, &'a Vec<SymbolID>)
    {
        (&self.normal_symbols, &self.peek_symbols)
    }
}

impl Debug for IRStateString
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!(
            "{}\\*\n {} \n*\\\n{}\n\n\n",
            self.get_state_header(),
            self.comment,
            self.ir_code,
        ))
    }
}

impl Display for IRStateString
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!(
            "{}\\*\n {} \n*\\\n{}\n\n\n",
            self.get_state_header(),
            self.comment,
            self.ir_code,
        ))
    }
}
