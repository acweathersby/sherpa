use std::io::Result;
use std::io::Write;

use hctk::debug::grammar;
use hctk::types::GrammarStore;

enum ASMLine
{
    CodeWithComment
    {
        code: String, comment: String
    },
    Code
    {
        code: String
    },
    Constant
    {
        key: String, expr: String
    },
}

pub trait X8664Writer<T: Write>
{
    // fn get_line(&mut self) -> &mut Vec<ASMLine>;

    fn get_writer_mut(&mut self) -> &mut T;

    // fn get_buffer(&mut self) -> &mut T;

    fn write_internal(&mut self, data: &[u8]) -> Result<&mut Self>
    {
        let amount = self.get_writer_mut().write(data)?;

        if amount != data.len() {
            Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "Did not write all data",
            ))
        } else {
            Ok(self)
        }
    }

    fn label(&mut self, name: &str, relative: bool) -> Result<&mut Self>
    {
        self.newline()?;
        if relative {
            self.write_internal(b".")?;
        }
        self.write_internal(name.as_bytes())?;
        self.write_internal(b":")?;
        Ok(self)
    }

    fn section(&mut self, name: &str) -> Result<&mut Self>
    {
        self.newline()?;
        self.write_internal(b"section ")?;
        self.write_internal(name.as_bytes())?;
        Ok(self)
    }

    fn line(&mut self, string: &str) -> Result<&mut Self>
    {
        self.newline()?;
        self.write_internal(string.as_bytes())
    }

    fn code(&mut self, string: &str) -> Result<&mut Self>
    {
        self.newline()?;
        self.write_internal(b"    ")?;
        self.write_internal(string.as_bytes())
    }

    fn commented_code(&mut self, code: &str, comment: &str) -> Result<&mut Self>
    {
        self.newline()?;
        self.write_internal(b"    ")?;
        self.write_internal(code.as_bytes())?;
        self.write_internal(b" ")?;
        self.comment(comment)
    }

    fn comment_line(&mut self, string: &str) -> Result<&mut Self>
    {
        self.newline()?;
        self.comment(string)
    }

    fn newline(&mut self) -> Result<&mut Self>
    {
        self.write_internal(b"\n")
    }

    /// defaults to a `nasm` comment: `; comment body`
    fn comment(&mut self, string: &str) -> Result<&mut Self>
    {
        self.write_internal(b"; ")?;
        self.write_internal(string.as_bytes())
    }

    fn constant(&mut self, name: &str, value: &str) -> Result<&mut Self>
    {
        self.newline()?;
        self.write_internal(name.as_bytes())?;
        self.write_internal(b" equ ")?;
        self.write_internal(value.as_bytes())
    }

    fn into_writer(self) -> T;

    fn inline(
        &mut self,
        inline_function: fn(&mut Self) -> Result<&mut Self>,
    ) -> Result<&mut Self>
    {
        inline_function(self)
    }

    fn inline_grammar<'a>(
        &'a mut self,
        grammar: &GrammarStore,
        inline_function: fn(&'a mut Self, grammar: &GrammarStore) -> Result<&'a mut Self>,
    ) -> Result<&mut Self>
    {
        inline_function(self, grammar)
    }

    fn inline_grammar_bytecode<'a>(
        &'a mut self,
        grammar: &GrammarStore,
        bytecode: &[u32],
        inline_function: fn(
            &'a mut Self,
            grammar: &GrammarStore,
            bytecode: &[u32],
        ) -> Result<&'a mut Self>,
    ) -> Result<&mut Self>
    {
        inline_function(self, grammar, bytecode)
    }
}
