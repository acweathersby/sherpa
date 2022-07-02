/// Represents either a single parsable token or
/// a range of characters, as in the case of `skip`
/// tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
#[repr(C)]
pub struct ParseToken
{
    pub byte_offset: u32,
    pub byte_length: u32,
    pub cp_offset:   u32,
    pub cp_length:   u32,
    pub line_number: u32,
    pub line_offset: u32,
    pub token_type:  u32,
    pub padding:     u32,
}

impl ParseToken
{
    pub fn new() -> ParseToken
    {
        ParseToken::default()
    }

    #[inline]
    pub fn next(&self) -> ParseToken
    {
        ParseToken {
            cp_offset: self.cp_offset + self.cp_length,
            byte_offset: self.byte_offset + self.byte_length,
            line_number: self.line_number,
            line_offset: self.line_offset,
            ..Default::default()
        }
    }
}
