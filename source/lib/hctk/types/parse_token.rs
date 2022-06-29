#[derive(Debug, Copy, Clone, PartialEq, Eq)]

pub struct ParseToken
{
    pub byte_offset: u32,
    pub byte_length: u32,
    pub cp_offset:   u32,
    pub cp_length:   u32,
    pub line_number: u32,
    pub line_offset: u32,
    pub typ:         u32,
}

impl ParseToken
{
    pub fn new() -> ParseToken
    {
        ParseToken {
            typ:         0,
            byte_offset: 0,
            byte_length: 0,
            cp_offset:   0,
            cp_length:   0,
            line_number: 0,
            line_offset: 0,
        }
    }

    #[inline]
    pub fn next(&self) -> ParseToken
    {
        ParseToken {
            typ:         0,
            byte_length: 0,
            cp_length:   0,
            cp_offset:   self.cp_offset + self.cp_length,
            byte_offset: self.byte_offset + self.byte_length,
            line_number: self.line_number,
            line_offset: self.line_offset,
        }
    }
}

impl Default for ParseToken
{
    fn default() -> Self
    {
        Self::new()
    }
}
