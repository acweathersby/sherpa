#[derive(Debug, Copy, Clone, PartialEq, Eq)]

pub struct KernelToken
{
    pub typ:         u16,
    pub byte_offset: u32,
    pub byte_length: u32,
    pub cp_offset:   u32,
    pub cp_length:   u32,
    pub line_number: u32,
    pub line_offset: u32,
}

impl KernelToken
{
    pub fn new() -> KernelToken
    {
        KernelToken {
            typ:         0,
            byte_offset: 0,
            byte_length: 0,
            cp_offset:   0,
            cp_length:   0,
            line_number: 0,
            line_offset: 0,
        }
    }

    pub fn next(&self) -> KernelToken
    {
        return KernelToken {
            typ:         0,
            byte_length: 0,
            cp_length:   0,
            cp_offset:   self.cp_offset + self.cp_length,
            byte_offset: self.byte_offset + self.byte_length,
            line_number: self.line_number,
            line_offset: self.line_offset,
        };
    }
}
