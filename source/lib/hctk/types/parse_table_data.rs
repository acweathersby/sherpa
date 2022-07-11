#[derive(Debug, Clone, Copy)]
pub struct TableHeaderData
{
    pub input_type:     u32,
    pub lexer_type:     u32,
    pub table_length:   u32,
    pub table_meta:     u32,
    pub scanner_offset: u32,
}

impl TableHeaderData
{
    #[inline(always)]
    pub fn from_bytecode(offset: usize, bytecode: &[u32]) -> Self
    {
        let i = offset;

        let (first, scanner_offset, third) = unsafe {
            let v = bytecode.get_unchecked(i..i + 3);
            (v[0], v[1], v[2])
        };

        let input_type = (first >> 22) & 0x7;
        let lexer_type = (first >> 26) & 0x3;
        let table_length = (third >> 16) & 0xFFFF;
        let table_meta = third & 0xFFFF;

        Self {
            input_type,
            lexer_type,
            table_length,
            table_meta,
            scanner_offset,
        }
    }
}
