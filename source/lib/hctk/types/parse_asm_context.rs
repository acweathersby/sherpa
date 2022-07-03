use super::*;

pub type ASMParserContextStack = [u64; 128];

#[repr(C)]
pub struct ASMParserContext<T: SymbolReader>
{
    foreign_rsp: u64,
    local_rsp: u64, // Points to top of local_state_stack
    max_rsp: u64,
    context_metadata: u64,
    reader: *mut T,
    get_line_data: fn(&T) -> (u64, u64),
    get_length_data: fn(&T) -> (u64, u64),
    word: fn(&T) -> u32,
    byte: fn(&T) -> u8,
    codepoint: fn(&T) -> u32,
    class: fn(&T) -> u32,
    set_cursor_to: fn(&mut T, token: &ParseToken) -> bool,
    assert_token: ParseToken,
    anchor_token: ParseToken,
    peek_token: ParseToken,
    parse_action: ParseAction,
    local_state_stack: ASMParserContextStack,
}

impl<T: SymbolReader> ASMParserContext<T>
{
    pub fn new(reader: &mut T) -> Self
    {
        unsafe {
            Self {
                foreign_rsp: 0,
                local_rsp: 0,
                max_rsp: 0,
                context_metadata: 0,
                reader,
                class: T::class,
                codepoint: T::codepoint,
                word: T::word,
                byte: T::byte,
                set_cursor_to: T::set_cursor_to,
                get_line_data: T::get_line_data,
                get_length_data: T::get_length_data,
                assert_token: ParseToken::default(),
                anchor_token: ParseToken::default(),
                peek_token: ParseToken::default(),
                parse_action: ParseAction::default(),
                local_state_stack: [0; 128],
            }
        }
    }
}
