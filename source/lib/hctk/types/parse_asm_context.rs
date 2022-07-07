use std::collections::VecDeque;

use super::*;

#[repr(C)]
pub struct ASMParserContext<T: SymbolReader>
{
    foreign_rsp: usize,
    local_rsp: usize, // Points to top of local_state_stack
    local_stack_base: usize,
    inner_context: usize,
    reader: *mut T,
    get_line_data: fn(&T) -> u64,
    get_length_data: fn(&T) -> u64,
    word: fn(&T) -> u32,
    byte: fn(&T) -> u32,
    codepoint: fn(&T) -> u32,
    class: fn(&T) -> u32,
    next: fn(&mut T, amount: u32),
    set_cursor_to: fn(&mut T, token: &ParseToken) -> bool,
    peek_token: ParseToken,
    anchor_token: ParseToken,
    assert_token: ParseToken,
    state_u64_data: usize,
    parse_action: ParseAction,
    local_state_stack: Vec<usize>,
}

impl<T: SymbolReader> ASMParserContext<T>
{
    pub fn new(reader: &mut T) -> Self
    {
        unsafe {
            Self {
                foreign_rsp: 0,
                local_rsp: 0,
                local_stack_base: 0,
                inner_context: 0,
                reader,
                class: T::class,
                codepoint: T::codepoint,
                word: T::word,
                byte: T::byte,
                next: T::next,
                set_cursor_to: T::set_cursor_to,
                get_line_data: T::get_line_data,
                get_length_data: T::get_length_data,
                peek_token: ParseToken::default(),
                anchor_token: ParseToken::default(),
                assert_token: ParseToken::default(),
                state_u64_data: 0,
                parse_action: ParseAction::default(),
                local_state_stack: vec![0; 32],
            }
        }
    }
}
#[no_mangle]
pub extern "C" fn hctk_extend_stack(stack: &mut Vec<usize>) -> usize
{
    let old_size = stack.len();
    if let Err(err) = stack.try_reserve(stack.len() << 1) {
        println!("Error on parse stack extension {}", err);
        0
    } else {
        // pad out stack if there is more
        // then double the original size
        for _ in 0..(stack.capacity() - (old_size << 1)) {
            stack.push(0);
        }

        // move all element to back of stack.
        for i in 0..old_size {
            stack.push(stack[i]);
        }
        1
    }
}
#[no_mangle]
pub extern "C" fn hctk_get_stack_pointer<'a>(
    stack: &mut Vec<usize>,
) -> *mut usize
{
    let ptr = stack.as_mut_ptr();

    ptr
}
#[no_mangle]
pub extern "C" fn hctk_get_stack_size(stack: &Vec<usize>) -> usize
{
    let size = stack.len() << 3;

    size
}
