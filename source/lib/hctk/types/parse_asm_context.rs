use std::collections::VecDeque;

use super::*;
#[derive(Clone)]
#[repr(C)]
pub struct ParserContext<T: SymbolReader>
{
    peek_token: ParseToken,
    anchor_token: ParseToken,
    assert_token: ParseToken,
    foreign_rsp: usize,
    local_rsp: usize, // Points to top of local_state_stack
    local_stack_base: usize,
    state_u64_data: usize,
    action_pointer: usize,
    struct_reader_ptr: *mut T,
    fn_get_line_data: fn(&T) -> u64,
    fn_get_length_data: fn(&T) -> u64,
    fn_next: fn(&mut T, amount: i32) -> u64,
    fn_set_cursor_to: fn(&mut T, token: &ParseToken) -> u64,
    local_state_stack: Vec<usize>,
}

impl<T: SymbolReader> ParserContext<T>
{
    pub fn new(reader: &mut T) -> Self
    {
        unsafe {
            Self {
                peek_token: ParseToken::default(),
                anchor_token: ParseToken::default(),
                assert_token: ParseToken::default(),
                foreign_rsp: 0,
                local_rsp: 0,
                local_stack_base: 0,
                state_u64_data: 0,
                action_pointer: 0,
                struct_reader_ptr: reader,
                fn_next: T::next,
                fn_set_cursor_to: T::set_cursor_to,
                fn_get_line_data: T::get_line_data,
                fn_get_length_data: T::get_length_data,
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
