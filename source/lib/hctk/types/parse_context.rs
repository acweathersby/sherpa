use std::collections::VecDeque;

use crate::bytecode::constants::NORMAL_STATE_MASK;

use super::*;
#[derive(Clone)]
#[repr(C)]
pub struct ParseContext<T: SymbolReader>
{
    pub(crate) peek_token: ParseToken,
    pub(crate) anchor_token: ParseToken,
    pub(crate) assert_token: ParseToken,
    foreign_rsp: usize,
    local_rsp: usize, // Points to top of local_state_stack
    local_stack_base: usize,
    state_u64_data: usize,
    action_pointer: usize,
    reader: usize,
    fn_get_line_data: fn(&T) -> u64,
    fn_get_length_data: fn(&T) -> u64,
    fn_next: fn(&mut T, amount: i32) -> u64,
    fn_set_cursor_to: fn(&mut T, token: &ParseToken) -> u64,
    local_state_stack: Vec<usize>,
}

impl<T: SymbolReader> ParseContext<T>
{
    pub fn new(reader: &mut T) -> Self
    {
        unsafe {
            let reader = (reader as *mut T) as usize;
            Self {
                peek_token: ParseToken::default(),
                anchor_token: ParseToken::default(),
                assert_token: ParseToken::default(),
                foreign_rsp: 0,
                local_rsp: 0,
                local_stack_base: 0,
                state_u64_data: 0,
                action_pointer: 0,
                reader,
                fn_next: T::next,
                fn_set_cursor_to: T::set_cursor_to,
                fn_get_line_data: T::get_line_data,
                fn_get_length_data: T::get_length_data,
                local_state_stack: vec![0; 32],
            }
        }
    }

    pub(crate) fn bytecode_context() -> Self
    {
        Self {
            peek_token: ParseToken::default(),
            anchor_token: ParseToken::default(),
            assert_token: ParseToken::default(),
            foreign_rsp: 0,
            local_rsp: 0,
            local_stack_base: 0,
            state_u64_data: 0,
            action_pointer: 0,
            reader: 0,
            fn_next: T::next,
            fn_set_cursor_to: T::set_cursor_to,
            fn_get_line_data: T::get_line_data,
            fn_get_length_data: T::get_length_data,
            local_state_stack: vec![0; 32],
        }
    }

    /// The following methods are used exclusively by the
    /// the rust functions in [hctk::runtime::parser::parser_functions.rs]

    #[inline]
    pub(crate) fn in_fail_mode(&self) -> bool
    {
        self.reader > 0
    }

    #[inline]
    pub(crate) fn set_fail_mode_to(&mut self, is_in_fail_mode: bool)
    {
        self.reader = is_in_fail_mode as usize;
    }

    #[inline]
    pub(crate) fn in_peek_mode(&self) -> bool
    {
        self.local_stack_base > 0
    }

    #[inline]
    pub(crate) fn set_peek_mode_to(&mut self, is_in_peek_mode: bool)
    {
        self.local_stack_base = is_in_peek_mode as usize;
    }

    #[inline]
    pub(crate) fn is_interrupted(&self) -> bool
    {
        self.state_u64_data > 0
    }

    #[inline]
    pub(crate) fn set_interrupted_to(&mut self, is_interrupted: bool)
    {
        self.state_u64_data = is_interrupted as usize
    }

    /// Used by the bytecode interpreter
    #[inline]
    pub(crate) fn is_scanner(&self) -> bool
    {
        self.action_pointer > 0
    }

    /// Used by the bytecode interpreter
    #[inline]
    pub(crate) fn make_scanner(&mut self)
    {
        self.action_pointer = 1;
    }

    #[inline]
    pub(crate) fn get_active_state(&mut self) -> u32
    {
        self.foreign_rsp as u32
    }

    #[inline]
    pub(crate) fn set_active_state_to(&mut self, state: u32)
    {
        self.foreign_rsp = state as usize;
    }

    #[inline]
    pub(crate) fn get_production(&mut self) -> u32
    {
        self.local_rsp as u32
    }

    #[inline]
    pub(crate) fn set_production_to(&mut self, state: u32)
    {
        self.local_rsp = state as usize;
    }

    #[inline]
    pub(crate) fn pop_state(&mut self) -> u32
    {
        if self.local_state_stack.len() > 0 {
            self.local_state_stack.pop().unwrap() as u32
        } else {
            0
        }
    }

    #[inline]
    pub(crate) fn push_state(&mut self, state: u32)
    {
        self.local_state_stack.push(state as usize);
    }

    #[inline]
    pub fn init_normal_state(&mut self, entry_point: u32)
    {
        self.local_state_stack.clear();
        self.local_state_stack
            .push((NORMAL_STATE_MASK | entry_point) as usize);
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
