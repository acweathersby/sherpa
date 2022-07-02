use crate::types::ParseToken;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum ParseAction
{
    Undefined,
    CompleteState,
    FailState,
    ScannerToken(ParseToken),
    Fork
    {
        states_start_offset: u32,
        num_of_states:       u32,
        target_production:   u32,
    },
    Shift
    {
        skipped_characters: ParseToken,
        token: ParseToken,
    },
    Reduce
    {
        production_id: u32,
        body_id:       u32,
        symbol_count:  u32,
    },
    Accept
    {
        production_id: u32,
    },
    Error
    {
        message:    &'static str,
        last_input: ParseToken,
    },
}

impl Default for ParseAction
{
    fn default() -> Self
    {
        ParseAction::Undefined
    }
}
