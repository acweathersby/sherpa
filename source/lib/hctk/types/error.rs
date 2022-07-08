use crate::deprecated_runtime::error::TokenError;

use ::std::fmt::Display;

use crate::types::Token;

/// Indicates an issue encountered while processing a grammar
/// symbol.
#[derive(Debug)]
pub struct CompileProblem
{
    // Message that appears
    pub message: String,
    /// Message that appears inline the code location
    /// diagram
    pub inline_message: String,
    pub loc: Token,
}

impl Display for CompileProblem
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!(
            "{}\n{}",
            self.message,
            self.loc
                .blame(1, 1, &self.inline_message)
                .unwrap_or_default(),
        ))
    }
}

/// Indicates an issue encountered while processing grammar
/// symbols that effects multiple locations in one or more files.
#[derive(Debug)]
pub struct CompoundCompileProblem
{
    pub message:   String,
    pub locations: Vec<CompileProblem>,
}

impl Display for CompoundCompileProblem
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!(
            "{}\n{}",
            self.message,
            self.locations
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join("\n"),
        ))
    }
}

#[derive(Debug)]
pub enum ParseError
{
    NOT_PARSED,
    UNDEFINED,
    IO_ERROR(std::io::Error),
    MUTEX_ERROR,
    THREAD_ERROR,
    COMPILE_PROBLEM(CompileProblem),
    TOKEN_ERROR(TokenError),
    COMPOUND_COMPILE_PROBLEM(CompoundCompileProblem),
}

impl ParseError
{
    pub fn is_not_parsed(&self) -> bool
    {
        matches!(self, ParseError::NOT_PARSED)
    }
}

impl Display for ParseError
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self {
            ParseError::NOT_PARSED => {
                f.write_str("This input has not been parsed")
            }
            ParseError::UNDEFINED => {
                f.write_str("An unknown error has occurred ")
            }
            ParseError::TOKEN_ERROR(err) => err.fmt(f),
            ParseError::IO_ERROR(err) => err.fmt(f),
            ParseError::MUTEX_ERROR => f.write_str("A Mutex has been poisoned"),
            ParseError::THREAD_ERROR => {
                f.write_str("Unable to get an exclusive lock on an object")
            }
            ParseError::COMPOUND_COMPILE_PROBLEM(err) => err.fmt(f),
            ParseError::COMPILE_PROBLEM(err) => err.fmt(f),
        }
    }
}
