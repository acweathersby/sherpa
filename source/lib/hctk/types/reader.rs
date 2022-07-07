use std::rc::Rc;
use std::sync::Arc;
use std::sync::RwLock;

use crate::types::parse_token::ParseToken;
use crate::utf8::*;

/// A multi-reader, multi-writer view of the underlying parser input
/// data, used to distribute access to the input string over multiple
/// Tokens and SymbolReaders.
pub type SharedSymbolBuffer = Arc<RwLock<Vec<u8>>>;

/// A reader maintains a read head which can be moved bidirectionally
/// over an array of bytes and yields information on the following:
/// - The byte offset and Unicode codepoint offset of the read head.
/// - The 8bit value of the current byte
/// - The Unicode at the read head, if there is a valid symbol there.
/// - The Hydrocarbon character class of the current symbol, if
///   available.
/// This loosely correlates to certain Unicode character classes.
/// - The UTF8 codepoint length, if it exists.
/// - The number of newline characters encountered up until the
///   current location of the read head.
/// - Both the codepoint offset and byte offset of the last line
/// encountered in the input.
pub trait SymbolReader
{
    /// Returns true if the cursor has reached the end of
    /// the input stream.

    fn at_end(&self) -> bool;

    fn offset_at_end(&self, offset: u32) -> bool
    {
        self.length() <= offset
    }

    /// Advances the internal cursor by `amount`
    fn next(&mut self, amount: u32);

    /// Returns the word at the current cursor position, little
    /// Endian

    fn word(&self) -> u32;

    /// Returns the byte at the current cursor position.
    fn byte(&self) -> u32;

    /// Returns the length of the source input. If this unknown
    /// then returns 0

    fn length(&self) -> u32
    {
        0
    }

    /// Returns the number of lines encountered.
    #[deprecated]
    fn line_count(&self) -> u32
    {
        0
    }

    /// Returns the offset of the most recent line character.
    #[deprecated]
    fn line_offset(&self) -> u32
    {
        0
    }

    /// Return a tuple comprised of the current line count
    /// and the current line offset, respectively.
    fn get_line_data(&self) -> u64
    {
        0
    }
    /// Return a u64 where the high 32bits represents
    /// the the byte length of the current character
    /// and the low 32bits represent the UTF16 codepoint
    /// length.
    fn get_length_data(&self) -> u64
    {
        0
    }

    /// Resets the cursor back to the value of the `offset`
    /// argument. Should the offset value exceed the limits
    /// of the underlying implementation, `false` is returned
    /// , indicating a parse failure as the input stream can
    /// no longer satisfy the requirements of the parser.

    fn set_cursor_to(&mut self, token: &ParseToken) -> bool;

    /// Return a new instance of byte reader with the same
    /// state as the source reader. Implementation should provide
    /// adequate shared buffers or other resources used to cache the
    /// input stream data, as multiple ByteReaders may be required
    /// read data at different cursor positions.
    #[deprecated]
    fn clone(&self) -> Self;

    /// Returns UTF8 codepoint information at the current cursor
    /// position.

    fn codepoint(&self) -> u32;

    fn codepoint_byte_length(&self) -> u32
    {
        return get_utf8_byte_length_from_code_point(self.codepoint());
    }

    fn codepoint_length(&self) -> u32
    {
        return get_token_length_from_code_point(self.codepoint());
    }

    fn class(&self) -> u32
    {
        return get_token_class_from_codepoint(self.codepoint());
    }

    fn cursor(&self) -> u32;

    /// Returns an optional vector of the input string data.
    fn get_source(&self) -> SharedSymbolBuffer;
}

pub struct UTF8FileReader
{
    length:      usize,
    cursor:      usize,
    line_count:  usize,
    line_offset: usize,
    string:      Rc<Vec<u8>>,
    word:        u32,
    codepoint:   u32,
}
