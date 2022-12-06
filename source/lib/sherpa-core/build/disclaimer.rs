use crate::util::SHERPA_VERSION;

use super::pipeline::PipelineContext;

pub static DISCLAIMER: fn(
  file_type: &str,
  comment_delimiter: &str,
  ctx: &PipelineContext,
) -> String = |file_type, comment_delimiter, ctx| {
  let grammar = ctx.get_journal().grammar().unwrap();
  format!(
    r##"
{3} ### `{1}` {2}
{3}
{3} - **GENERATOR**: sherpa {0}
{3} - **SOURCE**: {4}
{3}
{3} #### WARNING:
{3}
{3} This is a generated file. Any changes to this file may be **overwritten 
{3} without notice**.
{3}
{3} #### License:
{3} Copyright (c) 2023 Anthony Weathersby
{3} 
{3} Permission is hereby granted, free of charge, to any person obtaining a copy
{3} of this software and associated documentation files (the "Software"), to deal
{3} in the Software without restriction, including without limitation the rights
{3} to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
{3} copies of the Software, and to permit persons to whom the Software is
{3} furnished to do so, subject to the following conditions:
{3} 
{3} The above copyright notice and this permission notice shall be included in all
{3} copies or substantial portions of the Software.
{3} 
{3} THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
{3} IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
{3} FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
{3} AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
{3} LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
{3} OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
{3} SOFTWARE.
{3}
"##,
    SHERPA_VERSION,
    grammar.id.name,
    file_type,
    comment_delimiter,
    grammar.id.path.to_str().unwrap(),
  )
};
