use super::pipeline::PipelineContext;

pub static DISCLAIMER: fn(
  file_type: &str,
  comment_delimiter: &str,
  ctx: &PipelineContext,
) -> String = |file_type, comment_delimiter, ctx| {
  let grammar = ctx.get_journal().grammar().unwrap();
  format!(
    "
{3} ### `{1}` {2}
{3}
{3} - **GENERATOR** :   hc-compile {0}
{3} - **SOURCE FILE**:  {1} {4:?}
{3}
{3} #### WARNING:
{3}
{3} This is a generated file. Any changes to this file may be
{3} overwritten without notice.
{3}
{3} #### Copyright
{3} 
{3} (C) 2022 Anthony Weathersby

",
    env!("CARGO_PKG_VERSION"),
    grammar.id.name,
    file_type,
    comment_delimiter,
    grammar.id.path,
  )
};
