pub static DISCLAIMER: fn(
    source_file: &str,
    file_type: &str,
    comment_delimiter: &str,
) -> String = |source_file, file_type, comment_delimiter| {
    format!(
        "
{3} ### `{1}` {2}
{3}
{3} - **GENERATOR** :   hc-compile {0}
{3} - **SOURCE FILE**:  {1}
{3}
{3} #### WARNING:
{3}
{3} This is a generated file. Any changes to this file may be
{3} overwritten without notice.
{3}
{3} #### Copyright
{3} 
{3} (C) 2022 Anthony Weathersby and the Hydrocarbon Toolkit Authors.

",
        env!("CARGO_PKG_VERSION"),
        source_file,
        file_type,
        comment_delimiter,
    )
};
