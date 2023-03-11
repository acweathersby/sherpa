#[test]
pub fn lalrpop() -> SherpaResult<()> {
  test_runner(
    &[(
      "Top",
      "
use std::test::me;

grammar;

pub Term: i32 = {
  <n:Num> => n,
  \"(\" <t:Term> \")\" => t,
};",
      true,
    )
      .into()],
    None,
    TestConfig {
      print_disassembly: true,
      bytecode_parse: true,
      grammar_path: Some(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
          .join("../../grammar/lalrpop/lalrpop.sg")
          .canonicalize()
          .unwrap(),
      ),
      debugger_handler: Some(&|g| {
        console_debugger(g, super::utils::PrintConfig {
          display_scanner_output: false,
          display_input_data: false,
          display_instruction: false,
          display_state: false,
          ..Default::default()
        })
      }),
      ..Default::default()
    },
  )?;

  SherpaResult::Ok(())
}
