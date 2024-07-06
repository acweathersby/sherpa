use crate::utils::{compile_and_run_grammars, compile_and_run_grammars2};
use radlr_core::*;
use std::path::PathBuf;

#[test]
fn json_parser() -> RadlrResult<()> {
  let grammar_source_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammars/json/json.radlr").canonicalize().unwrap();
  compile_and_run_grammars(
    &[std::fs::read_to_string(grammar_source_path.as_path())?.as_str()],
    &[("default", r##"[]"##, true), ("default", r##"{"test":[{ "test":"12\"34", "test":"12\"34"}]}"##, true)],
    Default::default(),
  )
}

#[test]
fn lua_parser() -> RadlrResult<()> {
  let grammar_source_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammars/lua/lua.radlr").canonicalize().unwrap();
  compile_and_run_grammars(
    &[std::fs::read_to_string(grammar_source_path.as_path())?.as_str()],
    &[("default", r##"print("Hello World")"##, true)],
    ParserConfig::default().hybrid(),
  )
}

#[test]
fn rum_lang() -> RadlrResult<()> {
  let grammar_source_path = PathBuf::from("/home/work/projects/lib_rum_common/crates/language/grammar/low_level.radlr")
    .canonicalize()
    .expect("Grammar not found");

  compile_and_run_grammars2(
    &[std::fs::read_to_string(grammar_source_path.as_path())?.as_str()],
    grammar_source_path.into(),
    &[("default", r##"d( id:u32 ) { id = 0 }"##, true)],
    ParserConfig::default().hybrid(),
  )
}

#[test]
fn zig_lang() -> RadlrResult<()> {
  let grammar_source_path =
    PathBuf::from("/home/work/projects/lib_radlr/grammars/zig/zig.radlr").canonicalize().expect("Grammar not found");

  compile_and_run_grammars2(
    &[std::fs::read_to_string(grammar_source_path.as_path())?.as_str()],
    grammar_source_path.into(),
    &[("default", r##"d( id:u32 ) { id = 0 }"##, true)],
    ParserConfig::default().hybrid(),
  )
}

#[test]
fn rum_script() -> RadlrResult<()> {
  let grammar_source_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../grammars/rum_script/rum_script.radlr").canonicalize().unwrap();

  compile_and_run_grammars(
    &[std::fs::read_to_string(grammar_source_path.as_path())?.as_str()],
    &[(
      "default",
      r##"
    
    [VELOCITY#PERSON]
    [x - y - z]
    (f32, f32, f32)

    [PERSON]()

    [NAME#PERSON](str)

    [VELOCITY#PERSON]
    [x - y - z]
    [r - g - b]
    [j - k - i]
    (f32, f32, f32)

    [TEXTURE]( bytes : file-cached )

    [ACTIVE_TEXTURES#TEXTURE]()

    [FRAMEBUFFER]()

    [ACTIVE_FRAME_BUFFER#FRAMEBUFFER]

    [FRAMEBUFFER.attachment#TEXTURE]()
        
    /// Table row methods
    person.set_name (new_name: str) <person: PERSON, name: NAME> { 
      person.name = new_name;
    }

    /// Table row methods
    person.clear_name () <person: PERSON, name: NAME> { 
      delete person.name;
    }

    person.set_destination (destination: str) <name: NAME> { 
      delete person.name;
    }

    {

      INACTIVE_FRAME_BUFFER <== FRAMEBUFFER {}
      ACTIVE_FRAME_BUFFER 

      
      let persons = PERSON { @ACTIVE && @NAME == "charles" && @AGE == 2};

      /// All young people need to go home, but some will be rebellious and choose not
      /// to go home. We'll need to make them go home.
      for person in persons {
        person.goto("home");
      }

      ///
      for person in PERSON{ .dest != .loc } { 
        // Run one round of the A* algorithm to get this person to his location.
      }


      for frame_buffer in  { 
        ACTIVE_FRAME_BUFFER ==> 
      }
    }
    
    "##,
      true,
    )],
    ParserConfig::default().hybrid(),
  )
}
