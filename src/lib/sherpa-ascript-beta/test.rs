use sherpa_core::{SherpaGrammar, SherpaResult};
use sherpa_formatter::*;

use crate::types::AscriptDatabase;

#[test]
fn constructs_ascipt_build_database() -> SherpaResult<()> {
  let source = r###"
  <> A > B+^test :ast { t_B, type: str($test), dang: false, name: $test }

  <> B > "A" | "B" | "C" 
  
  "###;

  let db = SherpaGrammar::new().add_source_from_string(source, "", false)?.build_db("", Default::default())?;

  let adb: AscriptDatabase = db.into();

  dbg!(&adb);

  assert_eq!(adb.structs.len(), 1);
  //assert_eq!(*adb.structs.first_key_value().unwrap().1.properties.
  // first_key_value().unwrap().0, "type".into());

  let f: Formatter = FormatterResult::from(
    r##"
  
  
  #print_type type:obj { 
    match type.agg_type {
      "vec" { 
        match type.base_type {
          "str"  { String }
          "tok"     { Vec<Token> }
          { @"[TODO: Map Type ]" }
        }
      }
      "mep" { @"[TODO: Map Type ]" }
      {
        match type.base_type {
          "tok"    { Token }
          "str"    { String } 
          "bool"   { bool }
          { [UNDEFINED\ @type.base_type] }
        }
      }
    }
  }

  #print_props props:obj i:int {
    @p={ props.[i] }
    @i={ i + 1 }

    match @p.name {"type" { r # type } { @p.name }} : \ #print_type(@p.type)

    match i { 
      { ,  @; \  #print_props(props, i) }
      props.len {  }
    }
  }
  @"
  #[derive(Clone)]
  #[cfg_attr(debug_assertions, derive(Debug))]"
  #print_structs structs:obj i:int {
    @s={ structs.[i] }
    @i={ i + 1 }

    struct\ @s.name\ { 
      #print_props(@s.props, 0) 
    }

    match i { 
      { #print_structs(structs, i) }
      structs.len { }
    }
  }

  #type_structs structs:obj {
    #print_structs(structs, 0)
  }


@{ 
@structs
  
@rules
}
    
    "##,
  )
  .into_result()?;
  let mut ctx: FormatterContext<'_> = FormatterContext::new_with_values(&adb, Default::default());
  ctx.max_width = 20;
  let output = f.write_to_string(&mut ctx, 1024)?;
  println!("{output}");
  assert_eq!("rules!", output);

  Ok(())
}
