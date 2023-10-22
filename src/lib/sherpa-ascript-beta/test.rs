use sherpa_core::{SherpaGrammar, SherpaResult};

use crate::types::AscriptDatabase;

#[test]
fn constructs_ascipt_build_database() -> SherpaResult<()> {
  let source = r###"
  <> A > B+^test "tree" :ast { t_B, type: [$test, $2], dang: true, name: $test }

  <> B > "A" | "B" | "C" 
  
  "###;

  let db = SherpaGrammar::new().add_source_from_string(source, "", false)?.build_db("", Default::default())?;

  let adb: AscriptDatabase = db.into();

  dbg!(&adb.rules);

  assert_eq!(adb.structs.len(), 1);
  //assert_eq!(*adb.structs.first_key_value().unwrap().1.properties.
  // first_key_value().unwrap().0, "type".into());

  let output = adb.format_string(
    r##"
#type_SymNode ref:str {
  let\ @ref\ =\ ctx[@self.index];@;
}

#type_TokNode ref:str {
  let\ @ref\ =\ tok[@self.index];@;
}

#type_BoolNode ref:str {
  match self.val {
    { let\ @ref\ =\ Falsed @; }
    & { 
      let\ @ref\ =\ match self.base_val 
        { "false" { False } "true" { True } } 
        ; @; 
      }
  }
}

#type_StrNode ref:str {
  -----@;
  @self.val.agg_type @;
  @self.val.base_type @;
  @self.val.(ref) @;
  -----@;
  match (self.val.agg_type, self.val.base_type) {
    ("vec", "tok") {  let\ @ref\ =\ @ref.first().range(@ref.last); }
    { let\ @ref:String\ = "";  }
  }@;
}
#print_struct_initializer init_props:obj i:int {
  @p={ init_props.[i] }
  
  @p.node.(@p.name)@;

  match i + 1 {
    init_props.len { }
    { #print_struct_initializer(init_props, i + 1) }
  }
}

#print_struct_assign init_props:obj i:int {
  @p={ init_props.[i] }
  
  @p.name

  match i + 1 {
    init_props.len { @; }
    { , @; #print_struct_assign(init_props, i + 1) }
  }
}
    
#print_rules rules:obj i:int {
    @r={ rules.[i] } @i={ i + 1 } @name={ @r.init.name }
  
    fn\ rule_@r.id\ (\ ctx:  &ParserContext\ )\ {
      match r.#type {
        "StructRule" { 
          #print_struct_initializer(r.init.props, 0)
          AST_@name\ (Box::new(@name\ {  #print_struct_assign(r.init.props, 0)  }))
        }
        "Expression"  { Expr }
        "ListInitial"  { obj[0] }
        "ListContinue"  { obj[0].push(obj[last]) }
        "LastSymbol"  { obj[last] }
      }  
    }
  
    match i { 
      {\n #print_rules(rules, i) }
      rules.len { }
    }
  }
  
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

#get_rust_type_name p:obj { 
  match @p.name {"type" { r # type } { @p.name }}
}
  
#print_props props:obj i:int {
  @p={ props.[i] } @i={ i + 1 }

  #get_rust_type_name(p) : \ #print_type(@p.type)

  match i { 
    { ,  @; \  #print_props(props, i) }
    props.len {  }
  }
}

@"#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]"

#print_structs structs:obj i:int {
  @s={ structs.[i] } @i={ i + 1 }

  struct\ @s.name\ { 
    #print_props(@s.props, 0) 
  }

  match i { 
    { #print_structs(structs, i) }
    structs.len { }
  }
}

#type_structs {
  #print_structs(self, 0)
}


#print_node n:obj {
  [@n.#type]
}

#type_rules {
  #print_rules(self, 0)
}

#print_rule_lookup rules:obj i:int {
  @r={ rules.[i] } @i={ i + 1 }

  rule_@r.id _test

  match i { 
    {, #print_rule_lookup(rules, i) }
    rules.len { }
  }
}

@{ 
@structs
  
@rules

const rule_lu: [RuleFN;@rules.len] =  [ #print_rule_lookup(rules, 0) ];
}
    
    "##,
    120,
  )?;

  println!("{output}");
  assert_eq!("rules!", output);

  Ok(())
}
