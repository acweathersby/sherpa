mod parser {
  hctk::compile! {
  "
@NAME json

@IGNORE g:sp g:nl
  
  <> json_root >  array | object 
  
  <> array > \\[  value(*\\, ) \\]                  f:ast  { { t_Array, entries:$2 } }
  
  <> object > \\{  member(*\\, ) \\}                f:ast  { { t_Object, entries:$2 } }
  
  <> member > string \\:  value                      f:ast  { { t_Member, key:$1, val:$3 } }
  
  <> value > string 
    | number                                    
      | object 
      | array 
      | t:true                                    f:ast  { { t_Bool, val: false } }
      | t:false                                   f:ast  { { t_Bool, val: false } }
      | t:null                                    f:ast  { { t_Null } }
  
  <> string > 
      \\\" (g:id | g:sym | g:num | g:sp | escape )(*\" ) \\\"     f:ast  { { t_Str, val: $2 } }    
      | \\' (g:id | g:sym | g:num | g:sp | escape )(*\" ) \\'     f:ast  { { t_Str, val: $2 } }   
      
  <> escape > \\\\ ( g:id | g:sym | g:nl | g:num ) 
    
  
  <> number > (\\- |\\+ ) ? g:num(+) ( \\. g:num(*) )?                                
                                                        f:ast  { { t_Number, val: f64(tok) } }
  "
  }
}

#[cfg(test)]
mod test {
  use crate::parser;
  use hctk::types::UTF8StringReader;

  #[test]
  fn test() {
    unsafe {
      let bytes = include_bytes!("./spirv.data");
      let spirv = String::from_utf8_unchecked(bytes.to_vec());

      let input = " {\"d\\\"s\":2,\"ds\" :[2, -5404.004 ]}".to_string();

      let node = parser::Context::parse_default(&mut UTF8StringReader::new(&spirv));

      // println!("{:?}", node);
      if !node.is_ok() {
        panic!("Failed to parse input");
      }
    }
  }
}
