use std::{collections::HashMap, vec};

use sherpa_core::{CachedString, ErrorClass, IString, IStringStore, SherpaError, SherpaResult};

use crate::parser::{self, Call, Funct, Match};

pub struct Formatter {
  script:    Vec<parser::ASTNode>,
  functions: HashMap<IString, Box<Funct>>,
}

impl Formatter {
  pub fn new(script: Vec<parser::ASTNode>) -> Self {
    //dbg!(&script);
    Self {
      functions: script
        .iter()
        .filter_map(|n| {
          use parser::ASTNode::*;
          match n {
            Funct(funct) => Some((funct.name.to_token(), funct.clone())),
            _ => None,
          }
        })
        .collect(),
      script,
    }
  }

  pub fn build(&self, ctx: &mut FormatterContext) -> SherpaResult<String> {
    Ok(Self::interpret_sequence(&self.script, ctx, &self.functions)?.join(""))
  }

  fn interpret_sequence(
    seq: &[parser::ASTNode],
    ctx: &mut FormatterContext,
    functions: &HashMap<IString, Box<Funct>>,
  ) -> SherpaResult<Vec<String>> {
    use parser::*;
    let mut strings = vec![];

    let mut iter = seq.iter().peekable();

    while let Some(node) = iter.next() {
      if node.as_NewLine().is_some() {
        // Re-order indentation ahead of newlines
        while let Some(node) = iter.peek() {
          match node {
            ASTNode::Indent(_) | ASTNode::Dedent(_) => {
              if let Some(string) = Self::interpret_node(node, ctx, functions)? {
                strings.push(string)
              }
              iter.next();
            }
            _ => {
              break;
            }
          }
        }
      }
      if let Some(string) = Self::interpret_node(node, ctx, functions)? {
        strings.push(string)
      }
    }

    Ok(strings)
  }

  fn interpret_node(
    node: &parser::ASTNode,
    ctx: &mut FormatterContext,
    functions: &HashMap<IString, Box<Funct>>,
  ) -> SherpaResult<Option<String>> {
    use parser::ASTNode::*;
    let result = match node {
      Match(m) => return Self::interpret_match(m, ctx, functions).map(|s| Some(s)),
      Call(call) => {
        let call_name = &call.name;
        match functions.get(&call_name.to_token()) {
          Some(funct) => return Self::interpret_function_call(call, funct, ctx, functions).map(|s| Some(s)),
          _ => return create_missing_function_error(call),
        }
      }
      Space(space) => Some(" ".repeat(space.count.max(1) as usize)),
      NewLine(_) => Some("\n".to_string() + &" ".repeat(ctx.tab_size()).repeat(ctx.indent_level())),
      Dedent(_) => {
        ctx.dedent();
        None
      }
      Indent(_) => {
        ctx.indent();
        None
      }
      Literal(txt) => Some(txt.val.clone()),
      Text(txt) => Some(txt.val.clone()),
      Funct(_) => None,
      Num(num) => Some(num.val.clone()),
      obj @ Obj(..) => {
        let val = Self::eval_expression(&obj, ctx)?;
        Some(Self::print_value(val, ctx, functions)?)
      }
      Expression(expr) => {
        let val = Self::eval_expression(&expr.val, ctx)?;
        Some(Self::print_value(val, ctx, functions)?)
      }
      node => todo!("Handle the interpretation of: {node:?}"),
    };
    Ok(result)
  }

  fn print_value(val: Value, ctx: &FormatterContext, functions: &HashMap<IString, Box<Funct>>) -> SherpaResult<String> {
    Ok(match val {
      Value::Int(int) => int.to_string(),
      Value::Num(num) => num.to_string(),
      Value::Str(str) => str.to_string(&ctx.s_store),
      obj_val @ Value::Obj(type_name, map) => {
        println!("{}", "#type_".to_string() + type_name.to_str(&ctx.s_store).as_str());
        let type_function_name = ("#type_".to_string() + type_name.to_str(&ctx.s_store).as_str()).to_token();
        if let Some(funct) = functions.get(&type_function_name) {
          Self::interpret_type_function(funct, ctx, functions, obj_val)?
        } else {
          type_name.to_string(&ctx.s_store)
        }
      }
      _ => Default::default(),
    })
  }

  fn interpret_type_function(
    funct: &Funct,
    ctx: &FormatterContext,
    functions: &HashMap<IString, Box<Funct>>,
    obj_val: Value,
  ) -> SherpaResult<String> {
    if funct.params.len() != 1 {
      panic!("A type function should only have on param")
    }

    if funct.params[0].ty != "obj" {
      panic!("A type function param should have type 'obj'")
    }

    let local_map = HashMap::from_iter(vec![(funct.params[0].name.to_token(), obj_val)]);

    let result = loop {
      let ctx = &ctx;
      let mut ctx = FormatterContext::create_scope(ctx, &local_map);
      break Self::interpret_sequence(&funct.content, &mut ctx, functions)?.join("");
    };

    Ok(result)
  }

  fn interpret_function_call(
    call: &Call,
    funct: &Funct,
    ctx: &FormatterContext,
    functions: &HashMap<IString, Box<Funct>>,
  ) -> SherpaResult<String> {
    use parser::ASTNode::*;
    let mut values = HashMap::default();

    if call.args.len() != funct.params.len() {
      panic!("Args miss match")
    }

    for (arg, param) in call.args.iter().zip(funct.params.iter()) {
      let name = param.name.clone();
      match Self::eval_expression(arg, ctx)? {
        num @ Value::Num(val) => match param.ty.as_str() {
          "num" => {
            values.insert(name.intern(&ctx.s_store), num);
          }
          "int" => {
            values.insert(
              name.intern(&ctx.s_store),
              Value::Int(val.round().min(100000000000000.0).max(-1000000000000000.0) as isize),
            );
          }
          _ => panic!("Numeric Value miss match param: {name}-{} val: num -{}", &param.ty, val),
        },
        node => todo!("Handle the interpretation of arg type: {node:?}"),
      }
    }

    let result = loop {
      let ctx = &ctx;
      let mut ctx = FormatterContext::create_scope(ctx, &values);
      break Self::interpret_sequence(&funct.content, &mut ctx, functions)?.join("");
    };

    Ok(result)
  }

  fn eval_expression<'a>(expr: &'a parser::ASTNode, ctx: &'a FormatterContext) -> SherpaResult<Value<'a>> {
    use parser::ASTNode::*;
    Ok(match expr {
      Obj(obj) => match ctx.get(&obj.name.to_token()) {
        obj_val @ Value::Obj(_, obj_map) => {
          if obj.path.is_empty() {
            obj_val
          } else {
            let mut obj_map = Some(obj_map);
            let mut obj_val = Value::None;
            for (index, path) in obj.path.iter().enumerate() {
              if let Some(obj_map_unwrapped) = &obj_map {
                if let Some(o) = obj_map_unwrapped.get(&path.to_token()) {
                  obj_val = *o;
                  obj_map = match *o {
                    Value::Obj(_, obj_map) => Some(obj_map),
                    _ => None,
                  };
                } else {
                  panic!(
                    "{}.{} value is not an object",
                    obj.name,
                    obj.path[..=index].iter().cloned().collect::<Vec<_>>().join(".")
                  )
                }
              } else {
                panic!("property {}.{} is undefined", obj.name, obj.path[..=index].iter().cloned().collect::<Vec<_>>().join("."))
              }
            }
            obj_val
          }
        }
        _ => panic!("{} value is not an object", obj.name),
      },
      Num(num) => {
        if let Ok(val) = num.val.trim_start_matches('0').parse::<f64>() {
          Value::Num(val)
        } else {
          panic!("{} is unparsable as a number", &num.val)
        }
      }
      Add(add) => match &Self::eval_expression(&add.l, ctx)? {
        left_val @ Value::Num(left) => match Self::eval_expression(&add.r, ctx)? {
          Value::Num(right) => Value::Num(left + right),
          Value::Int(right) => Value::Num(left + right as f64),
          Value::Str(right) => Value::Str((left.to_string() + &right.to_string(&ctx.s_store)).intern(&ctx.s_store)),
          Value::Obj(..) => panic!("Invalid add expression"),
          _ => *left_val,
        },
        left_val @ Value::Int(left) => match Self::eval_expression(&add.r, ctx)? {
          Value::Num(right) => Value::Num(*left as f64 + right),
          Value::Int(right) => Value::Int(left + right),
          Value::Str(right) => Value::Str((left.to_string() + &right.to_string(&ctx.s_store)).intern(&ctx.s_store)),
          Value::Obj(..) => panic!("Invalid add expression"),
          _ => *left_val,
        },
        left_val @ Value::Str(left) => match Self::eval_expression(&add.r, ctx)? {
          Value::Num(right) => Value::Str((left.to_string(&ctx.s_store) + &right.to_string()).intern(&ctx.s_store)),
          Value::Int(right) => Value::Str((left.to_string(&ctx.s_store) + &right.to_string()).intern(&ctx.s_store)),
          Value::Str(right) => Value::Str((left.to_string(&ctx.s_store) + &right.to_string(&ctx.s_store)).intern(&ctx.s_store)),
          Value::Obj(..) => panic!("Invalid add expression"),
          _ => *left_val,
        },
        Value::Obj(..) => panic!("Invalid add expression"),
        _ => Self::eval_expression(&add.r, ctx)?,
      },
      Sub(sub) => match &Self::eval_expression(&sub.r, ctx)? {
        left_val @ Value::Num(left) => match Self::eval_expression(&sub.r, ctx)? {
          Value::Num(right) => Value::Num(left - right),
          Value::Int(right) => Value::Num(left - right as f64),
          Value::Str(..) => panic!("Invalid sub expression"),
          Value::Obj(..) => panic!("Invalid sub expression"),
          _ => *left_val,
        },
        left_val @ Value::Int(left) => match Self::eval_expression(&sub.r, ctx)? {
          Value::Num(right) => Value::Num(*left as f64 - right),
          Value::Int(right) => Value::Int(left - right),
          Value::Str(..) => panic!("Invalid sub expression"),
          Value::Obj(..) => panic!("Invalid sub expression"),
          _ => *left_val,
        },
        Value::None => match Self::eval_expression(&sub.r, ctx)? {
          Value::Num(right) => Value::Num(-right),
          Value::Int(right) => Value::Num(-right as f64),
          _ => panic!("Invalid sub expression"),
        },
        _ => panic!("Invalid sub expression"),
      },
      Mul(mul) => match &Self::eval_expression(&mul.r, ctx)? {
        Value::Num(left) => match Self::eval_expression(&mul.r, ctx)? {
          Value::Num(right) => Value::Num(left * right),
          Value::Int(right) => Value::Num(left * right as f64),
          Value::Str(..) => panic!("Invalid mul expression"),
          Value::Obj(..) => panic!("Invalid mul expression"),
          _ => Value::Num(0 as f64),
        },
        Value::Int(left) => match Self::eval_expression(&mul.r, ctx)? {
          Value::Num(right) => Value::Num(*left as f64 * right),
          Value::Int(right) => Value::Int(left * right),
          Value::Str(..) => panic!("Invalid mul expression"),
          Value::Obj(..) => panic!("Invalid mul expression"),
          _ => Value::Int(0),
        },
        Value::None => match Self::eval_expression(&mul.r, ctx)? {
          Value::Num(_) => Value::Num(0 as f64),
          Value::Int(_) => Value::Int(0),
          _ => panic!("Invalid mul expression"),
        },
        _ => panic!("Invalid mul expression"),
      },
      Div(div) => match &Self::eval_expression(&div.r, ctx)? {
        Value::Num(left) => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(right) => Value::Num(left * right),
          Value::Int(right) => Value::Num(left * right as f64),
          Value::Str(..) => panic!("Invalid div expression"),
          Value::Obj(..) => panic!("Invalid div expression"),
          _ => Value::None,
        },
        Value::Int(left) => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(right) => Value::Num(*left as f64 / right),
          Value::Int(right) => Value::Int(left / right),
          _ => panic!("Invalid div expression"),
        },
        Value::None => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(_) => Value::Num(0 as f64),
          Value::Int(_) => Value::Int(0),
          _ => panic!("Invalid div expression"),
        },
        _ => panic!("Invalid div expression"),
      },
      node => todo!("Handle the interpretation of for expressions: {node:?}"),
    })
  }

  fn eval_object<'a>(obj: &'a parser::Obj, ctx: &'a FormatterContext) -> SherpaResult<Value<'a>> {
    Ok(match ctx.get(&obj.name.to_token()) {
      string @ Value::Str(_) => string,
      int @ Value::Int(_) => int,
      num @ Value::Num(_) => num,
      val => todo!("Handle the object resolution of: {val:?} for ref {}", &obj.name),
    })
  }

  fn interpret_match(m: &Match, ctx: &mut FormatterContext, functions: &HashMap<IString, Box<Funct>>) -> SherpaResult<String> {
    let val = Self::eval_object(&m.obj, ctx)?;

    let (default, matches) = m.matches.iter().partition::<Vec<_>, _>(|f| f.default);

    for m in matches {
      if match Self::eval_expression(m.match_expr.as_ref().unwrap(), ctx)? {
        Value::Int(right) => match val {
          Value::Int(left) => left == right,
          Value::Num(left) => left == right as f64,
          _ => false,
        },
        Value::Num(right) => match val {
          Value::Int(left) => left == right as isize,
          Value::Num(left) => left == right,
          _ => false,
        },
        v => todo!("handle {v:?}"),
      } {
        return Ok(Self::interpret_sequence(&m.content, ctx, functions)?.join(""));
      }
    }

    for d in default {
      return Ok(Self::interpret_sequence(&d.content, ctx, functions)?.join(""));
    }

    Ok(String::default())
  }
}

fn create_missing_function_error(call: &parser::Call) -> Result<Option<String>, SherpaError> {
  Err(SherpaError::SourceError {
    loc:        call.tok.clone(),
    path:       Default::default(),
    id:         (ErrorClass::Parsing, 102, "missing function").into(),
    msg:        "Function ".to_string() + &call.name + " is not defined.",
    inline_msg: "".into(),
    ps_msg:     "hint: create a minimum function definition for ".to_string() + &call.name + ": " + &call.name + " { }",
    severity:   sherpa_core::SherpaErrorSeverity::Critical,
  })
}

pub struct FormatterContext<'scope> {
  parent:       Option<&'scope FormatterContext<'scope>>,
  values:       Option<&'scope HashMap<IString, Value<'scope>>>,
  tab_size:     usize,
  indent_level: usize,
  s_store:      IStringStore,
}

impl<'scope> FormatterContext<'scope> {
  pub fn new_with_values(values: &'scope HashMap<IString, Value<'scope>>, s_store: IStringStore) -> Self {
    Self {
      tab_size:     2,
      indent_level: 0,
      parent:       None,
      values:       Some(values),
      s_store:      s_store,
    }
  }

  pub fn new() -> Self {
    Self {
      tab_size:     2,
      indent_level: 0,
      parent:       None,
      values:       None,
      s_store:      Default::default(),
    }
  }

  fn dedent(&mut self) {
    if self.indent_level != 0 {
      self.indent_level -= 1
    }
  }

  fn tab_size(&self) -> usize {
    self.tab_size
  }

  fn indent_level(&self) -> usize {
    self.indent_level
  }

  fn indent(&mut self) {
    self.indent_level += 1
  }

  fn create_scope<'a>(par: &'a Self, values: &'a HashMap<IString, Value<'a>>) -> FormatterContext<'a> {
    FormatterContext {
      tab_size:     par.tab_size,
      indent_level: par.indent_level,
      parent:       Some(par),
      values:       Some(values),
      s_store:      par.s_store.clone(),
    }
  }

  fn get<'s>(&'s self, id: &IString) -> Value<'s> {
    if let Some(values) = &self.values {
      match values.get(id) {
        Some(value) => *value,
        None => Value::None,
      }
    } else {
      Value::None
    }
  }
}

pub enum FormatterResult {
  Ok(Formatter),
  Err(SherpaError),
}

impl From<&str> for FormatterResult {
  fn from(value: &str) -> Self {
    match parser::ast::default_from(value.into()) {
      Err(parse_error) => FormatterResult::Err(parse_error.into()),
      Ok(script) => FormatterResult::Ok(Formatter::new(script)),
    }
  }
}

#[derive(Default, Clone, Copy, Debug)]
pub enum Value<'scope> {
  Vec(&'scope [Value<'scope>]),
  Obj(IString, &'scope HashMap<IString, Value<'scope>>),
  Num(f64),
  Int(isize),
  Str(IString),
  #[default]
  None,
}
