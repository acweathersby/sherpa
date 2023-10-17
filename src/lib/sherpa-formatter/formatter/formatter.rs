use crate::{
  parser::{self, Call, Funct, Match},
  types::*,
};
use sherpa_core::{CachedString, ErrorClass, IString, IStringStore, SherpaError, SherpaResult};
use sherpa_rust_runtime::types::Token;
use std::{collections::HashMap, io::Write, vec};

/// A formatter for complicated code.
///
/// The Sherpa formatter is a scriptable formatter that uses a turing complete
/// formatting language to write input objects into a suitable output text based
/// format.
///
/// Visit the [Sherpa site](about:blank) for more information
///
/// # Example 1
/// ```rust
///   match r###"#t {
///   {{ This is a complex string that comes up frequently.  }}
/// }
///   
/// #t() \n #t()
///   
///   
/// "###
///     .into()
///   {
///     FormatterResult::Ok(formatter) => {
///       let mut context = FormatterContext::new();
///       assert_eq!(
///         "This is a complex string that comes up frequently.\nThis is a complex string that comes up frequently.",
///         formatter.write_to_string(&mut context, 1024)?
///       );
///       Ok(())
///     }
///     FormatterResult::Err(err) => SherpaResult::Err(err),
///   }
/// ```
///
/// # Example 2
pub struct Formatter {
  script:    Vec<parser::ASTNode>,
  functions: HashMap<IString, Box<Funct>>,
}

pub enum FormatterResult {
  Ok(Formatter),
  Err(SherpaError),
}

impl FormatterResult {
  pub fn into_result(self) -> SherpaResult<Formatter> {
    self.into()
  }
}

impl From<&str> for FormatterResult {
  fn from(value: &str) -> Self {
    match parser::ast::default_from(value.into()) {
      Err(parse_error) => FormatterResult::Err(parse_error.into()),
      Ok(script) => FormatterResult::Ok(Formatter::new(script)),
    }
  }
}

impl Into<SherpaResult<Formatter>> for FormatterResult {
  fn into(self) -> SherpaResult<Formatter> {
    match self {
      FormatterResult::Err(parse_error) => Result::Err(parse_error.into()),
      FormatterResult::Ok(f) => Result::Ok(f),
    }
  }
}

impl Formatter {
  pub fn new(script: Vec<parser::ASTNode>) -> Self {
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

  pub fn write_to_string(&self, ctx: &mut FormatterContext, capacity: usize) -> SherpaResult<String> {
    unsafe { Ok(String::from_utf8_unchecked(self.write_to_output(ctx, Vec::with_capacity(capacity))?)) }
  }

  pub fn write_to_output<W: Write>(&self, ctx: &mut FormatterContext, mut output: W) -> SherpaResult<W> {
    let sequence = &self.script;
    let functions = &self.functions;
    Self::write_to_output_internal(sequence, ctx, functions, output)
  }

  fn write_to_output_internal<W: Write>(
    sequence: &Vec<parser::ASTNode>,
    ctx: &mut FormatterContext<'_>,
    functions: &HashMap<IString, Box<Funct>>,
    mut output: W,
  ) -> Result<W, SherpaError> {
    match Self::interpret_sequence(sequence, ctx, functions, &mut output)? {
      None => (),
      Some((tail_call, funct)) => {
        Self::interpret_function_call(tail_call, funct, ctx, functions, &mut output)?;
      }
    }
    output.flush()?;
    Ok(output)
  }

  fn interpret_sequence<'call, W: Write>(
    seq: &'call [parser::ASTNode],
    ctx: &mut FormatterContext,
    functions: &'call HashMap<IString, Box<Funct>>,
    w: &mut W,
  ) -> SherpaResult<Option<TailCall<'call>>> {
    use parser::*;

    let mut iter = seq.iter().peekable();

    let mut tail_call = None;

    while let Some(node) = iter.next() {
      if let Some((call, funct)) = tail_call.take() {
        Self::interpret_function_call(call, funct, ctx, functions, w)?
      }

      if node.as_NewLine().is_some() {
        // Re-order indentation ahead of newlines
        while let Some(la) = iter.peek() {
          match la {
            ASTNode::Indent(_) | ASTNode::Dedent(_) => {
              match Self::interpret_node_mut_ctx(la, ctx, functions, w)? {
                None => {}
                _ => unreachable!(), // Some((call, funct)) => Self::interpret_function_call(call, funct, ctx, functions, w)?,
              }
              iter.next();
            }
            _ => {
              break;
            }
          }
        }
      }

      if node.as_Call().is_some() && iter.peek().is_none() {
        let call = node.as_Call().unwrap();
        let call_name = &call.name;
        match functions.get(&call_name.to_token()) {
          Some(funct) => tail_call = Some((call, funct.as_ref())),
          _ => return create_missing_function_error(call),
        }
      } else {
        match Self::interpret_node_mut_ctx(node, ctx, functions, w)? {
          None => (),
          new_tail_call => {
            tail_call = new_tail_call;
          }
        }
      }
    }

    Ok(tail_call)
  }

  /// Prints nodes that do not modify the calling context.
  fn interpret_node_imut_ctx<'call, W: Write>(
    node: &'call parser::ASTNode,
    ctx: &FormatterContext,
    functions: &'call HashMap<IString, Box<Funct>>,
    w: &mut W,
  ) -> SherpaResult<Option<TailCall<'call>>> {
    use parser::ASTNode::*;
    match node {
      Space(space) => {
        w.write(" ".repeat(space.count.max(1) as usize).as_bytes())?;
      }
      NewLine(_) => {
        w.write(("\n".to_string() + &" ".repeat(ctx.tab_size()).repeat(ctx.indent_level())).as_bytes())?;
      }
      Literal(txt) => {
        w.write(txt.val.as_bytes())?;
      }
      Text(txt) => {
        w.write(txt.val.as_bytes())?;
      }
      BreakPoint(_) => {
        if ctx.block_level() > 0 {
          w.write(&[0])?;
        }
      }
      Funct(_) => {}
      Num(num) => {
        w.write(num.val.as_bytes())?;
      }
      obj @ Obj(o) => match o.id.at {
        true => {
          let val: Value<'_> = Self::eval_expression(&obj, ctx)?;
          Self::print_value(val, ctx, functions, w)?;
        }
        false => {
          w.write(o.tok.to_string().as_bytes())?;
        }
      },
      Expression(expr) => {
        let val = Self::eval_expression(&expr.val, ctx)?;
        Self::print_value(val, ctx, functions, w)?;
      }
      Funct(_) => {}
      node => todo!("Handle the interpretation of: {node:#?}"),
    };
    Ok(None)
  }

  /// Prints nodes that may or may not modify the calling context.
  fn interpret_node_mut_ctx<'call, W: Write>(
    node: &'call parser::ASTNode,
    ctx: &mut FormatterContext,
    functions: &'call HashMap<IString, Box<Funct>>,
    w: &mut W,
  ) -> SherpaResult<Option<TailCall<'call>>> {
    use parser::ASTNode::*;
    match node {
      Call(call) => {
        let call_name = &call.name;
        match functions.get(&call_name.to_token()) {
          Some(funct) => {
            Self::interpret_function_call(call, funct, ctx, functions, w)?;
          }
          _ => return create_missing_function_error(call),
        }
      }
      Match(m) => return Self::interpret_match(m, ctx, functions, w),
      Dedent(_) => {
        ctx.dedent();
      }
      Indent(_) => {
        ctx.indent();
      }
      LiteralSpace(l_space) => return Self::interpret_sequence(&l_space.content, ctx, functions, w),
      SBlock(block) => {
        let (left_sentinal, right_sentinal) = match block.ty.as_str() {
          "{" => ('{', '}'),
          "(" => ('(', ')'),
          "[" => ('[', ']'),
          _ => unreachable!(),
        };

        let mut writer = BlockFormatter::new();

        let tab_count = ctx.indent_level() + ctx.block_level();

        ctx.block_open();
        Self::write_to_output_internal(&block.content, ctx, functions, &mut writer)?;
        ctx.block_close();

        let should_break = writer.len > ctx.max_width();

        w.write(&[left_sentinal as u8])?;
        if should_break {
          ctx.indent();
        }

        writer.flush_to_writer(!should_break, ctx, w)?;

        if should_break {
          ctx.dedent();
          w.write(("\n".to_string() + &" ".repeat(ctx.tab_size()).repeat(tab_count)).as_bytes())?;
        }

        w.write(&[right_sentinal as u8])?;
      }
      Assign(assign) => {
        let name = assign.id.name.as_str();
        let expr = Self::eval_expression(&assign.expr, ctx)?;
        ctx.set(Self::get_id_val(&assign.id).to_token(), expr);
      }
      node => return Self::interpret_node_imut_ctx(node, ctx, functions, w),
    };
    Ok(None)
  }

  fn print_value<W: Write>(
    val: Value,
    ctx: &FormatterContext,
    functions: &HashMap<IString, Box<Funct>>,
    w: &mut W,
  ) -> SherpaResult<()> {
    match val {
      Value::Int(int) => {
        w.write(int.to_string().as_bytes())?;
      }
      Value::Num(num) => {
        w.write(num.to_string().as_bytes())?;
      }
      Value::Str(str) => {
        w.write(str.to_str(&ctx.s_store).as_str().as_bytes())?;
      }
      obj_val @ Value::Obj(map) => {
        let type_name = map.get_type();
        let type_function_name = ("#type_".to_string() + type_name).to_token();
        if let Some(funct) = functions.get(&type_function_name) {
          Self::interpret_type_function(funct, ctx, functions, obj_val, w)?;
        } else {
          w.write(type_name.as_bytes())?;
        }
      }
      _ => Default::default(),
    }
    Ok(())
  }

  fn interpret_type_function<W: Write>(
    funct: &Funct,
    ctx: &FormatterContext,
    functions: &HashMap<IString, Box<Funct>>,
    obj_val: Value,
    w: &mut W,
  ) -> SherpaResult<()> {
    if funct.params.len() != 1 {
      panic!("A type function should only have on param")
    }

    if funct.params[0].ty != "obj" {
      panic!("A type function param should have type 'obj'")
    }

    let ctx = &ctx;
    let mut ctx = FormatterContext::create_scope(ctx);
    ctx.set(Self::get_id_val(&funct.params[0].name).to_token(), obj_val);
    let result = Self::interpret_sequence(&funct.content, &mut ctx, functions, w)?;
    match result {
      None => Ok(()),
      Some((tail_call, funct)) => {
        Self::interpret_tail_call(tail_call, funct, &mut ctx, functions, w)?;
        Ok(())
      }
    }
  }

  fn interpret_function_call<'call, W: Write>(
    call: &'call Call,
    funct: &'call Funct,
    root_ctx: &mut FormatterContext,
    functions: &'call HashMap<IString, Box<Funct>>,
    w: &mut W,
  ) -> SherpaResult<()> {
    if call.args.len() != funct.params.len() {
      panic!("Args miss match")
    }

    let mut vals = Self::process_call_params(call, funct, root_ctx)?;

    let result = {
      let ctx = &root_ctx;
      let mut ctx = FormatterContext::create_scope(ctx);

      for (key, val) in vals.drain(..) {
        ctx.set(key, val)
      }

      match Self::interpret_sequence(&funct.content, &mut ctx, functions, w)? {
        None => {}
        Some((tail_call, funct)) => {
          Self::interpret_tail_call(tail_call, funct, &mut ctx, functions, w)?;
        }
      }
    };
    Ok(result)
  }

  fn interpret_tail_call<'call, W: Write>(
    call: &'call Call,
    mut funct: &'call Funct,
    root_ctx: &mut FormatterContext,
    functions: &'call HashMap<IString, Box<Funct>>,
    w: &mut W,
  ) -> SherpaResult<()> {
    if call.args.len() != funct.params.len() {
      panic!("Args miss match")
    }

    let mut vals = Self::process_call_params(call, funct, root_ctx)?;

    let result = loop {
      for (key, val) in vals.drain(..) {
        root_ctx.set(key, val)
      }
      match Self::interpret_sequence(&funct.content, root_ctx, functions, w)? {
        None => {
          break;
        }
        Some((tail_call, tail_funct)) => {
          vals = Self::process_call_params(tail_call, tail_funct, root_ctx)?;
          funct = tail_funct;
        }
      }
    };
    Ok(result)
  }

  fn process_call_params<'b, 'a: 'b, 'scope: 'a + 'b>(
    call: &'a Call,
    funct: &'a Funct,
    ctx: &'b mut FormatterContext<'scope>,
  ) -> Result<Vec<(IString, Value<'scope>)>, SherpaError> {
    let mut vals = vec![];

    for (arg, param) in call.args.iter().zip(funct.params.iter()) {
      let param_name = Self::get_id_val(&param.name);
      match Self::eval_expression(arg, ctx)? {
        num @ Value::Num(val) => match param.ty.as_str() {
          "flt" | "num" => {
            vals.push((param_name.intern(&ctx.s_store), num));
          }
          "int" => {
            vals.push((
              param_name.intern(&ctx.s_store),
              Value::Int(val.round().min(100000000000000.0).max(-1000000000000000.0) as isize),
            ));
          }
          _ => panic!("Numeric Value miss match param: {param_name}-{} val: num -{}", &param.ty, val),
        },
        int @ Value::Int(val) => match param.ty.as_str() {
          "num" | "flt" => {
            vals.push((param_name.intern(&ctx.s_store), Value::Num(val as f64)));
          }
          "int" => {
            vals.push((param_name.intern(&ctx.s_store), int));
          }
          val => panic!("Numeric Value miss match param: {param_name}-{} val: num -{}", &param.ty, val),
        },
        str @ Value::Str(_) => match param.ty.as_str() {
          "str" => {
            vals.push((param_name.intern(&ctx.s_store), str));
          }
          obj => {
            panic!("String Value miss match param: {param_name}-{} val: non-str -{:?}", &param.ty, obj)
          }
        },
        obj @ Value::Obj(_) => match param.ty.as_str() {
          "obj" => {
            vals.push((param_name.intern(&ctx.s_store), obj));
          }
          obj => panic!("Object type miss match param: \n  {param_name}-{} val: \n  non-obj -{:?}", &param.ty, obj),
        },
        node => todo!("Handle the interpretation of arg type: {node:#?}"),
      }
    }

    Ok(vals)
  }

  fn get_prop<'a>(obj: &'a dyn ValueObj, prop: &str, s_store: &IStringStore) -> Value<'a> {
    match prop {
      "len" => Value::Int(obj.get_len() as isize),
      _ => obj.get_val(prop, s_store),
    }
  }

  fn eval_expression<'a, 'scope: 'a>(expr: &parser::ASTNode, ctx: &'a FormatterContext<'scope>) -> SherpaResult<Value<'scope>> {
    use parser::ASTNode::*;
    let val = match expr {
      Obj(obj) => {
        let val = ctx.get(&Self::get_id_val(&obj.id).to_token());
        let val = match val {
          obj_val @ Value::Obj(obj_map) => {
            if obj.path.is_empty() {
              obj_val
            } else {
              let s_store = &ctx.s_store;
              let mut obj_map = Some(obj_map);
              let mut obj_val = Value::None;
              for (index, path) in obj.path.iter().enumerate() {
                if let Some(obj_map_unwrapped) = &obj_map {
                  match match path {
                    Prop(prop) => match prop.name.as_str() {
                      "len" => Value::Int(obj_map.unwrap().get_len() as isize),
                      prop_name => Self::get_prop(*obj_map_unwrapped, prop_name, s_store),
                    },
                    Index(idx) => match Self::eval_expression(&idx.expr, ctx)? {
                      Value::Num(i) => obj_map_unwrapped.get_index(i.min(0.0) as usize, s_store),
                      Value::Int(i) => obj_map_unwrapped.get_index(i as usize, s_store),
                      Value::Str(str) => Self::get_prop(*obj_map_unwrapped, &str.to_string(s_store), s_store),
                      _ => panic!("invalid property expression"),
                    },
                    _ => Value::None,
                  } {
                    o_v @ Value::Obj(obj) => {
                      obj_map = Some(obj);
                      obj_val = o_v;
                    }
                    Value::None => {
                      eprintln!(
                        "{}{} value is not an object {val:#?}",
                        Self::get_id_val(&obj.id),
                        obj.path[..=index].iter().map(|a| a.to_string()).collect::<Vec<_>>().join("")
                      );
                      obj_val = Value::None;
                      obj_map = None;
                    }
                    val => {
                      obj_val = val;
                      obj_map = None;
                    }
                  }
                } else {
                  panic!(
                    "property {}.{} is undefined",
                    Self::get_id_val(&obj.id),
                    obj.path[..=index].iter().map(|a| a.to_string()).collect::<Vec<_>>().join("")
                  )
                }
              }
              obj_val
            }
          }
          val => val,
        };

        val
      }
      Num(num) => {
        if num.val == "0" {
          Value::Num(0.0)
        } else if let Ok(val) = num.val.trim_start_matches('0').parse::<f64>() {
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
      Sub(sub) => match &Self::eval_expression(&sub.l, ctx)? {
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
      Mul(mul) => match &Self::eval_expression(&mul.l, ctx)? {
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
      Div(div) => match &Self::eval_expression(&div.l, ctx)? {
        Value::Num(left) => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(right) => Value::Num(left / right),
          Value::Int(right) => Value::Num(left / right as f64),
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
      Pow(div) => match &Self::eval_expression(&div.l, ctx)? {
        Value::Num(left) => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(right) => Value::Num(left.powf(right)),
          Value::Int(right) => Value::Num(left.powf(right as f64)),
          Value::Str(..) => panic!("Invalid div expression"),
          Value::Obj(..) => panic!("Invalid div expression"),
          _ => Value::None,
        },
        Value::Int(left) => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(right) => Value::Num((*left as f64).powf(right)),
          Value::Int(right) => Value::Int(left.pow(right as u32)),
          _ => panic!("Invalid div expression"),
        },
        Value::None => match Self::eval_expression(&div.r, ctx)? {
          Value::Num(_) => Value::Num(0 as f64),
          Value::Int(_) => Value::Int(0),
          _ => panic!("Invalid div expression"),
        },
        _ => panic!("Invalid div expression"),
      },
      literal @ Literal(_) => {
        let mut vec = Vec::with_capacity(512);
        Self::interpret_node_imut_ctx(literal, ctx, &Default::default(), &mut vec)?;
        let string = unsafe { String::from_utf8_unchecked(vec) };
        Value::Str(string.intern(&ctx.s_store))
      }
      node => todo!("Handle the interpretation of for expressions: {node:?}"),
    };

    Ok(val)
  }

  fn get_id_val(id: &parser::Id) -> &str {
    match id.at {
      true => &id.name[1..],
      false => &id.name,
    }
  }

  fn interpret_match<'call, W: Write>(
    m: &'call Match,
    ctx: &mut FormatterContext,
    functions: &'call HashMap<IString, Box<Funct>>,
    writer: &mut W,
  ) -> SherpaResult<Option<TailCall<'call>>> {
    let mut expressions = Vec::with_capacity(4);

    match &m.expr {
      parser::ASTNode::ExprTuple(tuple) => {
        for expr in &tuple.expressions {
          expressions.push(Self::eval_expression(expr, ctx)?)
        }
      }
      expr => expressions.push(Self::eval_expression(expr, ctx)?),
    };

    let (default, matches) = m.matches.iter().partition::<Vec<_>, _>(|f| f.default);

    if expressions.len() > 0 {
      fn compare_vals(input: &Value<'_>, expected: &Value<'_>) -> bool {
        match input {
          Value::Int(right) => match expected {
            Value::Int(left) => *left == *right,
            Value::Num(left) => *left == *right as f64,
            _ => false,
          },
          Value::Num(right) => match expected {
            Value::Int(left) => *left == *right as isize,
            Value::Num(left) => *left == *right,
            _ => false,
          },
          Value::Str(right) => match expected {
            Value::Str(left) => left == right,
            _ => false,
          },
          Value::None => matches!(expected, Value::None),
          v => todo!("handle {v:?}"),
        }
      }

      for m in matches {
        if match &m.match_expr {
          Some(parser::ASTNode::ExprTuple(exprs)) => {
            expressions.len() == exprs.expressions.len() && {
              let mut i = 0;
              loop {
                if i == expressions.len() {
                  break true;
                } else if !exprs.expressions[i].as_Ignore().is_some()
                  && !compare_vals(&expressions[i], &Self::eval_expression(&exprs.expressions[i], ctx)?)
                {
                  break false;
                }
                i += 1;
              }
            }
          }
          Some(expr) => !(expressions.len() > 1 || !compare_vals(&expressions[0], &Self::eval_expression(expr, ctx)?)),
          _ => false,
        } {
          return Self::interpret_sequence(&m.content, ctx, functions, writer);
        }
      }
    }

    for d in default {
      return Self::interpret_sequence(&d.content, ctx, functions, writer);
    }

    Ok(None)
  }
}

fn _simple_error(tok: Token, message: &str) -> SherpaError {
  SherpaError::SourceError {
    loc:        tok,
    path:       Default::default(),
    id:         (ErrorClass::Parsing, 102, "missing function").into(),
    msg:        message.to_string(),
    inline_msg: Default::default(),
    ps_msg:     Default::default(),
    severity:   sherpa_core::SherpaErrorSeverity::Critical,
  }
}

fn _hint_error(tok: Token, message: &str, hint: &str) -> SherpaError {
  SherpaError::SourceError {
    loc:        tok,
    path:       Default::default(),
    id:         (ErrorClass::Parsing, 102, "missing function").into(),
    msg:        message.to_string(),
    inline_msg: Default::default(),
    ps_msg:     hint.to_string(),
    severity:   sherpa_core::SherpaErrorSeverity::Critical,
  }
}

fn create_missing_function_error<T>(call: &parser::Call) -> Result<T, SherpaError> {
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

/// A call made at the end of execution sequence.
type TailCall<'call> = (&'call Call, &'call Funct);

#[derive(Default)]
struct BlockFormatter {
  lines:      Vec<(bool, Vec<u8>)>,
  curr_index: usize,
  len:        usize,
}

impl BlockFormatter {
  fn new() -> Self {
    let mut s = Self::default();
    s.add_line();
    s
  }

  fn add_line(&mut self) {
    self.lines.push((true, Vec::with_capacity(1024)));
    self.curr_index = self.lines.len() - 1;
  }

  fn flush_to_writer<W: Write>(self, join: bool, ctx: &FormatterContext, w: &mut W) -> SherpaResult<()> {
    const SPACE: u8 = ' ' as u8;
    const NEW_LINE: u8 = '\n' as u8;

    let tab_size = ctx.tab_size();
    let tab_count = ctx.indent_level();
    let nesting_depth = ctx.block_level();

    println!("nesting_depth:{nesting_depth}");

    let num_of_spaces = tab_size * tab_count + tab_size * nesting_depth;

    let mut prefix = Vec::with_capacity(num_of_spaces + 1);
    prefix.push(NEW_LINE);

    for _ in 0..num_of_spaces {
      prefix.push(SPACE);
    }

    for (add_prefix, line) in self.lines {
      if !join {
        if add_prefix {
          w.write(&prefix)?;
        }
        if let Some((trim_point, _)) = line.iter().enumerate().find(|(_, i)| **i != '\n' as u8 && **i != ' ' as u8) {
          w.write(&line.as_slice()[trim_point..])?;
        } else {
          w.write(line.as_slice())?;
        }
        if ctx.block_level() > 1 {
          // Communicate with the other block writer by indicating this is line
          // that should not be prefixed if join is required in the host block,
          // as it already contains the correct offset.
          w.write(&[1])?;
        }
      } else {
        w.write(&line)?;
      }
    }

    Ok(())
  }
}

impl Write for BlockFormatter {
  fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
    if buf.len() == 1 {
      if buf[0] == 0 {
        self.add_line();
        return Ok(1);
      } else if buf[0] == 1 {
        let (should_add_prefix, _) = &mut self.lines[self.curr_index];
        *should_add_prefix = false;
        return Ok(1);
      }
    }

    if buf.len() > 0 {
      self.len += buf.len();
      let (_, line) = &mut self.lines[self.curr_index];
      line.extend(buf)
    }

    Ok(buf.len())
  }

  fn flush(&mut self) -> std::io::Result<()> {
    Ok(())
  }
}
