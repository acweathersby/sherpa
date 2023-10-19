use crate::{
  parser::{self, ASTNode, Call, Funct, Match},
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
  functions: HashMap<IString, Box<FunctionContext>>,
}
#[derive(Debug)]
pub struct FunctionContext {
  f:         Box<Funct>,
  functions: HashMap<IString, Box<FunctionContext>>,
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

pub(crate) type Functions = HashMap<IString, Box<FunctionContext>>;

enum ObjectEvalResult<'ctx: 'fn_scope, 'fn_scope> {
  Value(Value<'ctx>),
  TypeCall(&'ctx dyn ValueObj, &'fn_scope [parser::ASTNode]),
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
    Self { functions: Self::get_functions(&script), script }
  }

  pub fn get_functions(nodes: &[ASTNode]) -> HashMap<IString, Box<FunctionContext>> {
    let mut map = HashMap::default();
    use parser::ASTNode::*;
    for n in nodes.iter() {
      match n {
        Funct(funct) => {
          let functions = Self::get_functions(&funct.content);

          let sig_name = funct.name.clone()
            + &funct
              .params
              .iter()
              .map(|p| match p.ty.as_str() {
                "num" | "int" | "flt" => "_num".to_string(),
                _ => "_".to_string() + &p.ty,
              })
              .collect::<Vec<_>>()
              .join("");

          let name = sig_name.to_token();
          map.insert(name, Box::new(FunctionContext { f: funct.clone(), functions }));
        }
        _ => {}
      }
    }
    map
  }

  #[cfg(debug_assertions)]
  pub fn _debug_print_(&self) {
    println!("{:#?}", self.script)
  }

  pub fn write_to_string<'a>(&'a self, ctx: &mut FormatterContext<'_, 'a>, capacity: usize) -> SherpaResult<String> {
    unsafe { Ok(String::from_utf8_unchecked(self.write_to_output(ctx, Vec::with_capacity(capacity))?)) }
  }

  pub fn write_to_output<'a, W: Write>(&'a self, ctx: &mut FormatterContext<'_, 'a>, output: W) -> SherpaResult<W> {
    let sequence: &Vec<parser::ASTNode> = &self.script;
    ctx.functs = Some(&self.functions);
    let result = Self::write_to_output_internal(sequence, ctx, output);
    ctx.functs = None;
    result
  }

  fn write_to_output_internal<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    sequence: &'fn_scope Vec<parser::ASTNode>,
    ctx: &mut FormatterContext<'ctx, 'fn_scope>,
    mut output: W,
  ) -> Result<W, SherpaError> {
    match Self::interpret_sequence(sequence, ctx, &mut output)? {
      None => (),
      Some(tail_call) => {
        Self::interpret_function_call(tail_call, ctx, &mut output)?;
      }
    }
    output.flush()?;
    Ok(output)
  }

  fn interpret_sequence<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    seq: &'fn_scope [parser::ASTNode],
    ctx: &mut FormatterContext<'ctx, 'fn_scope>,
    w: &mut W,
  ) -> SherpaResult<Option<TailCall<'fn_scope>>> {
    use parser::*;

    let mut iter = seq.iter().peekable();

    let mut tail_call = None;

    while let Some(node) = iter.next() {
      if let Some(call) = tail_call.take() {
        Self::interpret_function_call(call, ctx, w)?
      }

      if node.as_NewLine().is_some() {
        // Re-order indentation ahead of newlines
        while let Some(la) = iter.peek() {
          match la {
            ASTNode::Indent(_) | ASTNode::Dedent(_) => {
              match Self::interpret_node_mut_ctx(la, ctx, w)? {
                None => {}
                _ => unreachable!(),
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
        tail_call = Some(node.as_Call().unwrap())
      } else {
        match Self::interpret_node_mut_ctx(node, ctx, w)? {
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
      Obj(o) => match o.id.at {
        true => match Self::eval_obj(&o, ctx)? {
          ObjectEvalResult::Value(val) => {
            Self::print_value(val, ctx, w)?;
          }
          ObjectEvalResult::TypeCall(obj, args) => {
            if !Self::interpret_type_function(obj.get_type(), args, ctx, Value::Obj(obj), w)? {
              w.write(o.tok.to_string().as_bytes())?;
            }
          }
        },
        false => {
          w.write(o.tok.to_string().as_bytes())?;
        }
      },
      Expression(expr) => {
        let val = Self::eval_expression(&expr.val, ctx)?;
        Self::print_value(val, ctx, w)?;
      }
      node => todo!("Handle the interpretation of: {node:#?}"),
    };
    Ok(None)
  }

  /// Prints nodes that may or may not modify the calling context.
  fn interpret_node_mut_ctx<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    node: &'fn_scope parser::ASTNode,
    ctx: &mut FormatterContext<'ctx, 'fn_scope>,
    w: &mut W,
  ) -> SherpaResult<Option<TailCall<'fn_scope>>> {
    use parser::ASTNode::*;
    match node {
      Call(call) => Self::interpret_function_call(call, ctx, w)?,
      Match(m) => return Self::interpret_match(m, ctx, w),
      Dedent(_) => {
        ctx.dedent();
      }
      Indent(_) => {
        ctx.indent();
      }
      LiteralSpace(l_space) => return Self::interpret_sequence(&l_space.content, ctx, w),
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
        Self::write_to_output_internal(&block.content, ctx, &mut writer)?;
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
        let expr = Self::eval_expression(&assign.expr, ctx)?;
        ctx.set(Self::get_id_val(&assign.id).to_token(), expr);
      }
      node => return Self::interpret_node_imut_ctx(node, ctx, w),
    };
    Ok(None)
  }

  fn print_value<W: Write>(val: Value, ctx: &FormatterContext, w: &mut W) -> SherpaResult<()> {
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
        if Self::interpret_type_function(type_name, &[], ctx, obj_val, w)? {
        } else {
          w.write(type_name.as_bytes())?;
        }
      }
      _ => Default::default(),
    }
    Ok(())
  }

  /// Returns `true` if a call was made to function with a matching signature.
  fn interpret_type_function<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    type_name: &str,
    args: &'fn_scope [ASTNode],
    ctx: &FormatterContext<'ctx, 'fn_scope>,
    obj_val: Value<'fn_scope>,
    w: &mut W,
  ) -> SherpaResult<bool> {
    let (name_sig, args) = Self::create_function_sig(&("#type_".to_string() + type_name), &args, ctx)?;
    if let Some(funct_ctx) = Self::resolve_function_call_target(name_sig, ctx) {
      let ctx = &ctx;
      let mut ctx = FormatterContext::create_scope(ctx);
      ctx.functs = Some(&funct_ctx.functions);
      ctx.set("self".to_token(), obj_val);
      Self::set_funct_variables(funct_ctx, args, &mut ctx);
      let result = Self::interpret_sequence(&funct_ctx.f.content, &mut ctx, w)?;
      match result {
        None => Ok(true),
        Some(tail_call) => {
          Self::interpret_tail_call(tail_call, &mut ctx, w)?;
          Ok(true)
        }
      }
    } else {
      Ok(false)
    }
  }

  fn resolve_function_call_target<'call: 'fn_scope, 'fn_scope>(
    call_name: IString,
    root_ctx: &FormatterContext<'call, 'fn_scope>,
  ) -> Option<&'fn_scope FunctionContext> {
    if let Some(functions) = root_ctx.functs {
      match functions.get(&call_name) {
        Some(funct) => Some(&funct),
        _ => {
          if let Some(parent) = root_ctx.parent {
            Self::resolve_function_call_target(call_name, parent)
          } else {
            None
          }
        }
      }
    } else {
      None
    }
  }

  fn create_function_sig<'ctx: 'fn_scope, 'fn_scope>(
    name: &str,
    call_args: &'fn_scope [ASTNode],
    ctx: &FormatterContext<'ctx, 'fn_scope>,
  ) -> SherpaResult<(IString, Vec<Value<'ctx>>)> {
    let mut name = name.to_string();
    let mut vals = vec![];
    for arg in call_args.iter() {
      match Self::eval_expression(arg, ctx)? {
        num @ Value::Num(_) => {
          name += "_num";
          vals.push(num);
        }
        int @ Value::Int(_) => {
          name += "_num";
          vals.push(int);
        }
        str @ Value::Str(_) => {
          name += "_str";
          vals.push(str);
        }
        obj @ Value::Obj(_) => {
          name += "_obj";
          vals.push(obj);
        }
        node => todo!("Handle the interpretation of arg type: {node:#?}"),
      }
    }

    Ok((name.intern(&ctx.s_store), vals))
  }

  fn set_funct_variables<'ctx: 'fn_scope, 'fn_scope>(
    funct_ctx: &FunctionContext,
    args: Vec<Value<'ctx>>,
    root_ctx: &mut FormatterContext<'ctx, 'fn_scope>,
  ) {
    for (param, arg) in funct_ctx.f.params.iter().zip(args.into_iter()) {
      let key = Self::get_id_val(&param.name).intern(&root_ctx.s_store);
      let val = match arg {
        num @ Value::Num(val) => match param.ty.as_str() {
          "flt" | "num" => num,
          "int" => Value::Int(val.round().min(100000000000000.0).max(-1000000000000000.0) as isize),
          _ => unreachable!(),
        },
        int @ Value::Int(val) => match param.ty.as_str() {
          "num" | "flt" => Value::Num(val as f64),
          "int" => int,
          _ => unreachable!(),
        },
        v => v,
      };
      root_ctx.set(key, val)
    }
  }

  fn interpret_function_call<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    call: &'fn_scope Call,
    root_ctx: &mut FormatterContext<'ctx, 'fn_scope>,
    w: &mut W,
  ) -> SherpaResult<()> {
    let (name_sig, args) = Self::create_function_sig(&call.name, &call.args, root_ctx)?;
    if let Some(funct_ctx) = Self::resolve_function_call_target(name_sig, root_ctx) {
      let result = {
        let ctx = &root_ctx;
        let mut ctx = FormatterContext::create_scope(ctx);
        ctx.functs = Some(&funct_ctx.functions);

        Self::set_funct_variables(funct_ctx, args, &mut ctx);

        match Self::interpret_sequence(&funct_ctx.f.content, &mut ctx, w)? {
          None => {}
          Some(tail_call) => {
            Self::interpret_tail_call(tail_call, &mut ctx, w)?;
          }
        }
      };

      Ok(result)
    } else {
      Err(SherpaError::StaticText("Function not defined on context"))
    }
  }

  fn interpret_tail_call<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    call: &'fn_scope Call,
    root_ctx: &mut FormatterContext<'ctx, 'fn_scope>,
    w: &mut W,
  ) -> SherpaResult<()> {
    let mut call = call;
    loop {
      let (name_sig, args) = Self::create_function_sig(&call.name, &call.args, root_ctx)?;
      if let Some(funct_ctx) = Self::resolve_function_call_target(name_sig, root_ctx) {
        if call.args.len() != funct_ctx.f.params.len() {
          panic!("Args miss match")
        }

        Self::set_funct_variables(funct_ctx, args, root_ctx);

        match Self::interpret_sequence(&funct_ctx.f.content, root_ctx, w)? {
          None => {
            return Ok(());
          }
          Some(tail_call) => {
            call = tail_call;
          }
        }
      } else {
        return Err(SherpaError::StaticText("Function not defined on context"));
      }
    }
  }

  fn get_prop<'a>(obj: &'a dyn ValueObj, prop: &str, s_store: &IStringStore) -> Value<'a> {
    match prop {
      "len" => Value::Int(obj.get_len() as isize),
      _ => obj.get_val(prop, s_store),
    }
  }

  fn eval_obj<'ctx: 'fn_scope, 'fn_scope>(
    obj: &'fn_scope parser::Obj,
    ctx: &FormatterContext<'ctx, 'fn_scope>,
  ) -> Result<ObjectEvalResult<'ctx, 'fn_scope>, SherpaError> {
    use parser::ASTNode::*;
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
                TypeCall(tc) => {
                  return Ok(ObjectEvalResult::TypeCall(*obj_map_unwrapped, &tc.expressions));
                }
                Type(_) => Value::Str(obj_map.unwrap().get_type().intern(&ctx.s_store)),
                Length(_) => Value::Int(obj_map.unwrap().get_len() as isize),
                Prop(prop) => Self::get_prop(*obj_map_unwrapped, prop.name.as_str(), s_store),
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
              eprintln!(
                "property {}.{} is undefined",
                Self::get_id_val(&obj.id),
                obj.path[..=index].iter().map(|a| a.to_string()).collect::<Vec<_>>().join("")
              );
              obj_val = Value::None;
              break;
            }
          }
          obj_val
        }
      }
      val => val,
    };
    Ok(ObjectEvalResult::Value(val))
  }

  fn eval_expression<'ctx: 'fn_scope, 'fn_scope>(
    expr: &'fn_scope parser::ASTNode,
    ctx: &FormatterContext<'ctx, 'fn_scope>,
  ) -> SherpaResult<Value<'ctx>> {
    use parser::ASTNode::*;
    let val = match expr {
      Obj(obj) => match Self::eval_obj(obj, ctx)? {
        ObjectEvalResult::Value(val) => val,
        ObjectEvalResult::TypeCall(obj, args) => {
          let mut writer = Vec::with_capacity(1024);
          Self::interpret_type_function(obj.get_type(), args, ctx, Value::Obj(obj), &mut writer)?;
          Value::Str(unsafe { String::from_utf8_unchecked(writer) }.intern(&ctx.s_store))
        }
      },
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
        Self::interpret_node_imut_ctx(literal, ctx, &mut vec)?;
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

  fn interpret_match<'ctx: 'fn_scope, 'fn_scope, W: Write>(
    m: &'fn_scope Match,
    ctx: &mut FormatterContext<'ctx, 'fn_scope>,
    writer: &mut W,
  ) -> SherpaResult<Option<TailCall<'fn_scope>>> {
    let mut expressions = Vec::with_capacity(4);

    for expr in &m.expr.expressions {
      expressions.push(Self::eval_expression(expr, ctx)?)
    }

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
          Some(exprs) => {
            expressions.len() == exprs.expressions.len() && {
              let mut i = 0;
              loop {
                if i == expressions.len() {
                  break true;
                } else if exprs.expressions[i].as_Ignore().is_some() {
                } else if exprs.expressions[i].as_NotNone().is_some() {
                  if !matches!(expressions[i], Value::None) {
                    break false;
                  }
                } else if !compare_vals(&expressions[i], &Self::eval_expression(&exprs.expressions[i], ctx)?) {
                  break false;
                }
                i += 1;
              }
            }
          }
          _ => false,
        } {
          return Self::interpret_sequence(&m.content, ctx, writer);
        }
      }
    }

    for d in default {
      return Self::interpret_sequence(&d.content, ctx, writer);
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
type TailCall<'call> = &'call Call;

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
