use super::*;
use sherpa_core::{CachedString, IString, IStringStore};
use std::collections::HashMap;

#[derive(Debug)]
pub struct FormatterContext<'scope> {
  #[allow(unused)]
  pub(crate) parent:       Option<&'scope FormatterContext<'scope>>,
  pub(crate) values:       HashMap<IString, Value<'scope>>,
  pub(crate) tab_size:     usize,
  pub(crate) indent_level: usize,
  pub(crate) block_level:  usize,
  pub(crate) s_store:      IStringStore,
  pub max_width:           usize,
}

impl<'scope> FormatterContext<'scope> {
  pub fn new_with_values(values: &'scope dyn ValueObj, s_store: IStringStore) -> Self {
    let mut ctx = Self {
      tab_size:     2,
      indent_level: 0,
      block_level:  0,
      parent:       None,
      values:       Default::default(),
      s_store:      s_store,
      max_width:    120,
    };

    values.vals(&mut |k, v| {
      let k = k.intern(&ctx.s_store);
      ctx.set(k, v)
    });

    ctx
  }

  pub fn new() -> Self {
    Self {
      tab_size:     2,
      indent_level: 0,
      block_level:  0,
      parent:       None,
      values:       Default::default(),
      s_store:      Default::default(),
      max_width:    120,
    }
  }

  pub(crate) fn tab_size(&self) -> usize {
    self.tab_size
  }

  pub(crate) fn indent_level(&self) -> usize {
    self.indent_level
  }

  pub(crate) fn indent(&mut self) {
    self.indent_level += 1
  }

  pub(crate) fn dedent(&mut self) {
    if self.indent_level != 0 {
      self.indent_level -= 1
    }
  }

  pub(crate) fn block_level(&self) -> usize {
    self.block_level
  }

  pub(crate) fn block_open(&mut self) {
    self.block_level += 1
  }

  pub(crate) fn block_close(&mut self) {
    if self.block_level != 0 {
      self.block_level -= 1
    }
  }

  pub(crate) fn max_width(&self) -> usize {
    let curr_offset = self.indent_level * self.tab_size;
    if curr_offset > self.max_width {
      0
    } else {
      self.max_width - curr_offset
    }
  }

  pub(crate) fn create_scope<'a>(par: &'a Self) -> FormatterContext<'a> {
    FormatterContext {
      tab_size:     par.tab_size,
      indent_level: par.indent_level,
      block_level:  par.block_level,
      parent:       Some(par),
      values:       Default::default(),
      s_store:      par.s_store.clone(),
      max_width:    par.max_width,
    }
  }

  pub(crate) fn get(&self, id: &IString) -> Value<'scope> {
    self.values.get(id).cloned().unwrap_or_default()
  }

  pub(crate) fn set(&mut self, id: IString, val: Value<'scope>) {
    self.values.insert(id, val);
  }

  pub fn set_val(&mut self, id: &str, val: Value<'scope>) {
    let id = id.intern(&self.s_store);
    self.set(id, val)
  }
}
