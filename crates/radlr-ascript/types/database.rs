use crate::{build_db, AscriptAnys, AscriptRules, AscriptScalarType, AscriptStructs, AscriptTypes};
use radlr_core::{proxy::OrderedSet, CachedString, IString, ParserDatabase, RadlrDatabase, RadlrError, RadlrResult};
use radlr_formatter::*;
use std::{fmt::Debug, io::Write, sync::Arc};

pub type AnyTypeRef = (String, OrderedSet<AscriptScalarType>);

/// Stores resolved type and construction information used to create
/// AST constructors based on `:ast` expressions extracted from a
/// grammar.
pub struct AscriptDatabase {
  pub(crate) structs:     AscriptStructs,
  /// If errors are present then it is likely that the
  /// database is in an incomplete state and SHOULD NOT be
  /// used to construct parser outputs.
  pub(crate) errors:      Vec<RadlrError>,
  /// Initializing information for converting a rules into AST nodes.
  pub(crate) rules:       AscriptRules,
  /// A set of all non-struct types produced by the the grammar's rules
  pub(crate) types:       AscriptTypes,
  /// A set of `ANY` type groups that should be converted into special enum
  /// types.
  pub(crate) any_type_lu: Vec<usize>,
  /// A set of `ANY` type groups that should be converted into special enum
  /// types.
  pub(crate) any_types:   Vec<AnyTypeRef>,
  ///  The underlying grammar database from which Ascript types are
  /// derived.
  pub(crate) db:          Arc<ParserDatabase>,
}

impl Debug for AscriptDatabase {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("AscriptDatabase");
    s.field("rules", &self.rules);
    s.field("structs", &self.structs);
    s.field("types", &self.types);
    s.field("erros", &self.errors);
    s.field("any_type_lu", &self.any_type_lu);
    s.field("any_types", &self.any_types);
    s.finish()
  }
}

impl AscriptDatabase {
  /// Format and print this database to a string using the a formatting
  /// script.
  pub fn format_string(&self, script: &str, max_width: usize, ast_struct_name: &str) -> RadlrResult<String> {
    unsafe { Ok(String::from_utf8_unchecked(self.format(script, Vec::with_capacity(1024), max_width, ast_struct_name)?)) }
  }

  /// Format and print this database to the given buffer using a formatting
  /// script.
  pub fn format<W: Write>(&self, script: &str, writer: W, max_width: usize, ast_struct_name: &str) -> RadlrResult<W> {
    let mut ctx: FormatterContext = FormatterContext::new("AscriptForm", self.db.string_store().clone());

    let any = AscriptAnys::new(self);
    ctx.set_val("STRUCTS", Value::Obj(&self.structs));
    ctx.set_val("TYPES", Value::Obj(&self.types));
    ctx.set_val("RULES", Value::Obj(&self.rules));
    ctx.set_val("ANY_ENUMS", Value::Obj(&any));
    ctx.set_val("AST_NAME", Value::Str(ast_struct_name.intern(self.db.string_store())));
    ctx.max_width = max_width;

    let f: Formatter = FormatterResult::from(script).into_result()?;
    f.write_to_output(&mut ctx, writer)
  }

  pub fn get_errors(&self) -> Option<&[RadlrError]> {
    if self.errors.len() > 0 {
      Some(&self.errors)
    } else {
      None
    }
  }
}

impl ValueObj for AscriptDatabase {
  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptDatabase"
  }

  fn get_val<'scope>(&'scope self, key: &str, _: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      _ => Value::None,
    }
  }
}

impl From<&RadlrDatabase> for AscriptDatabase {
  fn from(value: &RadlrDatabase) -> Self {
    build_db::build_database((*value).clone())
  }
}

impl From<RadlrDatabase> for AscriptDatabase {
  fn from(value: RadlrDatabase) -> Self {
    build_db::build_database(value)
  }
}

#[derive(Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct StringId(pub(crate) IString);

impl ToValue for StringId {
  fn into_val<'scope>(&'scope self, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    self.0.into_val(s_store)
  }
}

impl AsRef<IString> for StringId {
  fn as_ref(&self) -> &IString {
    &self.0
  }
}

impl Into<IString> for StringId {
  fn into(self) -> IString {
    self.0
  }
}

impl From<&str> for StringId {
  fn from(value: &str) -> Self {
    Self(value.to_token())
  }
}

impl From<String> for StringId {
  fn from(value: String) -> Self {
    Self(value.to_token())
  }
}
