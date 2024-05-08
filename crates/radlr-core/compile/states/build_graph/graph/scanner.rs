use crate::{
  proxy::{OrderedMap, OrderedSet},
  types::PrecedentDBTerm,
  CachedString,
  DBTermKey,
  IString,
  ParserDatabase,
};

#[derive(Clone, Default, Hash, PartialEq, Eq, Debug)]
pub struct ScannerData {
  pub hash:    u64,
  pub symbols: OrderedMap<PrecedentDBTerm, OrderedSet<PrecedentDBTerm>>,
  pub skipped: OrderedSet<DBTermKey>,
}

impl ScannerData {
  pub fn create_scanner_name(&self, db: &ParserDatabase) -> IString {
    ("scan".to_string() + &self.hash.to_string()).intern(db.string_store())
  }

  #[cfg(debug_assertions)]
  pub fn debug_print(&self, db: &ParserDatabase) {
    let Self { symbols, skipped, .. } = self;

    println!(
      "ScannerData\nSymbols:{}\nSkipped:{}",
      symbols.iter().map(|s| db.sym(s.0.tok()).debug_string(db)).collect::<Vec<_>>().join("\n"),
      skipped.iter().map(|s| db.sym(*s).debug_string(db)).collect::<Vec<_>>().join("\n")
    );
  }
}
