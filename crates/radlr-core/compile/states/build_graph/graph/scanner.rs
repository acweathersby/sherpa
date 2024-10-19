use crate::{
  proxy::{OrderedMap, OrderedSet},
  types::PrecedentDBTerm,
  CachedString,
  DBTermKey,
  IString,
  GrammarDatabase,
};

#[derive(Clone, Default, Hash, PartialEq, Eq, Debug)]
pub struct ScannerData {
  pub hash:    u64,
  pub symbols: OrderedMap<PrecedentDBTerm, OrderedSet<PrecedentDBTerm>>,
  pub skipped: OrderedSet<DBTermKey>,
}

impl ScannerData {
  #[cfg(debug_assertions)]
  pub fn debug_print(&self, db: &GrammarDatabase) {
    let Self { symbols, skipped, .. } = self;

    println!(
      "ScannerData\nSymbols:{}\nSkipped:{}",
      symbols.iter().map(|s| db.sym(s.0.tok()).debug_string(db)).collect::<Vec<_>>().join("\n"),
      skipped.iter().map(|s| db.sym(*s).debug_string(db)).collect::<Vec<_>>().join("\n")
    );
  }
}

pub fn create_scanner_name(db: &GrammarDatabase, hash: u64) -> IString {
  format!("scan_{:016X}", hash).intern(db.string_store())
}
