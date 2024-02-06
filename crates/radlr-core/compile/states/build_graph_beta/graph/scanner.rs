use crate::{proxy::OrderedSet, types::PrecedentDBTerm, CachedString, DBTermKey, IString, ParserDatabase};

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Default, Hash)]
pub struct ScannerData {
  pub hash:    u64,
  pub symbols: OrderedSet<PrecedentDBTerm>,
  pub skipped: OrderedSet<DBTermKey>,
  pub follow:  OrderedSet<PrecedentDBTerm>,
}

impl ScannerData {
  pub fn create_scanner_name(&self, db: &ParserDatabase) -> IString {
    ("scan".to_string() + &self.hash.to_string()).intern(db.string_store())
  }
}
