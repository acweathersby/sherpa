use crate::types::GrammarStore;
use crate::types::Item;

pub fn debug_items<T: IntoIterator<Item = Item>>(comment: &str, items: T, g: &GrammarStore) {
  eprintln!("{} --> ", comment);

  for item in items {
    eprintln!("    {}", item.debug_string(g));
  }
}
