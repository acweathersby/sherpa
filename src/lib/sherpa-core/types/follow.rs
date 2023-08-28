use super::*;

pub(crate) type _FollowSets<'db> = Array<OrderedSet<Item<'db>>>;

pub fn _create_follow_sets<'db>(db: &'db ParserDatabase) -> _FollowSets<'db> {
  let mut follow = Array::with_capacity(db.nonterms_len());

  for _ in 0..db.nonterms_len() {
    follow.push(OrderedSet::new())
  }

  for nterm in 0..db.nonterms_len() {
    for mut item in Items::start_items(nterm.into(), db) {
      loop {
        match item.get_type() {
          ItemType::NonTerminal(nterm) | ItemType::TokenNonTerminal(nterm, ..) => {
            let index: usize = nterm.into();
            follow[index].insert(item.increment().unwrap());
          }
          _ => {}
        }

        if let Some(i) = item.increment() {
          item = i;
        } else {
          break;
        }
      }
    }
  }
  follow
}
