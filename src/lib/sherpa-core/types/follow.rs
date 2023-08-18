use super::*;

pub(crate) type _FollowSets<'db> = Array<OrderedSet<Item<'db>>>;

pub fn _create_follow_sets<'db>(db: &'db ParserDatabase) -> _FollowSets<'db> {
  let mut follow = Array::with_capacity(db.prod_len());

  for _ in 0..db.prod_len() {
    follow.push(OrderedSet::new())
  }

  for prod_id in 0..db.prod_len() {
    for mut item in Items::start_items(prod_id.into(), db) {
      loop {
        match item.get_type() {
          ItemType::NonTerminal(prod_id) | ItemType::TokenNonTerminal(prod_id, ..) => {
            let index: usize = prod_id.into();
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
