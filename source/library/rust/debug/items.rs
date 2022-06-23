use crate::primitives::GrammarStore;
use crate::primitives::Item;

pub fn debug_items(comment: &str, items: &[Item], grammar: &GrammarStore)
{
    println!("{} --> ", comment);

    for item in items {
        println!("    {}", item.debug_string(grammar));
    }
}
