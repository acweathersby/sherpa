pub mod state_construct;
pub mod transition_tree;

#[cfg(test)]

mod transition_tree_tests
{

    use crate::debug::compile_test_grammar;
    use crate::grammar::get_production_by_name;
    use crate::grammar::get_production_plain_name;
    use crate::grammar::get_production_start_items;
    use crate::intermediate::transition_tree::construct_recursive_descent;

    #[test]
    pub fn test_construct_descent_on_basic_grammar()
    {
        let grammar = compile_test_grammar(
            "<> A > \\h \\e
         \\l \\l \\o",
        );

        let production_id = get_production_by_name("A", &grammar).unwrap();

        let result = construct_recursive_descent(
            &grammar,
            false,
            &get_production_start_items(&production_id, &grammar),
        );

        assert_eq!(result.get_node_len(), 7);

        assert_eq!(result.leaf_nodes.len(), 1);
    }

    #[test]
    pub fn test_construct_descent_on_scanner_symbol()
    {
        let grammar = compile_test_grammar(
            "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
        );

        let production = grammar
            .production_table
            .iter()
            .filter(|p| p.1.is_scanner)
            .next()
            .unwrap();

        let production_id = production.0;

        let result = construct_recursive_descent(
            &grammar,
            true,
            &get_production_start_items(production_id, &grammar),
        );

        assert_eq!(result.get_node_len(), 9);

        assert_eq!(result.leaf_nodes.len(), 2);
    }
}
