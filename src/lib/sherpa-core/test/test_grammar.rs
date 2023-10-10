use crate::{test::utils::build_parse_states_from_source_str as build, DBNonTermKey, SherpaResult as R, TestPackage};

#[test]
fn grammar_viable_grammar() -> R<()> {
  build("<> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_export_clause() -> R<()> {
  build("EXPORT a as b <> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_import_clause() -> R<()> {
  build("IMPORT a as b <> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_ignore_clause() -> R<()> {
  build("IGNORE { c:sp } <> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_name_clause() -> R<()> {
  build("NAME test <> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_peg_nonterminal() -> R<()> {
  build(":> a > 'b'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_append_nonterminal() -> R<()> {
  build("<> t > ('r') \n +> t > ( 'b' :ast 1 )", "".into(), Default::default(), &|TestPackage { db, .. }| {
    assert_eq!(
      db.nonterm_rules(DBNonTermKey::from(0usize))?.len(),
      2,
      "Non-terminal `t` should have two rules, one for `'r'` and the other for `'b'`"
    );
    R::Ok(())
  })
}

#[test]
fn grammar_id_grammar() -> R<()> {
  build("<> a > c:id", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_templates() -> R<()> {
  build("<C> a > 't' C <> b > a::<B> <> B > 'r'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_keyword() -> R<()> {
  build("<> a > 't'{kw} ", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_with_token_group_keywords() -> R<()> {
  build("<> a > tk:( 't' ) ", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_num_grammar() -> R<()> {
  build("<> a > c:num", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_sp_grammar() -> R<()> {
  build("<> a > c:sp", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_nl_grammar() -> R<()> {
  build("<> a > c:nl", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_optional_symbol() -> R<()> {
  build("<> a > c:sp? \"test\" ", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_list_symbol() -> R<()> {
  build("<> a > c:sp(+) ", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_list_symbol_with_separator() -> R<()> {
  build("<> a > c:sp(+ ',' ) ", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_optional_list_symbol() -> R<()> {
  build("<> a > c:sp(*) \"trail\"", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_optional_list_symbol_with_separator() -> R<()> {
  build("<> a > c:sp(* ',') \"trail\"", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_symbol_with_precedence() -> R<()> {
  build("<> a > c:sp{2}", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_symbol_with_reference() -> R<()> {
  build("<> a > c:sp^space", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_symbol_with_optional() -> R<()> {
  build("<> a > c:sp? c:sp", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_symbol_with_all_annotations_1() -> R<()> {
  build("<> a > c:sp?^a{2} c:sp", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_symbol_with_all_annotations_2() -> R<()> {
  build("<> a > c:sp^a{2}? c:sp", "".into(), Default::default(), &|_| R::Ok(()))
}
#[test]
fn grammar_optional_symbol_with_all_annotations_3() -> R<()> {
  build("<> a > c:sp^a?{2} c:sp", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_token_nonterminal() -> R<()> {
  build("<> a > tk:b <> b > tk:c <> c > 'c'", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_eof_symbol() -> R<()> {
  build("<> a > c:sp $ ", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_variable_set() -> R<()> {
  build("<> a > [ c:sp? c:num? ]", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_unordered_variable_set() -> R<()> {
  build("<> a > [ c:sp? c:num? ]!", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_anonymous_nonterminal() -> R<()> {
  build("<> a > [ c:sp? c:num? ]!", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ascript_struct_declaration() -> R<()> {
  build("<> a > \"b\" :ast { t_B, b:$1 }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_sub_struct() -> R<()> {
  build("<> a > \"b\" :ast { t_A, b: { t_B, b } }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ascript_struct_with_auto_bind_property() -> R<()> {
  build("<> a > \"b\"^b :ast { t_B, b }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ascript_struct_with_tok_declaration() -> R<()> {
  build("<> a > \"b\" :ast { t_B, tok }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ascript_struct_with_token_declaration() -> R<()> {
  build("<> a > \"b\" :ast { t_B, token }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ascript_struct_with_tk_declaration() -> R<()> {
  build("<> a > \"b\" :ast { t_B, tk }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ascript_struct_with_token_range_declaration() -> R<()> {
  build("<> a > \"b\" :ast { t_B, tok<2,2> }", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_str() -> R<()> {
  build("<> a > \"b\" :ast str($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_u8() -> R<()> {
  build("<> a > \"b\" :ast u8($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_u16() -> R<()> {
  build("<> a > \"b\" :ast u16($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_u32() -> R<()> {
  build("<> a > \"b\" :ast u32($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_u64() -> R<()> {
  build("<> a > \"b\" :ast u64($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_i8() -> R<()> {
  build("<> a > \"b\" :ast i8($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_i16() -> R<()> {
  build("<> a > \"b\" :ast i16($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_i32() -> R<()> {
  build("<> a > \"b\" :ast i32($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_i64() -> R<()> {
  build("<> a > \"b\" :ast i64($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_f32() -> R<()> {
  build("<> a > \"b\" :ast f32($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_f64() -> R<()> {
  build("<> a > \"b\" :ast f64($1)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_expression_bool() -> R<()> {
  build("<> a > \"b\"^b :ast bool($b)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_bool_literal_true() -> R<()> {
  build("<> a > \"b\" :ast true", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_bool_literal_false() -> R<()> {
  build("<> a > \"b\" :ast false", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_vector() -> R<()> {
  build("<> a > \"b\" \"b\" :ast [$1, $2]", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_add() -> R<()> {
  build("<> a > \"b\" \"b\" :ast $1 + $2", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_map() -> R<()> {
  build("<> a > \"b\" \"b\" :ast map($1,$2)", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_basic_ascript_member() -> R<()> {
  build("<> a > \"b\" \"b\" :ast $1.test", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ir_state_nonterminal() -> R<()> {
  build("test => pass", "".into(), Default::default(), &|_| R::Ok(()))
}

#[test]
fn grammar_ir_catch_state_nonterminal() -> R<()> {
  build("test =!> fail", "".into(), Default::default(), &|_| R::Ok(()))
}
