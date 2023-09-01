use super::items::get_goal_items;
use crate::types::*;

pub(super) fn create_reduce_reduce_error(graph: &GraphHost, end_items: ItemSet) -> SherpaResult<()> {
  let _db = graph.get_db();
  let _goals = end_items.iter().flat_map(|i| get_goal_items(&graph, i)).collect::<OrderedSet<_>>();
  /*   j.report_mut().add_error(SherpaError::SourcesError {
    id:       "reduce-conflict",
    msg:      "Unresolvable parse conflict encountered".into(),
    ps_msg:   {
      let mut string = "Enable the following configs to use an alternative parse strategy".into();

      if !graph.is_scanner() {
      } else {
        let nterm = &goals.first().expect("Should have at least one goal").nonterm_index();
        let name = db.nonterm_guid_name_string(*nterm).as_str().to_string();

        string +=
          ("\n - Turn non-terminal <".to_string() + &name + " > into a PEG by using one of the PEG mode specifiers:").as_str();
        string += "\n    [ FIRST_MATCH | LONGEST_MATCH | SHORTEST_MATCH ]";
        string += format!("\n\n    Example: <> {name} FIRST_MATCH >  ...",).as_str();
      }
      string
    },
    severity: SherpaErrorSeverity::Critical,
    sources:  Default::default(),
  }); */

  SherpaResult::Ok(())
}
