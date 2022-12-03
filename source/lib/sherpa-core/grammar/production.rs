//! Utility functions for the evaluation, interpretation, and
//! comprehension of productions

/// Used to separate a grammar's uuid name from a production's name
const GUID_NAME_DELIMITER: &str = "_";

/// Generate a unique scanner production name givin a uuid production
/// name

pub fn create_scanner_name(uuid_production_name: &String) -> String {
  format!("scan_tok_{}_", uuid_production_name)
}

pub fn create_defined_scanner_name(uuid_production_name: &String) -> String {
  format!("scan_def_{}_", uuid_production_name)
}

/// Generate a UUID name using the grammars uuid_name and the
/// productions name (omitting local import name portion of a
/// production)

pub fn create_production_guid_name(grammar_uuid_name: &String, production_name: &String) -> String {
  grammar_uuid_name.to_owned() + GUID_NAME_DELIMITER + production_name
}
