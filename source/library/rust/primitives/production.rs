use super::SymbolID;

pub type ProductionId = u16;
pub type BodyId = u16;

#[derive(Debug, Clone)]
pub struct Production {
    pub name: String,
    pub number_of_bodies: u16,
    pub id: ProductionId,
    pub is_scanner: bool,
    pub is_entry: bool,
    pub is_recursive: bool,
    pub priority: u32,
}
pub type ProductionTable = std::collections::BTreeMap<ProductionId, Production>;
pub type ProductionEntryNamesTable = std::collections::BTreeMap<String, ProductionId>;
pub type ProductionBodiesTable = std::collections::BTreeMap<ProductionId, Vec<BodyId>>;

#[derive(Debug, Clone)]
pub struct BodySymbolRef {
    pub sym_id: SymbolID,
    pub original_index: u32,
    pub annotation: String,
    pub consumable: bool,
}

#[derive(Debug, Clone)]
pub struct Body {
    pub symbols: Vec<BodySymbolRef>,
    pub length: u16,
    pub production: ProductionId,
    pub id: BodyId,
}

pub type BodyTable = std::collections::BTreeMap<BodyId, Body>;
