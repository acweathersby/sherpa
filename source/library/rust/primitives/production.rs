struct Production {
    name: String,
    number_of_bodies: u16,
    id: u16,
    is_scanner: bool,
    is_entry: bool,
    is_recursive: bool,
    priority: u32,
}

type ProductionEntryNamesTable = BtreeMap<u16, (u16, String)>;

struct Body {
    symbols: Vec<SymbolID>,
    length: u16,
    production: u16,
    id: u16,
}
