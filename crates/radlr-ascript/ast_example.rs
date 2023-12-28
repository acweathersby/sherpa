
/// The official AST node trait.
trait ASTNode {
    
    fn type_name() -> &'static str;
}

/// A simple ast node. It should be able to allow quick traversal from itself
/// to any child nodes. 
#[derive(Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
struct ExampleAST {
    // Properties ...
    nodes: Vec<ChildType>,

    // Connection to the Underlying cst node. 
    // This is only active if the CST is produced alongside
    // the AST. 
    cst: Option<(Rc<CSTHost>, usize)> 
}

impl ExampleAST {
    fn new(/* params ... */) -> Box<Self> {

    }
}

struct CSNode {
    name: String,
    parent:Option<usize>,
    children: Vec<usize>
}

struct CSHost {
    nodes: Vec<CSNode>
}


enum AST {
}
