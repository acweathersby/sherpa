import { EditGraph, JSBytecodeParserDB, JSCSTNode, JSPatchResult } from "js/sherpa/sherpa_wasm";

class CSTNode {
  /// The UI representation of this node.  
  ele: HTMLElement;

  parent: CSTNode | null

  constructor(ele_type: string) {
    this.ele = document.createElement(ele_type);
    this.parent = null
  }

  destruct() { }

  attachToEle(ele: HTMLElement) {
    ele.appendChild(this.ele)
  }

  getGraph(): null | EditGraph {
    if (this.parent) {
      return this.parent.getGraph()
    } else {
      return null
    }
  }

  addToMap(node: CSTNode = this) {
    if (this.parent) {
      this.parent.addToMap(node);
    }
  }

  removeFromMap(node: CSTNode = this) {
    if (this.parent) {
      this.parent.removeFromMap(node);
    }
  }

  offset(): number {
    if (this.parent instanceof CSTNodeNonTerm) {
      let offset = 0;
      for (const c of this.parent.children) {
        if (c == this)
          break
        offset += c.len()
      }

      return offset + this.parent.offset()
    } else {
      return 0
    }
  }

  len(): number {
    return 0
  }

  clear() { }
}

class CSTNodeTerminal extends CSTNode {

  node: JSCSTNode;

  constructor(par: CSTNode, node: JSCSTNode) {
    super("span")
    this.parent = par;
    this.node = node;

    this.ele.contentEditable = "true";
    this.ele.classList.add("cst-node");
    this.ele.addEventListener("beforeinput", <any>this.edited.bind(this));

    switch (node.get_type()) {
      case "Errata":
        this.ele.classList.add("cst-tk", "cst-errata")
        break
      case "Token":
        this.ele.classList.add("cst-tk")
        break
      case "Missing":
        this.ele.classList.add("cst-tk", "cst-missing")
        break
    }

    let graph = this.getGraph();

    if (!graph) throw "Graph is not defined";

    this.ele.innerText = node.get_text(graph);

    this.addToMap();
  }

  destruct() { }

  edited(e: InputEvent) {
    e.preventDefault();
    e.stopImmediatePropagation();
    return true;
  }

  len(): number {
    return this.node.len()
  }

  clear() {
    this.node.free();
    this.ele.innerHTML = "";
  }
}

function getIndex(len: number): number[] {
  return [...Array(len).keys()]
}



function setupChildren(nt: CSTNodeNonTerm, cst_nodes: (JSCSTNode | undefined)[]) {
  let node = nt.node;
  let children = [];

  if (node) {
    if (node.get_type() != "Nonterm")
      throw "Root is not Nonterm: " + node.get_type();

    for (const child_node of cst_nodes) {

      switch (child_node?.get_type()) {
        case "Nonterm":
          children.push(new CSTNodeNonTerm(nt, child_node));
          break;
        case "Alternative":
          console.log("TODO: Build Alternative child");
          break;
        case "Alternatives":
          console.log("TODO: Build Alternatives child");
          break;
        case "Errata":
        case "Token":
        case "Missing": {
          children.push(new CSTNodeTerminal(nt, child_node));
        }
          break;
        case "None":
          throw "Child is None!";
      }
    }
  }
  return children;
}

class CSTNodeNonTerm extends CSTNode {
  /// Child nodes of this element. 
  children: CSTNode[];

  node: JSCSTNode;

  constructor(par: CSTNode, node: JSCSTNode) {
    super("div")

    this.ele.style.display = "inline-block";
    this.ele.classList.add("cst-nt");
    this.parent = par;
    this.node = node;
    if (node) {
      this.children = setupChildren(this, getIndex(node.num_of_children()).map(i => node.child_at(i)));
    } else {
      this.children = []
    }
  }


  reloadElement() {
    this.ele.innerHTML = ""

    for (const c of this.children) {
      if (c instanceof CSTNodeNonTerm) {
        c.reloadElement()
      }
      c.attachToEle(this.ele)
    }
  }

  len(): number {
    return this.node.len()
  }

  clear() {
    for (const c of this.children) {
      c.clear()
    }

    this.children.length = 0
    this.node.free();
    this.ele.innerHTML = "";
  }

  insertText(offset: number, text: string, graph: EditGraph, sel: Selection): { nodes: JSCSTNode[], reparsed: boolean } | null {

    let adjusted_offset = offset;

    for (let i = 0; i < this.children.length; i++) {
      let child = this.children[i];
      let c_len = child.len();

      if (adjusted_offset <= c_len) {
        if (child instanceof CSTNodeNonTerm) {
          let result = child.insertText(adjusted_offset, text, graph, sel);
          if (result) {
            let par_node = graph.remove_child(this.node, i);
            this.node.free();

            if (!result.reparsed) {
              this.node = graph.add_child(par_node, result.nodes[0], i);
            } else {
              child.clear();
              for (const c of result.nodes.reverse()) {
                par_node = graph.add_child(par_node, c, i);
              }
              this.node = par_node;
              this.children.splice(i, 1, ...setupChildren(this, result.nodes));
              this.reloadElement();
              this.setOffset(graph, sel, offset + text.length);
            }

            return ({
              reparsed: false,
              nodes: [this.node]
            })
          } else {
            break;
          }
        } else if (child instanceof CSTNodeTerminal) {
          // Could potentially attempt a parse of a terminal 
          // and  attempt to return an updated terminal node. 
          // ATM this is not implemented, so we'll just modify 
          // the parent node.
          break;
        }
      }

      adjusted_offset -= c_len;
    }

    let result = graph.patch_insert(this.node, offset, text);

    if (result) {
      let r = <JSPatchResult><any>result;
      return { nodes: getIndex(result.num_of_nodes()).map(i => <JSCSTNode>r.node_at(i)), reparsed: true };
    } else {
      return null;
    }
  }

  deleteText(offset: number, len: number, graph: EditGraph, sel: Selection): { nodes: JSCSTNode[], reparsed: boolean } | null {

    let adjusted_offset = offset;

    for (let i = 0; i < this.children.length; i++) {
      let child = this.children[i];
      let c_len = child.len();

      if (adjusted_offset <= c_len && adjusted_offset + len <= c_len) {
        if (child instanceof CSTNodeNonTerm) {
          let result = child.deleteText(adjusted_offset, len, graph, sel);
          if (result) {
            let par_node = graph.remove_child(this.node, i);
            this.node.free();

            if (!result.reparsed) {
              this.node = graph.add_child(par_node, result.nodes[0], i);
            } else {
              child.clear();
              for (const c of result.nodes.reverse()) {
                par_node = graph.add_child(par_node, c, i);
              }
              this.node = par_node;
              this.children.splice(i, 1, ...setupChildren(this, result.nodes));
              this.reloadElement();
              this.setOffset(graph, sel, offset);
            }

            return ({
              reparsed: false,
              nodes: [this.node]
            })
          } else {
            break;
          }
        } else if (child instanceof CSTNodeTerminal) {
          // Could potentially attempt a parse of a terminal 
          // and  attempt to return an updated terminal node. 
          // ATM this is not implemented, so we'll just modify 
          // the parent node.
          break;
        }
      }

      adjusted_offset -= c_len;
    }

    let result = graph.patch_remove(this.node, offset, len);

    if (result) {
      let r = <JSPatchResult><any>result;
      return { nodes: getIndex(result.num_of_nodes()).map(i => <JSCSTNode>r.node_at(i)), reparsed: true };
    } else {
      return null;
    }
  }

  setOffset(graph: EditGraph, sel: Selection, offset: number) {
    let data = graph.get_offset(this.node, offset);

    if (data) {

      let inner_offset = data[data.length - 1];

      let target: CSTNode = this;

      for (const index of data.slice(1, -1)) {
        if (target instanceof CSTNodeNonTerm) {
          target = target.children[index];
        }
      }

      if (target instanceof CSTNodeTerminal) {
        sel.removeAllRanges();
        let range = document.createRange();
        range.setStart(<any>target.ele.firstChild, inner_offset);
        sel.addRange(range);
      } else {
        throw "Incorrect Selection Node"
      }
    }
  }
}

export class CSTNodeRoot extends CSTNodeNonTerm {

  graph: null | EditGraph;

  db: JSBytecodeParserDB;

  node_lu: Map<Node, CSTNode>;

  constructor(db: JSBytecodeParserDB) {
    super(<any>null, <any>null)
    this.db = db;
    this.ele.classList.add("cst-root");
    this.graph = null;
    this.ele.contentEditable = "true";
    this.ele.addEventListener("beforeinput", <any>this.edited.bind(this))
    this.node_lu = new Map;
  }

  getGraph(): EditGraph | null {
    return this.graph
  }

  /// Attaches node to the given element
  attach(ele: HTMLElement) {
    ele.appendChild(this.ele);
  }

  delete(len: number) {
    if (this.graph) {
      const sel = window.getSelection();
      let node = sel?.focusNode?.parentElement;

      if (!sel)
        throw "Could not resolve selection"

      if (!node)
        throw "Node not in graph!";

      let cst = this.node_lu.get(node);

      if (!cst)
        throw "CST not in graph!";

      let offset = sel?.focusOffset ?? 0;

      if (cst instanceof CSTNodeTerminal) {
        if (cst.node.get_type() == "Missing") {
          offset = 0;
        }
      }

      let cst_offset = cst.offset();
      offset += cst_offset;

      let result = this.deleteText(offset - len, len, this.graph, sel);

      if (result) {
        this.node.free;
        this.node = result.nodes[0];
        if (result.reparsed) {
          this.children = setupChildren(this, getIndex(this.node.num_of_children()).map(i => this.node.child_at(i)));
          this.reloadElement();
          this.setOffset(this.graph, sel, offset - len);
        }
      }
    }
  }

  insert(text: string) {
    if (!this.graph) {
      this.graph = new EditGraph(text, this.db);

      let node = this.graph.get_root();

      if (node) {
        this.node = node;
        this.children = setupChildren(this, getIndex(node.num_of_children()).map(i => node.child_at(i)));
        this.reloadElement();
        const sel = window.getSelection();
        if (sel) {
          this.setOffset(this.graph, sel, text.length);
        }
      }
    } else {
      const sel = window.getSelection();
      let node = sel?.focusNode?.parentElement;

      if (!sel)
        throw "Could not resolve selection"

      if (!node)
        throw "Node not in graph!";

      let cst = this.node_lu.get(node);

      if (!cst)
        throw "CST not in graph!";

      let offset = sel?.focusOffset ?? 0;

      if (cst instanceof CSTNodeTerminal) {
        if (cst.node.get_type() == "Missing") {
          offset = 0;
        }
      }

      let cst_offset = cst.offset();
      offset += cst_offset;

      let result = this.insertText(offset, text, this.graph, sel);

      if (result) {
        this.node.free;
        this.node = result.nodes[0];
        if (result.reparsed) {
          this.children = setupChildren(this, getIndex(this.node.num_of_children()).map(i => this.node.child_at(i)));
          this.reloadElement();
          this.setOffset(this.graph, sel, offset + text.length);
        }
      }
    }
  }

  addToMap(node: CSTNode): void {
    this.node_lu.set(node.ele, node);
  }

  removeFromMap(node: CSTNode): void {
    this.node_lu.delete(node.ele)
  }

  edited(e: InputEvent) {
    switch (e.inputType) {
      case "insertText": {
        let data = <string>e.data;
        this.insert(data)
      } break;
      case "deleteContentBackward":
        this.delete(1);
        break;
      case "insertFromPaste": {
        e.dataTransfer?.items[0].getAsString(data => {
          this.insert(data)
        });
      } break;
      case "insertLineBreak":
        break;
    }

    e.preventDefault();

    return true;
  }
}

