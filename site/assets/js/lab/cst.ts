import * as pipeline from "./pipeline";
import * as radlr from "js/radlr/radlr_wasm.js";
import { NBContentField, NBEditorField } from "./notebook";

export class CSTView {
  field: NBContentField;
  cst_nodes: CSTNode[] = [];

  constructor(field: NBContentField) {
    if (field instanceof NBEditorField) {
      throw "CSTView cannot bind to a NBEditorField";
    }
    this.field = field;

    this.field.body.classList.add("debugger-cst-output");
  }

  reset() {
    let ele = this.field.body;
    if (ele) {
      ele.innerHTML = "";
    }

    this.cst_nodes.length = 0;
  }

  handle_reduce(reduce: pipeline.ReduceStruct) {
    let { non_terminal_id, rule_id, symbols, db } = reduce;
    var [_, ...expr] = radlr.get_rule_expression_string(rule_id, db).split(">");

    let o_expr = "> " + expr.join("");

    let [start, end] = radlr.get_rule_location(rule_id, db);


    let name = radlr.get_nonterminal_name_from_id(non_terminal_id, db);


    let children = this.cst_nodes.slice(-symbols);

    let off_start = children[0].offset;
    let length = children[children.length - 1].offset + children[children.length - 1].length - off_start;

    let node = new CSTNode(name, o_expr, false, off_start, length, start, end - start);

    node.children = this.cst_nodes.slice(-symbols);


    this.cst_nodes.length -= symbols;
    this.cst_nodes.push(node);

    this.render_cst();
  }


  handle_shift(shift: pipeline.ShiftStruct) {

    let { token, db, byte_offset, byte_len } = shift;

    this.cst_nodes.push(new CSTNode(token, "", true, byte_offset, byte_len));

    this.render_cst();
  }

  render_cst() {
    let ele = this.field.body;
    if (ele) {
      ele.innerHTML = "";
      for (const node of this.cst_nodes) {
        ele.appendChild(node.toDOM());
      }
    }
  }
}



export class CSTNode {
  public children: CSTNode[];
  public name: string;
  public expression: string;
  public terminal: boolean;
  offset: number;
  length: number;
  g_offset: number;
  g_length: number;

  constructor(name: string, expression: string, terminal: boolean, offset: number, length: number, g_offset: number = 0, g_length: number = 0) {
    this.children = [];
    this.name = name;
    this.terminal = terminal;
    this.expression = expression;
    this.offset = offset;
    this.length = length;

    this.g_offset = g_offset;
    this.g_length = g_length;
  }


  toDOM(): HTMLElement {
    const ele = document.createElement("div");
    ele.classList.add("cst-node");
    ele.classList.add("close");

    const name_ele = document.createElement("div");
    name_ele.classList.add("cst-name");
    name_ele.innerText = this.name;
    ele.appendChild(name_ele);

    name_ele.addEventListener("mouseenter", () => {
      /*      this.input.clearHighlightClass("term-test", "nterm-test");
           this.input.addHighlight(this.offset, this.length, this.terminal ? "term-test" : "nterm-test");
     
           if (!this.terminal) {
             this.grammar.clearHighlightClass("term-test", "nterm-test");
             this.grammar.addHighlight(this.g_offset, this.g_length, this.terminal ? "term-test" : "nterm-test")
           } */
    });

    name_ele.addEventListener("mouseleave", () => {
      /*       this.input.clearHighlightClass("term-test", "nterm-test");
            if (this.grammar) {
      
              this.grammar.clearHighlightClass("term-test", "nterm-test");
            } */
    });


    name_ele.addEventListener("click", e => {
      if (ele.classList.contains("open")) {
        ele.classList.add("close");
        ele.classList.remove("open");
      } else {
        ele.classList.add("open");
        ele.classList.remove("close");
      }
      e.stopImmediatePropagation();
      e.stopPropagation();
      e.preventDefault();
      return false;
    });

    if (this.terminal) {
      ele.classList.add("terminal");
    } else {
      ele.classList.add("nonterminal");
      if (this.expression) {
        let e_ele = document.createElement("span");
        e_ele.innerText = this.expression;
        e_ele.classList.add("cst-rule-item-expression");
        name_ele.appendChild(e_ele);
      }
    }

    if (this.children.length > 0) {
      let children = document.createElement("div");
      children.classList.add("cst-children");
      for (const child of this.children) {
        children.appendChild(child.toDOM());
      }
      ele.appendChild(children);
    }

    return ele;
  }
}
