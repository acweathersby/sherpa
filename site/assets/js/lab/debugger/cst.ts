import { CMThing } from "js/common/layout";

export class CSTNode {
  public children: CSTNode[]
  public name: string
  public expression: string
  public terminal: boolean;
  input: CMThing;
  grammar: CMThing;
  offset: number
  length: number

  constructor(
    name: string, expression: string, terminal: boolean, offset: number, length: number, input: CMThing, grammar: CMThing, g_offset: number, g_length: number) {
    this.children = [];
    this.name = name;
    this.terminal = terminal;
    this.expression = expression;
    this.input = input;
    this.offset = offset;
    this.length = length
    this.grammar = grammar;

    this.g_offset = g_offset;
    this.g_length = g_length;
  }


  toDOM(): HTMLElement {
    const ele = document.createElement("div");
    ele.classList.add("cst-node");
    ele.classList.add("close")

    const name_ele = document.createElement("div");
    name_ele.classList.add("cst-name");
    name_ele.innerText = this.name;
    ele.appendChild(name_ele);

    name_ele.addEventListener("mouseenter", () => {
      this.input.clearHighlightClass("term-test", "nterm-test");
      this.input.addHighlight(this.offset, this.length, this.terminal ? "term-test" : "nterm-test");

      if (!this.terminal) {
        this.grammar.clearHighlightClass("term-test", "nterm-test");
        this.grammar.addHighlight(this.g_offset, this.g_length, this.terminal ? "term-test" : "nterm-test")
      }
    })

    name_ele.addEventListener("mouseleave", () => {
      this.input.clearHighlightClass("term-test", "nterm-test");
      if (this.grammar) {

        this.grammar.clearHighlightClass("term-test", "nterm-test");
      }
    })


    name_ele.addEventListener("click", e => {
      if (ele.classList.contains("open")) {
        ele.classList.add("close")
        ele.classList.remove("open")
      } else {
        ele.classList.add("open")
        ele.classList.remove("close")
      }
      e.stopImmediatePropagation();
      e.stopPropagation();
      e.preventDefault();
      return false;
    })

    if (this.terminal) {
      ele.classList.add("terminal")
    } else {
      ele.classList.add("nonterminal")
      if (this.expression) {
        let e_ele = document.createElement("span")
        e_ele.innerText = this.expression
        e_ele.classList.add("cst-rule-item-expression")
        name_ele.appendChild(e_ele);
      }
    }

    if (this.children.length > 0) {
      let children = document.createElement("div");
      children.classList.add("cst-children")
      for (const child of this.children) {
        children.appendChild(child.toDOM())
      }
      ele.appendChild(children);
    }

    return ele;
  }
}
