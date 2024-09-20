import { EditorView, basicSetup } from "codemirror";
import { ScrollHandler } from "js/lab";
import { DebuggerButton, DebuggerCheckbox, DebuggerField } from "js/lab/debugger/debugger_io";

/**
 * An box that holds Things and other Containers.
 */
export class Container {

  ele: HTMLDivElement
  children: (Container | Thing<any>)[];
  id: string;
  dir: "vertical" | "horizontal";
  parent: Container | null
  fixed_size: boolean = false

  constructor(id: string,) {
    if (id == "") {
      throw new Error("Container ids must not be empty")
    }
    this.ele = document.createElement("div");
    this.ele.classList.add("lab-container");
    this.children = [];
    this.id = "lab-" + id;
    this.dir = "horizontal";
    this.direction = "vertical";
    this.parent = null
  }

  set direction(direction: "vertical" | "horizontal") {
    if (direction != this.dir) {
      switch (direction) {
        case "horizontal":
          this.ele.classList.add("hori");
          this.ele.classList.remove("vert");
          this.dir = direction;
          break;
        case "vertical":
          this.ele.classList.add("vert");
          this.ele.classList.remove("hori");
          this.dir = direction;
          break
      }
      this.calculateSizes();
    }
  }

  getMajorSize(ele: HTMLElement): number {
    switch (this.dir) {
      case "horizontal":
        return ele.clientWidth;
      case "vertical":
        return ele.clientHeight;
    }
  }

  calculateSizes() {
    let total_size = this.getMajorSize(this.ele);
    let used_sizes = 0;
    let number_of_containers = 0;

    for (const node of this.children) {

      if (node.fixed_size) {
        used_sizes += this.getMajorSize(node.ele);
      } else {
        number_of_containers++;
      }
    }

    let diff = total_size - used_sizes;
    let container_allotment = (diff / number_of_containers) / total_size;


    if (container_allotment > 0 && container_allotment < Infinity) {


      for (const node of this.children) {
        if (node.fixed_size) {
          continue
        }
        if (true /* node instanceof Container */) {
          switch (this.dir) {
            case "horizontal":
              node.ele.style.width = 100 * container_allotment + "%";
              break;
            case "vertical":
              node.ele.style.height = 100 * container_allotment + "%";
              break;
          }

          if (node instanceof Container)
            node.calculateSizes();
        }
      }
    }
  }

  addChild(child: Container | Thing<any>) {
    if (child.parent) {
      child.parent.removeChild(child);
    }

    child.parent = this;
    this.children.push(child);
    this.ele.appendChild(child.ele);
    this.calculateSizes();
  }

  private removeChild(child: Container | Thing<any>) {
    if (child.parent == this) {
      this.ele.removeChild(child.ele);
      child.parent = null;
      let index = this.children.indexOf(child);
      if (index >= 0) {
        this.children.splice(index, 1);
      }
    }
  }

  attach(parent: HTMLElement | Container) {
    if (parent instanceof Container) {
      parent.addChild(this)
    } else {
      parent.appendChild(this.ele)
    }
  }
}

/**
 * A thing that displays some information to the user. May optionally have 
 * the ability to receive input. 
 */
export class Thing<K extends keyof HTMLElementTagNameMap> {
  protected label: HTMLSpanElement;
  protected show_label: boolean;
  protected id: string
  protected type: string;
  readonly io: HTMLElementTagNameMap[K];
  ele: HTMLDivElement;
  content: HTMLDivElement;
  parent: Container | null
  fixed_size: boolean = false

  constructor(io_type: K, id: string, label_text: string, readonly: boolean = false) {
    if (id == "") {
      throw new Error("Thing ids must not be empty")
    }

    this.label = document.createElement("div");
    this.label.classList.add("lab-thing-title");

    this.show_label = true;
    this.label_text = label_text;

    this.io = <any>document.createElement(io_type);
    this.io.classList.add("lab-io-ele");

    this.readonly = readonly;

    this.ele = document.createElement("div");
    this.ele.classList.add("lab-thing")

    this.content = document.createElement("div");
    this.content.classList.add("lab-thing-content");
    this.ele.appendChild(this.content);
    this.ele.appendChild(this.label);

    for (let i = 0; i < 2; i++) {
      for (let j = 0; j < 2; j++) {
        for (let k = 0; k < 2; k++) {
          let ele = document.createElement("div");
          ele.style.position = "absolute";

          if (i == 0) {
            ele.style.top = "0";
          } else {
            ele.style.bottom = "0";
          }

          if (j == 0) {
            ele.style.left = "0";
          } else {
            ele.style.right = "0";
          }

          if (k == 0) {
            ele.style.width = "2px";
            ele.style.height = "8px";
          } else {
            ele.style.width = "8px";
            ele.style.height = "2px";
          }

          ele.classList.add("thing-decoration");

          this.ele.appendChild(ele);
        }
      }
    }

    if (label_text) {
      //  this.ele.appendChild(this.label);
    }

    this.id = id;

    this.ele.id = id;

    this.parent = null;

    this.type = io_type;
  }

  vertical_scroll() {
    let sh = new ScrollHandler(this.content, this.ele);
  }

  set readonly(readonly: boolean) {
    if (readonly) {
      this.io.setAttribute("readonly", "")
      this.io.classList.add("readonly");
    } else {
      this.io.removeAttribute("readonly")
      this.io.classList.remove("readonly");
    }
  }

  get readonly(): boolean {
    return this.io.classList.contains("readonly");
  }

  set label_text(label_text: string) {
    this.label.innerText = label_text;
  }

  get label_text(): string {
    return this.label.innerText;
  }

  ele_as<K extends keyof HTMLElementTagNameMap>(type: K): HTMLElementTagNameMap[K] | null {
    if (this.type != type) {
      return null;
    } else {
      return <any>this.ele;
    }
  }

  attach(parent: Container) {
    parent.addChild(this);
  }
}

import { StateEffect, Range, StateField } from "@codemirror/state";
import { Decoration } from "@codemirror/view";

const highlight_effect = StateEffect.define<Range<Decoration>[]>();
const filter_effects = StateEffect.define<((from: number, to: number, decoration: Decoration & { attrs: any }) => boolean)>();

/**
 * A thing that displays a codemirror box
 */
export class CMThing extends Thing<"div"> {

  editor: EditorView;

  update_fn: any;

  constructor(id: string, label_text: string, doc_text: string = "", extensions: any[] = []) {
    super("div", id, label_text, false);
    this.editor = new EditorView({
      doc: doc_text,
      extensions: [basicSetup, EditorView.domEventHandlers({
        input: () => {
          this.update_fn(this.editor.state.doc.toString());
        },
        scroll: () => { },
        blur: () => {
        },
        paste: event => { },
      }), StateField.define({
        create() { return Decoration.none },
        update(value, tr) {
          value = value.map(tr.changes)

          for (let effect of tr.effects) {
            if (effect.is(highlight_effect)) value = value.update({ add: effect.value, sort: true });
            else if (effect.is(filter_effects)) value = value.update({ filter: effect.value });
          }

          return value
        },
        provide(f) { return EditorView.decorations.from(f) }
      }), ...extensions],
      parent: this.content
    });

    this.update_fn = () => { };

  }

  setText(text: string) {
    this.editor.dispatch({
      changes: { from: 0, to: this.editor.state.doc.length, insert: text }
    });
  }


  clearHighlights() {
    this.editor.dispatch({ effects: [filter_effects.of((from, to) => false)] });
  }

  clearHighlightClass(...highlight_class: string[]) {
    if (highlight_class.length > 0)
      this.editor.dispatch({
        effects: [filter_effects.of((from, to, decoration) => {
          return !highlight_class.includes(decoration.attrs.class)
        })]
      });
  }

  addHighlight(offset: number, length: number, highlight_class: string = "test", attributes = {}) {
    let effect = highlight_effect.of([
      Decoration.mark({ attributes: { class: highlight_class, ...attributes } }).range(offset, offset + length)
    ]);

    this.editor.dispatch({ effects: [effect] });
  }

  onUpdate(fn: any) {
    this.update_fn = fn;
  }
}