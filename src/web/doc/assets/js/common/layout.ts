import { EditorView } from "codemirror";
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

  constructor(id: string,) {
    if (id == "") {
      throw new Error("Container ids must not be empty")
    }
    this.ele = document.createElement("div");
    this.ele.classList.add("lab-container");
    this.children = [];
    this.id = "lab-" + id;
    //this.ele.innerHTML = this.id;
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
      if (node instanceof Container) {
        number_of_containers++;
        //ele.calculateSizes();
      } else {
        used_sizes += this.getMajorSize(node.ele);
      }
    }


    let diff = total_size - used_sizes;
    let container_allotment = (diff / number_of_containers) / total_size;
    console.log({ used_sizes, total_size, diff, number_of_containers })

    if (container_allotment > 0 && container_allotment < Infinity) {
      console.log({ container_allotment })

      for (const node of this.children) {
        if (node instanceof Container) {
          switch (this.dir) {
            case "horizontal":
              node.ele.style.width = 100 * container_allotment + "%";
              break;
            case "vertical":
              node.ele.style.height = 100 * container_allotment + "%";
              break;
          }
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
  parent: Container | null

  constructor(io_type: K, id: string, label_text: string, readonly: boolean = false) {
    if (id == "") {
      throw new Error("Thing ids must not be empty")
    }

    this.show_label = true;

    this.label = document.createElement("label");
    this.label.classList.add("lab-label");

    this.label_text = label_text;

    this.io = <any>document.createElement(io_type);
    this.io.classList.add("lab-io-ele");

    this.readonly = readonly;

    this.ele = document.createElement("div");
    this.ele.classList.add("lab-thing")

    if (label_text) {
      this.ele.appendChild(this.label);
    }

    this.ele.appendChild(this.io);

    this.id = id;

    this.parent = null;

    this.type = io_type;
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

type HTMLInputTypeAttribute = "text" | "checkbox"

/**
 * A thing that displays a button with a label 
 */
export class ButtonThing extends Thing<"button"> {
  constructor(id: string, label_text: string, readonly: boolean = false) {
    super("button", id, label_text, readonly);
    this.io.id = this.id;
    this.id = this.id + "-button-container";
    this.ele.id = this.id;
    this.io.classList.add(DebuggerButton.className);
    DebuggerButton.gatherButtons(this.ele);

  }
}

/**
 * A thing that displays an input field
 */
export class InputThing extends Thing<"input"> {

  input_type: string;

  constructor(input_type: HTMLInputTypeAttribute, id: string, label_text: string, readonly: boolean = false) {
    super("input", id, label_text, readonly);

    this.input_type = input_type;

    switch (input_type) {
      case "checkbox": {
        this.io.type = "checkbox";
        this.io.id = this.id;
        this.io.classList.add(DebuggerCheckbox.className);
        DebuggerCheckbox.gatherCheckBoxes(this.ele);
      } break;

      case "text": {
        this.io.type = "text";
        this.io.id = this.id;
        this.io.classList.add(DebuggerField.className);
        DebuggerField.gatherFields(this.ele);
      } break;
    }

    this.id = this.id + "-input-container";
    this.ele.id = this.id;
  }
}

/**
 * A thing that displays a codemirror box
 */
export class CMThing extends Thing<"div"> {

  editor: EditorView;

  constructor(id: string, label_text: string, doc_text: string = "", extensions: any[] = []) {
    super("div", id, label_text, false);
    this.editor = new EditorView({
      doc: doc_text,
      extensions,
      parent: this.ele
    });

    this.ele.removeChild(this.io);
  }
}