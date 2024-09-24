
import { StateEffect, StateField, Range } from "@codemirror/state";
import { Decoration, EditorView } from "@codemirror/view";
import { Diagnostic, setDiagnostics } from "@codemirror/lint";
import { basicSetup } from "codemirror";

export const highlight_effect = StateEffect.define<Range<Decoration>[]>();
export const filter_effects = StateEffect.define<((from: number, to: number, decoration: Decoration & { attrs: any }) => boolean)>();

export class InfoField<EventObj = null, event_names = ""> {
  ele: HTMLElement;
  header: HTMLElement;
  body: HTMLElement;
  label: HTMLElement;
  collapsed: boolean = false;
  listeners: Map<event_names, ((arg: EventObj) => void)[]> = new Map;

  constructor(name: string = "") {
    let ele = document.createElement("div");
    ele.append((<HTMLTemplateElement>document.querySelector("#panel")).content.cloneNode(true));
    this.ele = <any>ele.firstElementChild;

    let notebook = document.getElementById("notebook");

    if (notebook)
      notebook.append(this.ele);

    this.header = <any>this.ele.querySelector(".nb-header");
    this.label = <any>this.ele.querySelector(".nb-label");
    this.body = <any>this.ele.querySelector(".nb-body");

    this.label.innerHTML = name;

    this.header.addEventListener("click", () =>
      this.setExpanded(this.ele.classList.contains("collapsed"))
    );
  }

  setContentVisible(is_content_visible: boolean) {
    if (is_content_visible) {
      this.ele.classList.add("content-visible");
    } else {
      this.ele.classList.remove("content-visible");
    }
  }

  setLoading(is_loading: boolean) {
    if (is_loading) {
      this.ele.classList.add("loading");
    } else {
      this.ele.classList.remove("loading");
    }
  }

  setExpanded(is_expanded: boolean) {
    if (!is_expanded) {
      this.ele.classList.add("collapsed");
    } else {
      this.ele.classList.remove("collapsed");
    }
  }

  protected emit(event: event_names) {
    for (const listener of this.listeners.get(event) ?? []) {
      listener(<EventObj><any>this);
    }
  }

  addListener(event: event_names, listener: (arg: EventObj) => void) {
    if (!this.listeners.get(event)) {
      this.listeners.set(event, [listener]);
    } else {
      this.listeners.get(event)?.push(listener);
    }
  }
}

export class EditorField extends InfoField<EditorField, "text_changed"> {

  cm: EditorView;
  diagnostics: Diagnostic[] = [];

  constructor(name: string) {
    super(name);
    this.cm = new EditorView({
      doc: "",
      extensions: [basicSetup, EditorView.domEventHandlers({
        input: () => {
          this.emit("text_changed");
        },
        scroll: () => { },
        blur: () => {
        },
        paste: event => { },
      }), StateField.define({
        create() { return Decoration.none; },
        update(value, tr) {
          value = value.map(tr.changes);

          for (let effect of tr.effects) {
            if (effect.is(highlight_effect)) value = value.update({ add: effect.value, sort: true });
            else if (effect.is(filter_effects)) value = value.update({ filter: effect.value });
          }

          return value;
        },
        provide(f) { return EditorView.decorations.from(f); }
      })],
      parent: this.body
    });
  }


  setText(text: string) {
    this.cm.dispatch(
      {
        changes: { from: 0, to: this.cm.state.doc.length, insert: text }
      }
    );
  }

  getText(): string {
    return this.cm.state.doc.toString();
  }

  addHighlight(start_char: number, end_char: number, color: string) {
    this.cm.dispatch({
      effects: [highlight_effect.of([
        Decoration.mark({ attributes: { style: `color: ${color}`, } }).range(start_char, end_char)
      ])]
    });
  }

  removeHighlight() {
    this.cm.dispatch({
      effects: [filter_effects.of((from, to, decoration) => {
        return true;
      })]
    });
  }

  addMsg(start_char: number, len: number, msg: string) {

    this.diagnostics.push({
      from: start_char,
      to: start_char + len,
      message: msg,
      severity: "warning",
    });

    this.cm.dispatch(setDiagnostics(this.cm.state, this.diagnostics));


  }

  removeMsgs() {
    this.cm.dispatch(setDiagnostics(this.cm.state, this.diagnostics));
  };
}

