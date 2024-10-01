

import { MoveFieldDragOperation, ResizeFieldOperation } from "./dragndrop_operations";


const TRANSITION_DURATION_MS = 100;
export const MIN_EXPANDED_FIELD_HEIGHT = 160;
const COLLAPSED_FIELD_HEIGHT = 40;

export class NB {
  ele: HTMLElement
  columns: NBColumn[];
  max_columns: number = 3;

  constructor(num_of_columns: number) {

    let ele = <HTMLElement | null>document.querySelector("#notebook")

    if (ele) {
      this.ele = ele;

      num_of_columns = Math.min(Math.max(1, num_of_columns), 3);

      this.columns = new Array(num_of_columns).fill(0).map((_, i) => new NBColumn(this, i, false))

      for (const row of this.columns) {
        this.ele.append(row.ele);
      }
    } else {
      throw "Could not locate #notebook element"
    }
  }

  addField<T extends NBField>(field: T, col: number = 0, row: number = Infinity): T {
    col = Math.min(Math.max(0, col), this.columns.length - 1);

    field.nb_host = this;

    // Remove from any existing rows

    for (const row of this.columns) {
      row.remove(field);
    }

    this.columns[col].add(field, row);

    return field
  }

  removeField<T extends NBField>(field: T): boolean {
    let { col, v_row: row, nb_host } = field;

    if (col < 0 || nb_host != this) return false;

    this.columns[col].remove(field);

    field.col = -1;
    field.v_row = -1;
    field.r_row = -1;
    field.nb_host = null;

    return true
  }

  removeCol(col_index: number) {
    if (col_index >= 0 && col_index < this.columns.length) {
      let col = this.columns[col_index];

      if (col.cell_count > 0) {
        throw "Attempted to delete a non-empty column"
      }

      this.columns.splice(col_index, 1);
      this.columns.forEach((col, i) => col.setIndex(i));
      col.delete();
    }
  }


  insertCol(col_index: number) {
    let new_col = new NBColumn(this, col_index, false);
    this.columns.splice(col_index, 0, new_col);

    if (new_col != this.columns[col_index]) {
      throw "wt"
    }

    this.columns.forEach((col, i) => col.setIndex(i));

    if (col_index == 0) {
      this.ele.prepend(new_col.ele)
    } else {
      this.ele.insertBefore(new_col.ele, <any>this.columns[col_index - 1].ele.nextElementSibling);
    }

  }


  calculateHeights() {
    for (const col of this.columns) {
      col.latchHeights();
      col.distributeHeight();
    }
  }
}

export class NBColumn {
  ele: HTMLElement;
  cells: NBField[]
  nb_host: NB
  index: number

  constructor(nb_host: NB, index: number, animate_in: boolean) {
    this.ele = document.createElement("div");
    this.ele.classList.add("nb-column");
    this.cells = [];
    this.nb_host = nb_host;
    this.index = index;

    if (animate_in) {
      this.ele.style.width = "0"
      setTimeout(() => {
        this.ele.style.width = ""
      }, 10)
    }
  }


  swap(other_field: NBField, own_index: number) {
    let own_field = this.cells[own_index];

    if (own_field && own_field != other_field && other_field.nb_host == this.nb_host) {


      let other_col = this.nb_host.columns[other_field.col]

      if (!other_col) return;

      let own_height = own_field.latched_height;
      let other_height = other_field.latched_height;

      own_field.latched_height = other_height;
      other_field.latched_height = own_height;

      let other_index = other_field.r_row;

      let temp_ele = document.createElement("span");
      let own_ele = own_field.ele;
      let other_ele = other_field.ele;

      this.cells[own_index] = other_field;
      other_col.cells[other_index] = own_field;

      this.setIndex();
      other_col.setIndex();

      this.distributeHeight();
      other_col.distributeHeight();

      this.ele.replaceChild(temp_ele, own_ele);
      other_col.ele.replaceChild(own_ele, other_ele);
      this.ele.replaceChild(other_ele, temp_ele);
    }
  }


  max_free(): number {
    return this.ele.getBoundingClientRect().height - this.cell_count * MIN_EXPANDED_FIELD_HEIGHT;
  }

  delete() {
    this.ele.parentElement?.removeChild(this.ele);
  }

  remove(field: NBField) {
    let index = this.find(field);

    if (index >= 0) {
      this.cells.splice(index, 1);
      this.ele.removeChild(field.ele);
      field.nb_host = null;
      field.col = -1;
      field.v_row = -1;

      this.setIndex()
    }
  }

  add(field: NBField, row: number = Infinity, using_real_index: boolean = false) {
    row = using_real_index ? row : this.findRealIndex(row);

    if (row < this.cells.length) {
      this.cells.splice(row, 0, field);
      this.ele.insertBefore(field.ele, this.cells[row + 1].ele);
      field.col = this.index;
    } else {
      this.cells.push(field);
      this.ele.appendChild(field.ele);
    }

    field.nb_host = this.nb_host;
    field.v_row = -1;

    this.setIndex()
  }

  setIndex(col_index: number = this.index) {
    this.index = col_index;
    this.cells.forEach((i, index) => { i.r_row = index; i.col = col_index });
    this.cells.filter(n => !(n instanceof NBBlankField)).forEach((i, index) => i.v_row = index);
  }

  find(field: NBField): number {
    return this.cells.findIndex(f => f === field);
  }

  findRealIndex(virtual_index: number): number {
    let index = -1;
    for (const cell of this.cells) {
      index++;
      if (cell.v_row >= 0 && cell.v_row >= virtual_index) {
        return index;
      }
    }

    return this.cells.length
  }

  pointInside(x: number, y: number, edge_size: number = 0): { insert_row: number, alignment: number } | null {

    const { x: col_x, y: col_y, width, height } = this.ele.getBoundingClientRect();
    const col_max_x = col_x + width;
    const col_max_y = col_y + height;
    const inside_x = x > col_x && x < col_max_x;
    const inside_y = y > col_y && y < col_max_y;

    if (inside_x && inside_y) {
      let alignment = 0;

      if (x - col_x <= edge_size) {
        alignment = 1
      } else if (col_max_x - x <= edge_size) {
        alignment = 2
      }

      let target_offset = y - col_y;
      const real_cells = this.cells.filter(f => !(f instanceof NBBlankField));
      for (const cell of real_cells) {
        const height = cell.latched_height;
        if (target_offset <= height) {
          if (target_offset < (height / 2)) {
            // Insert before field
            return { insert_row: cell.v_row, alignment }
          } else {
            // Insert after field
            return { insert_row: cell.v_row + 1, alignment }
          }

        } else {
          target_offset -= height;
        }
      }

      return { insert_row: real_cells.length, alignment }
    }

    return null;
  }

  latchHeights() {
    this.cells.forEach(c => c.latchHeight())
  }

  get cell_count(): number {
    return this.cells.filter(f => !(f instanceof NBBlankField)).length
  }


  distributeHeight(fixed_heights_settings: { index: number, height: number }[] = []) {

    let real_height = this.ele.getBoundingClientRect().height;

    let cell_heights = this.cells.map((c, i) => {

      if (c instanceof NBBlankField && c.deleting) {
        return { f: false, h: 0 }
      }

      let v = fixed_heights_settings.find(f => f.index == i);
      if (v) {
        return { f: true, h: v.height }
      } else if (c.collapsed) {
        return { f: true, h: COLLAPSED_FIELD_HEIGHT }
      } else if (c.latched_height <= MIN_EXPANDED_FIELD_HEIGHT) {
        return { f: true, h: MIN_EXPANDED_FIELD_HEIGHT }
      } else {
        return { f: false, h: c.latched_height }
      }
    });

    let cell_height_sum = cell_heights.reduce((r, l) => {
      if (l.f) { r.f += l.h } else { r.l += l.h }
      return r
    }, { f: 0, l: 0 });

    let remainder_percentage = 1 / (cell_height_sum.l / (real_height - cell_height_sum.f));
    let inv_real_height = 1 / real_height;


    let normalized_heights = [];
    for (let i = 0; i < this.cells.length; i++) {
      let height = cell_heights[i];
      if (height.f) {
        normalized_heights.push(height.h * inv_real_height);
      } else {
        normalized_heights.push(height.h * remainder_percentage * inv_real_height);
      }
    }
    let normalized_value = 1 / normalized_heights.reduce((v, a) => v + a, 0);
    for (let i = 0; i < this.cells.length; i++) {
      this.cells[i].setRelativeHeight(normalized_heights[i] * normalized_value);
    }
  }
}


export class NBField {
  ele: HTMLElement;
  nb_host: null | NB = null
  // The index of the row excluding any blank cells
  v_row: number = 0;
  // The index of the row including blank cells
  r_row: number = 0;
  col: number = 0;
  latched_height: number = 0
  collapsed: boolean = false;

  constructor(ele = document.createElement("div")) {
    this.ele = ele;
    this.ele.classList.add("nb-field")


  }

  latchHeight() {
    if (this.collapsed) return;

    const { height } = this.ele.getBoundingClientRect();
    this.latched_height = height;
  }

  /**
   * @param height - a ratio of the parent containers height
   */
  setRelativeHeight(height: number) {
    this.ele.style.height = `${height * 100}%`
  }

  unsetRelativeHeight() {
    this.ele.style.height = ""
  }
}

export class NBBlankField extends NBField {
  deleting: boolean = false

  constructor(width: number, height: number, force_height: boolean = false) {
    super()
    this.ele.classList.add("nb-blank-field");
    this.latched_height = height;
    this.ele.appendChild(document.createElement("div"))

    if (!force_height) {
      setTimeout(() => {
        this.ele.style.opacity = "1"
        this.ele.style.height = `${height}px`
      }, 10)
    } else {
      this.ele.style.opacity = "1"
      this.ele.style.height = `${height}px`
    }
  }

  delete() {

    this.deleting = true;
    this.ele.style.opacity = `${0}px`

    if (this.nb_host) {
      let nb_host = this.nb_host;
      setTimeout(() => { nb_host.removeField(this); }, TRANSITION_DURATION_MS)
    } else {
      if (this.ele.parentElement)
        this.ele.parentElement.removeChild(this.ele)
    }
  }
}

export class NBContentField<EventObj = null, event_names = ""> extends NBField {
  header: HTMLElement;
  body: HTMLElement;
  label: HTMLElement;
  resize_handle: HTMLElement;
  collapsed: boolean = false;
  pre_collapse_size: number = 0
  listeners: Map<event_names, ((arg: EventObj) => void)[]> = new Map;

  constructor(name: string = "") {
    let ele = document.createElement("div");
    ele.append((<HTMLTemplateElement>document.querySelector("#panel")).content.cloneNode(true));

    super(<any>ele.firstElementChild)

    this.header = <any>this.ele.querySelector(".nb-header");
    this.label = <any>this.ele.querySelector(".nb-label");
    this.body = <any>this.ele.querySelector(".nb-body");
    this.resize_handle = <any>this.ele.querySelector(".nb-resize-handle");

    this.label.innerHTML = name;

    this.resize_handle.addEventListener("pointerdown", e => {
      new ResizeFieldOperation(e, this);
    }, { capture: true, passive: false })

    this.header.addEventListener("pointerdown", async e => {
      if (this.nb_host) {
        let drag_op = new MoveFieldDragOperation(e, this);
        if (!await drag_op.willDrag()) {
          this.setExpanded(this.ele.classList.contains("collapsed"))
        }
      }
    }, { capture: true, passive: false });
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

  setExpanded(is_collapsed: boolean) {
    if (!this.nb_host) return;

    if (!is_collapsed) {
      this.nb_host.calculateHeights();
      this.pre_collapse_size = this.latched_height
      this.ele.classList.add("collapsed");
      this.collapsed = true
      this.nb_host.calculateHeights();
    } else {
      this.ele.classList.remove("collapsed");
      this.collapsed = false
      this.latched_height = this.pre_collapse_size
      this.nb_host.columns[this.col].distributeHeight()
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

import * as language from "@codemirror/language";
import * as state from "@codemirror/state";
import * as view from "@codemirror/view";
import * as commands from "@codemirror/commands";
import * as autocomplete from "@codemirror/autocomplete";
import * as search from "@codemirror/search";
import * as lint from "@codemirror/lint";

const basicSetup = (() => [
  view.EditorView.theme({ "*": { "color": "unset !important", "background-color": "unset !important" } }),
  autocomplete.autocompletion(),
  autocomplete.closeBrackets(),
  commands.history(),
  language.bracketMatching(),
  language.foldGutter(),
  language.indentOnInput(),
  language.syntaxHighlighting(language.defaultHighlightStyle, { fallback: true }),
  search.highlightSelectionMatches(),
  state.EditorState.allowMultipleSelections.of(true),
  view.crosshairCursor(),
  view.drawSelection(),
  view.dropCursor(),
  view.highlightActiveLine(),
  view.highlightActiveLineGutter(),
  view.highlightSpecialChars(),
  view.lineNumbers(),
  view.rectangularSelection(),
  view.keymap.of([
    ...autocomplete.closeBracketsKeymap,
    ...commands.defaultKeymap,
    ...search.searchKeymap,
    ...commands.historyKeymap,
    ...language.foldKeymap,
    ...autocomplete.completionKeymap,
    ...lint.lintKeymap
  ])
])();

export const highlight_effect = state.StateEffect.define<state.Range<view.Decoration>[]>();
export const filter_effects = state.StateEffect.define<((from: number, to: number, decoration: view.Decoration & { attrs: any }) => boolean)>();


export class NBEditorField extends NBContentField<NBEditorField, "text_changed"> {

  cm: view.EditorView;
  diagnostics: lint.Diagnostic[] = [];

  constructor(name: string) {
    super(name);

    let change_interval_id = -1;

    this.cm = new view.EditorView({
      doc: "",
      extensions: [basicSetup, state.StateField.define({
        create() {
          return state.RangeSet.of([]);

        },
        update(value, tr) {

          value = value.map(tr.changes)

          for (let effect of tr.effects) {
            if (effect.is(highlight_effect)) {
              value = value.update({ add: effect.value, filterTo: 20, sort: true })
            }
            else if (effect.is(filter_effects)) value = value.update({ filter: effect.value });
          }

          return value.map(tr.changes)
        },
        provide: f => view.EditorView.decorations.from(<any>f)
      }), view.EditorView.domEventHandlers({
        input: () => {
          if (change_interval_id >= 0) {
            // Disable current interval and create a new one
            clearTimeout(change_interval_id);
          }

          change_interval_id = setTimeout(() => {
            change_interval_id = -1;
            this.emit("text_changed")
          }, 100);
          ;
        },
        scroll: () => { },
        blur: () => {
        },
        paste: event => { },
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
    end_char = Math.min(this.cm.state.doc.length, end_char);
    if (start_char == end_char) return;
    this.cm.dispatch({
      effects: [highlight_effect.of([
        view.Decoration.mark({ attributes: { style: `color: ${color} !important`, } }).range(start_char, end_char)
      ])]
    });
  }

  removeHighlight() {
    this.cm.dispatch({
      effects: [filter_effects.of((from, to, decoration) => {
        return false;
      })]
    });
  }

  addMsg(start_char: number, end_char: number, msg: string) {

    end_char = Math.min(this.cm.state.doc.length, end_char);
    if (start_char == end_char || start_char > end_char) return;

    console.log({ start_char, end_char })
    this.diagnostics.push({
      from: start_char,
      to: end_char,
      message: msg,
      severity: "warning",
    });

    this.cm.dispatch(lint.setDiagnostics(this.cm.state, this.diagnostics));


  }

  removeMsgs() {
    this.diagnostics.length = 0;
    this.cm.dispatch(lint.setDiagnostics(this.cm.state, this.diagnostics));
  };
}

