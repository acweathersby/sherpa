
import { StateEffect, StateField, Range } from "@codemirror/state";
import { Decoration, EditorView } from "@codemirror/view";
import { Diagnostic, setDiagnostics } from "@codemirror/lint";
import { basicSetup } from "codemirror";
import { sleep } from "./pipeline";

export const highlight_effect = StateEffect.define<Range<Decoration>[]>();
export const filter_effects = StateEffect.define<((from: number, to: number, decoration: Decoration & { attrs: any }) => boolean)>();



export class NB {
  ele: HTMLElement
  columns: NBColumn[];

  constructor(num_of_columns: number) {

    let ele = <HTMLElement | null>document.querySelector("#notebook")

    if (ele) {
      this.ele = ele;

      num_of_columns = Math.min(Math.max(1, num_of_columns), 3);

      this.columns = new Array(num_of_columns).fill(0).map((_, i) => new NBColumn(this, i))

      for (const row of this.columns) {
        this.ele.append(row.ele);
      }
    } else {
      throw "Could not locate #notebook element"
    }

    document.addEventListener("pointermove", this.pointerMove.bind(this));
    document.addEventListener("pointerup", this.pointerUp.bind(this));
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
    field.nb_host = null;

    return true
  }

  private static drag_delay_ms: number = 200;

  active_drag = false;

  pointer_timeout = 0;
  drag_field: NBField | null = null;
  drag_start_col: number = -1;
  drag_start_row: number = -1;

  drag_target_col: number = -1;
  drag_target_row: number = -1;
  placeholder: NBBlankField | null = null;

  mouse_up_trigger_count = 0;

  drag_committed = false;
  move_threshold = 10;

  start_pos_x = 0;
  start_pos_y = 0;
  curr_pos_x = 0;
  curr_pos_y = 0;
  offset_x = 0;
  offset_y = 0;
  start_width = 0;
  start_height = 0;

  async queueFieldDrag(e: PointerEvent, field: NBField): Promise<boolean> {
    let col_index = -1;
    for (const col of this.columns) {
      col_index++;
      let row_index = col.find(field);
      if (row_index >= 0) {
        let trigger_start = this.mouse_up_trigger_count;

        await sleep(NB.drag_delay_ms);

        if (trigger_start != this.mouse_up_trigger_count) {
          return false;
        } else {
          this.startFieldDrag(e, field, col_index, row_index);
          return true;
        }
      }
    }

    return false;
  }

  private startFieldDrag(e: PointerEvent, field: NBField, col: number, row: number) {
    this.drag_field = field;
    this.drag_start_col = col;
    this.drag_start_row = row;

    this.drag_target_col = col;
    this.drag_target_row = row;

    let { x, y } = e;
    let { x: ele_x, y: ele_y, width, height } = field.ele.getBoundingClientRect();

    this.offset_x = ele_x;
    this.offset_y = ele_y;

    this.start_height = height;
    this.start_width = width;

    this.start_pos_x = x;
    this.start_pos_y = y;

    this.curr_pos_x = x;
    this.curr_pos_y = y;

    this.drag_committed = false;
    this.active_drag = true;
  }

  private updateDragPos(field: NBField) {
    let { x, y } = this.getDragPos();
    field.ele.style.top = `${y}px`;
    field.ele.style.left = `${x}px`;
  }

  private getDragPos() {
    let x = (this.curr_pos_x - this.start_pos_x) + this.offset_x;
    let y = (this.curr_pos_y - this.start_pos_y) + this.offset_y;
    return { x, y };
  }

  private commitFieldDrag() {
    if (this.drag_field) {
      for (const col of this.columns) {
        for (const cell of col.cells) {
          cell.latchHeight();
        }
      }

      this.drag_committed = true;
      this.drag_field.ele.classList.add("dragging");
      this.updateDragPos(this.drag_field)
      this.drag_field.ele.style.width = this.start_width + "px";
      this.drag_field.ele.style.height = this.start_height + "px";

      // Remove the field from the host row and insert a placeholder of the correct dimensions.
      // the active field should now be attached the document body. 
      const col = this.columns[this.drag_start_col];

      col.remove(this.drag_field);

      this.placeholder = new NBBlankField(this.start_width, this.start_height, true);
      col.add(this.placeholder, this.drag_start_row);

      document.body.appendChild(this.drag_field.ele);
    }
  }

  private updateFieldDrag() {
    if (this.active_drag && this.drag_committed) {
      if (this.drag_field) {
        this.updateDragPos(this.drag_field);

        for (const col of this.columns) {
          const insert_data = col.pointInside(this.curr_pos_x, this.curr_pos_y);

          if (insert_data && (this.drag_target_col != col.index || this.drag_target_row != insert_data.insert_row)) {
            this.drag_target_col = col.index;
            this.drag_target_row = insert_data.insert_row;

            if (this.placeholder) {
              this.placeholder.delete();
              this.placeholder = null;
            }

            this.placeholder = new NBBlankField(this.start_width, this.start_height);
            col.add(this.placeholder, this.drag_target_row);
          }
        }
      }
    }
  }

  private endFieldDrag() {
    if (this.active_drag && this.drag_committed) {

      if (this.drag_field) {
        this.drag_field.ele.classList.remove("dragging");
        this.drag_field.ele.style.top = ``;
        this.drag_field.ele.style.left = ``;
        this.drag_field.ele.style.width = ``;
        this.drag_field.ele.style.height = ``;

        this.addField(this.drag_field, this.drag_target_col, this.drag_target_row);

        if (this.placeholder) {
          this.removeField(this.placeholder);
          this.placeholder = null;
        }
      }
    }

    this.drag_committed = false;
    this.active_drag = false;
  }

  private pointerMove(e: PointerEvent) {
    if (this.active_drag) {

      let { x, y } = e;

      this.curr_pos_x = x;
      this.curr_pos_y = y;

      if (!this.drag_committed) {
        const abs_diff = Math.sqrt((x - this.start_pos_x) ** 2 + (y - this.start_pos_y) ** 2);
        if (abs_diff >= this.move_threshold) {
          this.commitFieldDrag();

        }
      } else {
        this.updateFieldDrag();
      }

      if (e.stopPropagation) e.stopPropagation();
      if (e.preventDefault) e.preventDefault();
      return false;
    }
  }

  private pointerUp(e: PointerEvent) {
    this.mouse_up_trigger_count++;
    this.endFieldDrag();
  }
}

export class NBColumn {
  ele: HTMLElement;
  cells: NBField[]
  nb_host: NB
  index: number

  constructor(nb_host: NB, index: number) {
    this.ele = document.createElement("div");
    this.ele.classList.add("nb-row");
    this.cells = [];
    this.nb_host = nb_host;
    this.index = index;
  }

  remove(field: NBField) {
    let index = this.find(field);

    if (index >= 0) {
      this.cells.splice(index, 1);
      this.ele.removeChild(field.ele);
      field.nb_host = null;
      field.col = -1;
      field.v_row = -1;

      this.cells.filter(n => !(n instanceof NBBlankField)).forEach((i, index) => i.v_row = index);
    }
  }

  add(field: NBField, row: number = Infinity) {
    row = this.findRealIndex(row);

    if (row < this.cells.length) {
      this.cells.splice(row, 0, field);
      this.ele.insertBefore(field.ele, this.cells[row + 1].ele);
      field.col = this.index;
    } else {
      this.cells.push(field);
      this.ele.appendChild(field.ele);
    }

    field.nb_host = this.nb_host;
    field.col = this.index;
    field.v_row = -1;

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

  pointInside(x: number, y: number): { insert_row: number } | null {

    const { x: col_x, y: col_y, width, height } = this.ele.getBoundingClientRect();
    const col_max_x = col_x + width;
    const col_max_y = col_y + height;
    const inside_x = x > col_x && x < col_max_x;
    const inside_y = y > col_y && y < col_max_y;

    if (inside_x && inside_y) {
      let target_offset = y - col_y;
      const real_cells = this.cells.filter(f => !(f instanceof NBBlankField));
      for (const cell of real_cells) {
        const height = cell.latched_height;
        if (target_offset <= height) {
          if (target_offset < (height / 2)) {
            // Insert before field
            return { insert_row: cell.v_row }
          } else {
            // Insert after field
            return { insert_row: cell.v_row + 1 }
          }

        } else {
          target_offset -= height;
        }
      }

      return { insert_row: real_cells.length }
    }

    return null;
  }
}


export class NBField {
  ele: HTMLElement;
  nb_host: null | NB = null
  v_row: number = 0;
  col: number = 0;
  latched_height: number = 0

  constructor(ele = document.createElement("div")) {
    this.ele = ele;
    this.ele.classList.add("nb-field")
  }

  latchHeight() {
    const { height } = this.ele.getBoundingClientRect();
    this.latched_height = height;
  }
}

export class NBBlankField extends NBField {
  constructor(width: number, height: number, force_height: boolean = false) {
    super()
    this.ele.classList.add("nb-blank-field");

    if (!force_height) {
      setTimeout(() => {
        this.ele.style.width = `${width}px`
        this.ele.style.height = `${height}px`
      }, 10)
    } else {
      this.ele.style.width = `${width}px`
      this.ele.style.height = `${height}px`
    }
  }

  delete() {
    if (this.nb_host) {
      let nb_host = this.nb_host;
      this.ele.style.height = `${0}px`
      setTimeout(() => { nb_host.removeField(this); }, 100)
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
  collapsed: boolean = false;
  listeners: Map<event_names, ((arg: EventObj) => void)[]> = new Map;

  constructor(name: string = "") {
    let ele = document.createElement("div");
    ele.append((<HTMLTemplateElement>document.querySelector("#panel")).content.cloneNode(true));

    super(<any>ele.firstElementChild)

    this.header = <any>this.ele.querySelector(".nb-header");
    this.label = <any>this.ele.querySelector(".nb-label");
    this.body = <any>this.ele.querySelector(".nb-body");

    this.label.innerHTML = name;

    this.header.addEventListener("pointerdown", async e => {
      if (this.nb_host) {
        if (!await this.nb_host.queueFieldDrag(e, this)) {
          this.setExpanded(this.ele.classList.contains("collapsed"))
        }
      } else {
        this.setExpanded(this.ele.classList.contains("collapsed"))
      }
    });
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

export class NBEditorField extends NBContentField<NBEditorField, "text_changed"> {

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
        Decoration.mark({ attributes: { style: `color: ${color} `, } }).range(start_char, end_char)
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

