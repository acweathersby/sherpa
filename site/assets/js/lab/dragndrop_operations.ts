import { sleep } from "./pipeline";
import { NBField, NBBlankField, NB, NBColumn } from "./notebook";

class DragOperation {
  pointer_timeout = 0;

  bound_pointer_move: (e: PointerEvent) => void;
  bound_pointer_up: (e: PointerEvent) => void;

  start_pos_x = 0;
  start_pos_y = 0;
  curr_pos_x = 0;
  curr_pos_y = 0;
  offset_x = 0;
  offset_y = 0;
  active_drag = false;

  mouse_up_trigger_count = 0;
  move_threshold = 10;

  private drag_committed = false;

  drag_delay_ms: number = 200;

  will_drag: Promise<boolean> | null = null;

  constructor(e: PointerEvent, move_threshold = 10, drag_delay_ms: number = 200) {
    this.drag_delay_ms = drag_delay_ms;
    this.move_threshold = move_threshold;
    this.bound_pointer_up = this.pointerUp.bind(this);
    this.bound_pointer_move = this.pointerMove.bind(this);
    document.addEventListener("pointermove", this.bound_pointer_move);
    document.addEventListener("pointerup", this.bound_pointer_up);
  }

  initialize(e: PointerEvent) {
    this.will_drag = this.init_async(e);
  }

  async init_async(e: PointerEvent): Promise<boolean> {

    if (this.criteriaMet()) {
      let trigger_start = this.mouse_up_trigger_count;

      await sleep(this.drag_delay_ms);

      if (trigger_start != this.mouse_up_trigger_count) {
        return false;
      } else {
        let { x, y } = e;

        this.start_pos_x = x;
        this.start_pos_y = y;

        this.curr_pos_x = x;
        this.curr_pos_y = y;

        this.active_drag = true;

        this.start(e);
        return true;
      }
    }

    return false;
  }

  async willDrag(): Promise<boolean> {
    if (this.will_drag) {
      return await this.will_drag;
    } else {
      return false;
    }
  }



  get diff_x(): number {
    return this.curr_pos_x - this.start_pos_x
  }
  get diff_y(): number {
    return this.curr_pos_y - this.start_pos_y
  }

  protected criteriaMet(): boolean { return true; }
  protected start(e: PointerEvent) { }
  protected commit() { }
  protected update() { }
  protected end() { }

  private pointerMove(e: PointerEvent) {
    if (this.active_drag) {

      let { x, y } = e;

      this.curr_pos_x = x;
      this.curr_pos_y = y;

      if (!this.drag_committed) {
        const abs_diff = Math.sqrt((x - this.start_pos_x) ** 2 + (y - this.start_pos_y) ** 2);
        if (abs_diff >= this.move_threshold) {
          this.drag_committed = true;
          this.commit();
          this.update();
        }
      } else {
        this.update();
      }

      if (e.stopPropagation) e.stopPropagation();
      if (e.preventDefault) e.preventDefault();
      return false;
    }
  }

  private pointerUp(e: PointerEvent) {
    this.mouse_up_trigger_count++;

    if (this.drag_committed) {
      this.end();
    }

    document.removeEventListener("pointermove", this.bound_pointer_move);
    document.removeEventListener("pointerup", this.bound_pointer_up);
  }
}


export class MoveFieldDragOperation extends DragOperation {

  drag_field: NBField;
  drag_start_col: number = -1;
  drag_start_row: number = -1;

  drag_target_col: number = -1;
  drag_target_row: number = -1;
  placeholder: NBBlankField | null = null;

  start_width = 0;
  start_height = 0;

  nb: NB;

  constructor(e: PointerEvent, drag_field: NBField) {
    super(e);
    if (drag_field.nb_host) {
      this.nb = drag_field.nb_host;
      this.drag_field = drag_field;
      this.drag_start_col = this.drag_field.col;
      this.drag_start_row = this.drag_field.v_row;
      this.drag_target_col = this.drag_field.col;
      this.drag_target_row = this.drag_field.v_row;
      this.initialize(e);
    } else {
      throw "Notebook host not found";
    }
  }

  protected criteriaMet(): boolean {
    let col_index = -1;
    for (const col of this.nb.columns) {
      col_index++;
      let row_index = col.find(this.drag_field);
      if (row_index >= 0) {
        return true;
      }
    }
    return false;
  }

  protected start(e: PointerEvent) {
    let { x: ele_x, y: ele_y, width, height } = this.drag_field.ele.getBoundingClientRect();

    this.offset_x = ele_x;
    this.offset_y = ele_y;

    this.start_height = height;
    this.start_width = width;
  }

  protected updateDragPos(field: NBField) {
    let { x, y } = this.getDragPos();
    field.ele.style.top = `${y}px`;
    field.ele.style.left = `${x}px`;
  }

  protected getDragPos() {
    let x = (this.curr_pos_x - this.start_pos_x) + this.offset_x;
    let y = (this.curr_pos_y - this.start_pos_y) + this.offset_y;
    return { x, y };
  }

  protected commit() {

    this.nb.calculateHeights();

    this.drag_field.ele.classList.add("dragging");
    this.updateDragPos(this.drag_field);
    this.drag_field.ele.style.width = this.start_width + "px";
    this.drag_field.ele.style.height = this.start_height + "px";

    // Remove the field from the host row and insert a placeholder of the correct dimensions.
    // the active field should now be attached the document body. 
    const col = this.nb.columns[this.drag_start_col];

    col.remove(this.drag_field);

    this.placeholder = new NBBlankField(this.start_width, this.start_height, true);
    col.add(this.placeholder, this.drag_start_row);

    document.body.appendChild(this.drag_field.ele);

  }

  protected update() {
    this.updateDragPos(this.drag_field);

    for (const col of this.nb.columns) {
      const insert_data = col.pointInside(this.curr_pos_x, this.curr_pos_y);

      if (insert_data && (this.drag_target_col != col.index || this.drag_target_row != insert_data.insert_row)) {

        if (this.placeholder) {
          this.placeholder.delete();
          this.placeholder = null;
          this.nb.columns[this.drag_target_col].distributeHeight();
        }

        this.drag_target_col = col.index;
        this.drag_target_row = insert_data.insert_row;

        this.placeholder = new NBBlankField(this.start_width, this.start_height);
        col.add(this.placeholder, this.drag_target_row);
        this.nb.columns[this.drag_target_col].distributeHeight([{ index: this.placeholder.r_row, height: this.start_height }]);
      }
    }

  }

  protected end() {
    this.drag_field.ele.classList.remove("dragging");
    this.drag_field.ele.style.top = ``;
    this.drag_field.ele.style.left = ``;
    this.drag_field.ele.style.width = ``;
    this.drag_field.ele.style.height = ``;

    this.nb.addField(this.drag_field, this.drag_target_col, this.drag_target_row);

    if (this.placeholder) {
      this.nb.removeField(this.placeholder);
      this.placeholder = null;
    }

    this.nb.columns[this.drag_target_col].distributeHeight([{ index: this.drag_field.v_row, height: this.start_height }]);
  }
}


export class ResizeFieldOperation extends DragOperation {

  top_field: NBField;
  bottom_field: NBField;
  target_column: NBColumn

  drag_target_col: number = -1;
  drag_target_row: number = -1;
  placeholder: NBBlankField | null = null;

  start_total_height = 0;
  start_top_height = 0;

  nb: NB;

  constructor(e: PointerEvent, drag_field: NBField) {
    super(e, 1, 0);
    if (drag_field.nb_host) {
      this.nb = drag_field.nb_host;
      this.top_field = drag_field;
      this.bottom_field = drag_field;
      this.target_column = this.nb.columns[drag_field.col];
      this.initialize(e);
    } else {
      throw "Notebook host not found";
    }
  }

  protected criteriaMet(): boolean {
    return this.top_field.r_row < this.target_column.cells.length - 1;
  }

  protected start(e: PointerEvent) {
    let { x: ele_x, y: ele_y, width, height } = this.top_field.ele.getBoundingClientRect();

    this.bottom_field = this.target_column.cells[this.top_field.r_row + 1];

    this.offset_x = ele_x;
    this.offset_y = ele_y;

    this.target_column.latchHeights()
    this.start_total_height = this.top_field.latched_height + this.bottom_field.latched_height;
    this.start_top_height = this.top_field.latched_height;
  }

  protected update(): void {
    let top_height = Math.min(Math.max(160, this.start_top_height + this.diff_y), this.start_total_height - 160);
    let bottom_height = this.start_total_height - top_height;

    this.top_field.ele.style.height = `${top_height}px`;
    this.bottom_field.ele.style.height = `${bottom_height}px`;

    this.top_field.ele.style.transition = "unset"
    this.bottom_field.ele.style.transition = "unset"
  }

  protected end(): void {

    this.target_column.latchHeights()
    this.target_column.distributeHeight();

    this.top_field.ele.style.transition = ""
    this.bottom_field.ele.style.transition = ""
  }
}