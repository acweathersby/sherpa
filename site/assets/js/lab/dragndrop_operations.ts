import { sleep } from "./pipeline";
import { NBField as NBCell, NBBlankField, NB, NBColumn, MIN_EXPANDED_FIELD_HEIGHT, NBField } from "./notebook";

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
    document.addEventListener("pointermove", this.bound_pointer_move, { capture: true, passive: false });
    document.addEventListener("pointerup", this.bound_pointer_up, { capture: true, passive: false, once: true });

    document.body.style.touchAction = "none";
    document.body.setPointerCapture(e.pointerId);
    this.squashEvent(e);
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
  protected update(e: PointerEvent) { }
  protected end() { }

  private pointerMove(e: PointerEvent): boolean {
    if (this.active_drag) {

      let { x, y } = e;

      this.curr_pos_x = x;
      this.curr_pos_y = y;

      if (!this.drag_committed) {
        const abs_diff = Math.sqrt((x - this.start_pos_x) ** 2 + (y - this.start_pos_y) ** 2);
        if (abs_diff >= this.move_threshold) {
          this.drag_committed = true;
          this.commit();
          this.update(e);
        }
      } else {
        this.update(e);
      }

      this.squashEvent(e);
      return false;
    }

    this.squashEvent(e);
    return false;
  }

  private squashEvent(e: PointerEvent) {
    e.preventDefault();
    e.stopPropagation();
    e.stopImmediatePropagation();
  }

  private pointerUp(e: PointerEvent): boolean {

    this.mouse_up_trigger_count++;

    if (this.drag_committed) {
      this.end();
    }

    document.body.releasePointerCapture(e.pointerId);
    document.removeEventListener("pointermove", this.bound_pointer_move, { capture: true });
    document.removeEventListener("pointerup", this.bound_pointer_up, { capture: true });

    this.squashEvent(e);
    return false;
  }
}


export class MoveFieldDragOperation extends DragOperation {

  drag_field: NBCell;

  drag_start_col: number = -1;
  drag_start_row: number = -1;

  drag_target_col: number = -1;
  drag_target_row: number = -1;


  placeholder: NBBlankField | null = null;
  replacement: NBField | null = null;
  replacement_original_height: number = 0
  replacement_original_row: number = 0
  replacement_original_col: number = 0

  start_width = 0;
  start_height = 0;

  swap_mode: boolean = false;

  nb: NB;

  constructor(e: PointerEvent, drag_field: NBCell) {
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

    this.swap_mode = e.ctrlKey;

    this.offset_x = ele_x;
    this.offset_y = ele_y;

    this.start_height = this.nb.columns[this.drag_field.col].cell_count == 1 ? height / 2 : height;
    this.start_width = width;
  }

  protected updateDragPos(field: NBCell) {
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
      const insert_data = col.pointInside(this.curr_pos_x, this.curr_pos_y, 100);

      if (insert_data) {
        const swap_mode = this.swap_mode
        const different_pos = (this.drag_target_col != col.index || this.drag_target_row != insert_data.insert_row);
        const insert_column = (insert_data.alignment > 0 && col.cell_count > 0);
        const max_columns = this.nb.columns.length >= this.nb.max_columns;


        if (swap_mode) {
          if (different_pos && this.placeholder) {
            // Take the old position and swap it with our new position

            if (this.replacement) {
              const old_replacement = this.replacement;
              const old_replacement_col = this.nb.columns[old_replacement.col];
              this.replacement = col.cells[insert_data.insert_row];
              this.start_height = this.replacement.latched_height;

              old_replacement_col.swap(this.placeholder, old_replacement.r_row);
              col.swap(this.placeholder, insert_data.insert_row);

              this.drag_target_col = col.index;
              this.drag_target_row = insert_data.insert_row;

            } else {
              // The new cell should now take the spot of the replacement

              this.replacement = col.cells[insert_data.insert_row];
              this.start_height = this.replacement.latched_height;

              col.swap(this.placeholder, insert_data.insert_row);

              this.drag_target_col = col.index;
              this.drag_target_row = insert_data.insert_row;
            }
          }
        } else {


          if (different_pos || (insert_column && !max_columns)) {

            let exiting_empty = false;
            let new_col = false;

            if (this.placeholder) {
              this.placeholder.delete();
              this.placeholder = null;
              let target_col = this.nb.columns[this.drag_target_col];
              target_col.distributeHeight();

              if (target_col.cell_count == 0) {
                this.nb.removeCol(this.drag_target_col);
                exiting_empty = true;
              }
            }

            if (insert_data.alignment > 0 && !exiting_empty && !max_columns) {
              let insertion_index = insert_data.alignment == 1 ? col.index : col.index + 1;
              this.nb.insertCol(insertion_index);
              this.drag_target_col = insertion_index;
              this.drag_target_row = 0;
              new_col = true;
            } else {
              this.drag_target_col = col.index;
              this.drag_target_row = insert_data.insert_row;
            }

            {
              let col = this.nb.columns[this.drag_target_col];
              this.placeholder = new NBBlankField(this.start_width, this.start_height);
              const height = new_col  ? col.max_free() : Math.min(this.start_height, col.max_free());
              console.log({height})
              col.add(this.placeholder, this.drag_target_row);
              col.distributeHeight([{ index: this.placeholder.r_row, height }]);
            }
          }
        }
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

  static stick_zone_size = 20;

  placeholder: NBBlankField | null = null;
  steps: number[] = []

  sets: {
    offset_y: number,
    start_total_height: number,
    start_top_height: number
    top: NBCell,
    bottom: NBCell,
    col: NBColumn
  }[] = []

  nb: NB;

  constructor(e: PointerEvent, start_cell: NBCell) {
    super(e, 1, 0);
    if (start_cell.nb_host) {
      this.nb = start_cell.nb_host;

      this.addSet(start_cell);

      const adjust_adjacent = e.altKey;

      let { y, height } = start_cell.ele.getBoundingClientRect();
      let main_step = y + height;

      for (const col of this.nb.columns) {
        for (const cell of col.cells) {
          let { y, height } = cell.ele.getBoundingClientRect();
          let step = y + height;



          let diff = Math.abs(main_step - step);

          if (diff <= 5 && adjust_adjacent && cell != start_cell) {
            this.addSet(cell);
          } else {
            this.steps.push(step);
          }
        }
      }

      this.initialize(e);
    } else {
      throw "Notebook host not found";
    }
  }

  private addSet(cell: NBCell) {
    this.sets.push({
      offset_y: 0,
      start_total_height: 0,
      start_top_height: 0,
      top: cell,
      bottom: cell,
      col: this.nb.columns[cell.col]
    });
  }

  protected criteriaMet(): boolean {

    this.sets = this.sets.filter((set) => {
      return set.top.r_row < set.col.cells.length - 1
    })


    return this.sets.length > 0;
  }

  protected start(e: PointerEvent) {
    for (const set of this.sets) {

      let { y: ele_y } = set.top.ele.getBoundingClientRect();

      set.bottom = set.col.cells[set.top.r_row + 1];

      set.offset_y = ele_y;

      set.col.latchHeights()
      set.start_total_height = set.top.latched_height + set.bottom.latched_height;
      set.start_top_height = set.top.latched_height;
    }
  }

  protected update(e: PointerEvent): void {

    let stick_zone_size = e.ctrlKey ? ResizeFieldOperation.stick_zone_size : 0;

    for (const set of this.sets) {

      let top_height = Math.min(Math.max(MIN_EXPANDED_FIELD_HEIGHT, set.start_top_height + this.diff_y), set.start_total_height - MIN_EXPANDED_FIELD_HEIGHT);

      if (stick_zone_size > 0) {

        for (const step of this.steps) {

          let diff = (top_height + set.offset_y) - step;

          if (Math.abs(diff) < stick_zone_size) {
            top_height -= diff;
            break
          }
        }
      }

      let bottom_height = set.start_total_height - top_height;

      set.top.ele.style.height = `${top_height}px`;
      set.bottom.ele.style.height = `${bottom_height}px`;

      set.top.ele.style.transition = "unset"
      set.bottom.ele.style.transition = "unset"
    }
  }

  protected end(): void {
    for (const set of this.sets) {
      set.col.latchHeights()
      set.col.distributeHeight();
      set.top.ele.style.transition = ""
      set.bottom.ele.style.transition = ""
    }
  }
}