export class DividerHandler {
    containing_element: HTMLElement;
    dividers: DividerHandle[] = [];
    width_basis: number = 1;
    min_width_ratio: number = 0.1;

    constructor(container_element: HTMLElement) {
        this.containing_element = container_element;

        this.width_basis = this.containing_element.scrollWidth;
        console.log(this.width_basis)
        for (const child of Array.from(this.containing_element.children)) {
            if (child.classList.contains("adjustable")) {
                console.log(child.clientWidth)
                const child_ratio = child.clientWidth / this.width_basis;
                let div = new DividerHandle(<HTMLElement>child, child_ratio, this);
                div.deltaRatio(0);
                this.dividers.push(div);
            }

        }
    }

    handleUpdate(handle: DividerHandle, pixel_diff: number) {
        let handle_found = false;
        let curr_size = 0;

        let diff_amount = pixel_diff / this.containing_element.scrollWidth;

        for (let i = 0; i < this.dividers.length; i++) {
            let div = this.dividers[i];
            if (div == handle) {
                handle_found = true;
                curr_size += handle.curr_ratio;
                if (diff_amount > 0) {
                    let minimum = (this.dividers.length - 1 - i) * this.min_width_ratio;
                    let curr_min = this.dividers.slice(i + 1).reduce((a, i) => a + i.curr_ratio, 0);

                    if (minimum > (curr_min - diff_amount)) {
                        diff_amount = curr_min - minimum;
                    }

                    div.deltaRatio(diff_amount);
                } else {

                    if (div.curr_ratio + diff_amount < this.min_width_ratio) {
                        let possible = this.min_width_ratio - div.curr_ratio;
                        div.deltaRatio(possible);
                        diff_amount = possible;

                    } else {
                        div.deltaRatio(diff_amount);
                    }
                }
            } else if (handle_found) {
                if (diff_amount > 0) {
                    if (div.curr_ratio - diff_amount < this.min_width_ratio) {
                        let possible = div.curr_ratio - this.min_width_ratio;
                        div.deltaRatio(-possible);
                        diff_amount -= possible
                    } else {
                        div.deltaRatio(-diff_amount);
                        diff_amount = 0;
                    }
                } else {
                    div.deltaRatio(-diff_amount);
                    diff_amount = 0;
                }
            } else {
                curr_size += handle.curr_ratio;
            }

            if (diff_amount == 0) {
                return
            }
        }
    }
}


export class DividerHandle {
    ele: HTMLElement;
    handle: HTMLElement;
    curr_ratio: number;
    par: DividerHandler;
    ptr_id: number = -1;
    start_pos: number = 0;

    constructor(child: HTMLElement, curr_ratio: number, par: DividerHandler) {
        this.ele = child;

        this.handle = document.createElement("div");
        this.handle.classList.add("vertical-divider-handle");

        this.handle.addEventListener("pointerdown", this.pointerdown.bind(this));
        this.handle.addEventListener("pointerup", this.pointerup.bind(this));
        this.handle.addEventListener("pointermove", this.pointermove.bind(this));
        this.ele.appendChild(this.handle);
        this.curr_ratio = curr_ratio;
        this.par = par;
    }

    pointerdown(e: PointerEvent) {
        this.ptr_id = e.pointerId;
        this.start_pos = e.clientX;
        this.handle.setPointerCapture(this.ptr_id);
    }
    pointerup(e: PointerEvent) {
        this.handle.releasePointerCapture(this.ptr_id);
        this.ptr_id = -1
    }
    pointermove(e: PointerEvent) {
        if (this.ptr_id < 0) {
            return;
        }
        const diff = e.clientX - this.start_pos;

        this.par.handleUpdate(this, diff);

        this.start_pos = e.clientX
    }
    deltaRatio(amount: number) {
        this.curr_ratio += amount;
        this.ele.style.width = (Math.round(this.curr_ratio * 1000) / 10) + "%";
    }

}