/**
 * A custom scrollbar implementation
 */

export class ScrollHandler {
    private ele?: HTMLElement;
    private handle: HTMLDivElement;
    private scroll_host: HTMLElement;
    private scroll: HTMLDivElement;
    private scroll_end: HTMLDivElement;
    private scroll_beg: HTMLDivElement;
    private resize_obs: ResizeObserver;
    private mutate_obs: MutationObserver;
    private curr_height: number;
    private target_height: number;
    private target_pos: number;
    private curr_pos: number;
    private sb_distance: number;
    private scroll_box_ratio: number;
    private grab_diff: number;
    private scroll_amount: number;
    private HANDLE_MOVE: boolean;

    private bound_handle_wheel: (evnt: any) => void;
    private bound_handle_scroll: (evnt: any) => void;


    constructor(ele: HTMLElement, scroll_host: HTMLElement) {

        if (ele == scroll_host) {
            throw "Scroll bar host element must be different than scrolling element"
        }

        this.scroll_host = scroll_host;

        this.scroll = document.createElement("div");

        this.scroll.classList.add("no-scroll");
        this.scroll.classList.add("scroll");


        this.handle = document.createElement("div");
        this.handle.classList.add("scroll-handle");

        this.handle.addEventListener("pointerdown", this.handle_down.bind(this));
        this.handle.addEventListener("pointermove", this.handle_move.bind(this));
        this.handle.addEventListener("pointerup", this.handle_up.bind(this));

        this.scroll_beg = document.createElement("div");
        this.scroll_beg.classList.add("scroll-beg");

        this.scroll_end = document.createElement("div");
        this.scroll_end.classList.add("scroll-end");

        this.scroll.appendChild(this.handle);
        this.scroll.appendChild(this.scroll_beg);
        this.scroll.appendChild(this.scroll_end);

        this.scroll_host.appendChild(this.scroll);

        this.resize_obs = new ResizeObserver(this.handle_resize.bind(this));
        this.mutate_obs = new MutationObserver(this.handle_resize.bind(this));


        this.resize_obs.observe(ele);

        if (ele != scroll_host)
            this.resize_obs.observe(scroll_host);

        this.bound_handle_wheel = this.handle_wheel.bind(this);
        this.bound_handle_scroll = this.handle_scroll.bind(this);

        this.scroll_amount = 0;
        this.scroll_box_ratio = 1;
        this.sb_distance = 0;
        this.target_height = 0;
        this.curr_height = 0;
        this.curr_pos = 0;
        this.target_pos = 0;
        this.HANDLE_MOVE = false;
        this.grab_diff = 0;


        this.set_target(ele);
    }

    set_target(target: HTMLElement) {

        if (this.ele) {
            this.ele.removeEventListener("wheel", this.bound_handle_wheel);
            this.ele.removeEventListener("scroll", this.bound_handle_scroll);
            this.resize_obs.unobserve(this.ele);
            this.mutate_obs.disconnect();
        }

        this.resize_obs.observe(target);


        this.mutate_obs.observe(target, {
            subtree: this.ele != this.scroll_host,
            attributes: this.ele != this.scroll_host,
            childList: true
        });

        //for (const ele of Array.from(target.children))
        //    this.resize_obs.observe(ele);

        target.addEventListener("wheel", this.bound_handle_wheel);
        target.addEventListener("scroll", this.bound_handle_scroll);

        this.ele = target;

        this.handle_resize();
    }

    handle_resize() {
        if (this.ele) {
            const clientHeight = this.ele.clientHeight;

            this.scroll_amount = this.ele.scrollHeight - clientHeight;
            this.scroll_box_ratio = clientHeight / this.ele.scrollHeight;
            this.target_height = Math.max(this.scroll_box_ratio * this.scroll.clientHeight, 75);
            this.target_pos = ((this.ele.scrollTop / this.scroll_amount) * (this.scroll.clientHeight - this.target_height));
            this.sb_distance = (this.scroll.clientHeight - this.target_height);

            if (this.sb_distance < 1) {
                this.scroll.classList.add("no-scroll");
            } else {
                this.scroll.classList.remove("no-scroll");
            }

            this.handle_pos_adjust();
            this.handle_height_adjust();
            this.handle_scrolled();
        }
    }

    handle_height_adjust() {
        if (this.curr_height != this.target_height && this.ele) {
            const diff = (this.target_height - this.curr_height) * 0.5;

            if (Math.abs(diff) > 0.01) {
                this.curr_height += diff;

                setTimeout(this.handle_height_adjust.bind(this), 10);
            } else {
                this.curr_height = this.target_height;
            }

            if (this.curr_height + this.target_pos > this.ele.scrollHeight)
                this.curr_height -= ((this.curr_height + this.target_pos) - this.ele.scrollHeight);

            this.handle.style.height = this.curr_height + "px";
        }
    }

    handle_pos_adjust() {
        if (this.curr_pos != this.target_pos) {
            const diff = (this.target_pos - this.curr_pos) * 0.5;

            if (Math.abs(diff) > 10000000000000000000) {
                this.curr_pos += diff;

                setTimeout(this.handle_pos_adjust.bind(this), 10);
            } else {
                this.curr_pos = this.target_pos;
            }
            this.handle.style.top = this.curr_pos + "px";
        }
    }

    set_handle_pos_diff(diff: number) {
        this.set_handle_pos(this.target_pos + diff);
    }
    set_handle_pos(pos: number) {
        if (this.ele) {
            this.target_pos = Math.max(Math.min(pos, this.sb_distance), 0);
            this.handle_pos_adjust();
            this.handle_scrolled();
            const ratio = this.target_pos / this.sb_distance;
            this.ele.scrollTop = this.scroll_amount * ratio;
        }
    }

    handle_scrolled() {

    }

    handle_scroll(e: Event) {
        if (this.HANDLE_MOVE) return;
        this.handle_resize();
    }

    handle_move(e: PointerEvent) {
        if (this.HANDLE_MOVE) {
            const y = (e.clientY - getTop(this.scroll)) + this.grab_diff;
            this.set_handle_pos(y);
            e.preventDefault();
            e.stopImmediatePropagation();
            e.stopPropagation();
            return false;
        }
    }

    handle_up(e: PointerEvent) {
        this.handle.releasePointerCapture(e.pointerId);
        this.HANDLE_MOVE = false;
        e.preventDefault();
        e.stopPropagation();
    }

    handle_down(e: PointerEvent) {
        this.handle.setPointerCapture(e.pointerId);
        this.grab_diff = -(e.clientY - getTop(this.scroll)) + this.target_pos;
        this.HANDLE_MOVE = true;
    }
    handle_wheel(e: WheelEvent) {
        this.set_handle_pos_diff(Math.sign(e.deltaY) * 50 * this.scroll_box_ratio);
    }

}

function getTop(ele: HTMLElement | null) {
    let top = 0;

    while (ele) {
        top += ele.offsetTop;
        ele = ele.parentElement;
    }

    return top;
}
