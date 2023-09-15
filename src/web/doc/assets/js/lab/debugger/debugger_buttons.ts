
export class DebuggerButton {
    static buttons: Map<string, DebuggerButton> = new Map();

    public ele: HTMLElement

    removeEventListener: HTMLElement["removeEventListener"];
    addEventListener: HTMLElement["addEventListener"];

    constructor(ele: HTMLElement) {
        this.ele = ele;
        this.addEventListener = ele.addEventListener.bind(ele);
        this.removeEventListener = ele.removeEventListener.bind(ele);
    }

    set active(active: boolean) {
        if (active)
            this.ele.classList.add("active")
        else
            this.ele.classList.remove("active")
    }

    set disable(active: boolean) {
        if (active)
            this.ele.classList.add("disable")
        else
            this.ele.classList.remove("disable")
    }

    static gatherButtons() {
        for (const ele of Array.from(document.getElementsByClassName("debugger-button"))) {
            console.log(ele)
            DebuggerButton.buttons.set(ele.id, new DebuggerButton(<HTMLElement>ele));
        }
    }

    static get(button_name: string): DebuggerButton {
        return <any>DebuggerButton.buttons.get(button_name);
    }
}


export class DebuggerCheckbox {
    static buttons: Map<string, DebuggerCheckbox> = new Map();

    public _ele: HTMLInputElement

    removeEventListener: HTMLElement["removeEventListener"];
    addEventListener: HTMLElement["addEventListener"];

    constructor(ele: HTMLInputElement) {
        this._ele = ele;
        this.addEventListener = ele.addEventListener.bind(ele);
        this.removeEventListener = ele.removeEventListener.bind(ele);
    }

    set active(active: boolean) {
        if (active)
            this._ele.classList.add("active")
        else
            this._ele.classList.remove("active")
    }

    set disable(active: boolean) {
        if (active)
            this._ele.classList.add("disable")
        else
            this._ele.classList.remove("disable")
    }

    get ele(): HTMLInputElement {
        return this._ele;
    }

    static gatherCheckBoxes() {
        for (const ele of Array.from(document.getElementsByClassName("debugger-checkbox"))) {
            console.log(ele)
            DebuggerCheckbox.buttons.set(ele.id, new DebuggerCheckbox(<HTMLInputElement>ele));
        }
    }

    static get(check_box_name: string): DebuggerCheckbox {
        return <any>DebuggerCheckbox.buttons.get(check_box_name);
    }
}