
export class DebuggerButton {
    static buttons: Map<string, DebuggerButton> = new Map();

    public ele: HTMLElement

    constructor(ele: HTMLElement) {
        this.ele = ele;
        this.addEventListener = ele.addEventListener.bind(ele);
    }
    addEventListener<K extends keyof HTMLElementEventMap>(type: K, listener: (this: HTMLElement, ev: HTMLElementEventMap[K]) => any, options?: boolean | AddEventListenerOptions | undefined) { }
    // addEventListener(event: string, listener: (...e: any) => void) { }

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