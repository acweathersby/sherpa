
export class DebuggerButton {

  static className: string = "debugger-button";
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

  static gatherButtons(host_ele: HTMLElement = document.body) {
    for (const ele of Array.from(host_ele.getElementsByClassName(DebuggerButton.className))) {
      if (DebuggerButton.buttons.has(ele.id)) {
        if (DebuggerButton.buttons.get(ele.id)?.ele != ele) {
          throw new Error(`Button id ${ele.id} has been obfuscated!`);
        }
      } else {
        DebuggerButton.buttons.set(ele.id, new DebuggerButton(<HTMLElement>ele));
      }
    }
  }

  static get(button_name: string): DebuggerButton {
    return <any>DebuggerButton.buttons.get(button_name);
  }
}


export class DebuggerCheckbox {

  static className: string = "debugger-checkbox";

  static checkboxes: Map<string, DebuggerCheckbox> = new Map();

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

  static gatherCheckBoxes(host_ele: HTMLElement = document.body) {
    for (const ele of Array.from(host_ele.getElementsByClassName(DebuggerCheckbox.className))) {
      if (DebuggerCheckbox.checkboxes.has(ele.id)) {
        if (DebuggerCheckbox.checkboxes.get(ele.id)?._ele != ele) {
          throw new Error(`Button id ${ele.id} has been obfuscated!`);
        }
      } else {
        DebuggerCheckbox.checkboxes.set(ele.id, new DebuggerCheckbox(<HTMLInputElement>ele));
      }
    }
  }

  static get(check_box_name: string): DebuggerCheckbox {
    return <any>DebuggerCheckbox.checkboxes.get(check_box_name);
  }
}


export class DebuggerField {
  static className: string = "debugger-field";

  static fields: Map<string, DebuggerField> = new Map();

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

  static gatherFields(host_ele: HTMLElement = document.body) {
    for (const ele of Array.from(host_ele.getElementsByClassName(DebuggerField.className))) {
      if (DebuggerField.fields.has(ele.id)) {
        if (DebuggerField.fields.get(ele.id)?._ele != ele) {
          throw new Error(`Button id ${ele.id} has been obfuscated!`);
        }
      } else {
        DebuggerField.fields.set(ele.id, new DebuggerField(<HTMLInputElement>ele));
      }
    }
  }

  static get(check_box_name: string): DebuggerField {
    return <any>DebuggerField.fields.get(check_box_name);
  }
}