import { DebuggerButton } from "./debugger/debugger_io";

export class Controls {
  play_button: DebuggerButton
  step_button: DebuggerButton
  jump_button: DebuggerButton
  controls: HTMLDivElement

  constructor() {
    this.controls = <HTMLDivElement>document.getElementById("controls");
    this.play_button = DebuggerButton.get("play");
    this.step_button = DebuggerButton.get("step");
    this.jump_button = DebuggerButton.get("jump");
  }

  setActive(active=true) {
    if(active && this.controls.classList.contains("inactive")){
      this.controls.classList.replace("inactive", "active");
    } if(!active && this.controls.classList.contains("active")){
      this.controls.classList.replace("active", "inactive");
    }
  }
}