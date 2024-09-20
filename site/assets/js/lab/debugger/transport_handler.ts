import { DebuggerButton } from "./debugger_io";
import { FlowNode } from "../../common/flow";
import * as radlr from "js/radlr/radlr_wasm.js";
import { CSTNode } from "./cst";
import { StateEffect, Range } from "@codemirror/state";
import { Decoration } from "@codemirror/view";
import { DebuggerData, EnableRestartButton, DebuggerError, EnableTransportButtons, DisableTransportButtons, DisableRestartButton } from "./debugger";
import { JSDebugEvent } from "js/radlr/radlr_wasm";
import { debugger_bus } from "./debugger_bus";

const highlight_effect = StateEffect.define<Range<Decoration>[]>();
const filter_effects = StateEffect.define<((from: number, to: number) => boolean)>();
const head_dec = Decoration.mark({ attributes: { style: "background-color: red" } });
const scan_dec = Decoration.mark({ attributes: { style: "background-color: blue" } });
const end_dec = Decoration.mark({ attributes: { style: "background-color: green" } });


export class TransportHandler extends FlowNode<DebuggerData> {

  parser: radlr.JSByteCodeParser | null = null;

  debugger_offset: number = -1;
  debugger_steps: radlr.JSDebugPacket[] = [];
  PARSING: boolean = false;
  allow_play: boolean = false;
  play_interval: number = -1;
  active_search_symbols: Set<string> = new Set();
  last_step: any = null;
  active_scanner_state_source = '';
  step_to_next_action: boolean = false;
  parser_off: [number, number] = [0, 0];
  scanner_off: [number, number] = [0, 0];
  cst_nodes: CSTNode[] = [];
  input: string = "";
  _restartParser: any;
  _stepInstruction: any;
  _stepAction: any;
  _togglePlay: any;


  constructor() {
    super();
    this._restartParser = this.restartParser.bind(this);
    this._stepInstruction = this.stepInstruction.bind(this);
    this._stepAction = this.stepAction.bind(this);
    this._togglePlay = this.togglePlayAction.bind(this);
  }

  clearSteps() {
    this.debugger_steps.forEach(s => s.free())
    this.debugger_steps.length = 0;
  }

  resetData(data: DebuggerData) {
    this.debugger_offset = 0;
    this.clearSteps()
    this.PARSING = false;
    this.allow_play = false;
    this.play_interval = -1;
    this.active_search_symbols = new Set();
    this.last_step = null;
    this.active_scanner_state_source = '';
    this.parser_off = [0, 0];
    this.scanner_off = [0, 0];
    this.cst_nodes = [];


    if (data.parser_host.editor) {
      this.input = data.parser_host.editor.state.doc.toString();
    } else {
      this.input = "";
    }
  }

  deleteParser() { if (this.parser) { this.parser.free(); } this.parser = null; }
  restartParser() { this.emit("TransportHandler_restartParser"); }
  stepInstruction() { this.emit("TransportHandler_stepInstruction"); }
  stepAction() { this.emit("TransportHandler_stepAction"); }
  togglePlayAction() { this.emit("TransportHandler_togglePlay"); }

  setupInputs() {
    DebuggerButton.get("restart").addEventListener("click", this._restartParser);
    DebuggerButton.get("step").addEventListener("click", this._stepInstruction);
    DebuggerButton.get("step-action").addEventListener("click", this._stepAction);
    DebuggerButton.get("play").addEventListener("click", this._togglePlay);
  }

  removeInputs() {
    DebuggerButton.get("restart").removeEventListener("click", this._restartParser);
    DebuggerButton.get("step").removeEventListener("click", this._stepInstruction);
    DebuggerButton.get("step-action").removeEventListener("click", this._stepAction);
    DebuggerButton.get("play").removeEventListener("click", this._togglePlay);
  }

  step(data: DebuggerData, key_frame: boolean = false, step_to_next_action: boolean = false): boolean {
    let { states, bytecode, grammar_ctx: { db }, parser_host } = data;

    let parser = this.parser;
    let view = parser_host.editor;
    let input = this.input;

    if (!parser || !db || !bytecode || !states || !view || !input)
      return false;

    let step: radlr.JSDebugPacket | undefined = undefined;

    outer: while (true) {

      if (step) {
        step.free();
      }

      step = parser.next();

      if (!step) {
        return false;
      }

      if (step.event == JSDebugEvent.ExecuteInstruction && step.complete) {
        key_frame = true;
      }

      debugger_bus.announce({
        type: "step",
        debug_data: step,
        db: db,
        bc_db: bytecode,
        parser,
        states,
        input,
        key_frame
      });

      switch (step.event) {

        case JSDebugEvent.ExecuteState: {
          break
        };

        case JSDebugEvent.ExecuteInstruction: {

          this.last_step = step;

          if (!step.is_scanner) {
            this.active_scanner_state_source = "";
          }

          if (step_to_next_action) { break; }

          break outer;
        };

        case JSDebugEvent.Complete: {
          this.emit("TransportHandler_disableTransportButtons");
          this.PARSING = false;
        } break outer;
        case JSDebugEvent.Error: {
          this.emit("TransportHandler_disableTransportButtons");
          this.PARSING = false;
        } break outer;
        case JSDebugEvent.EndOfFile: {
          this.emit("TransportHandler_disableTransportButtons");
          this.PARSING = false;
        } break outer;
        default: break outer;
      }
    }

    if (step) {
      step.free();
    }

    return true;
  }

  update(t: string, data: DebuggerData): FlowNode<DebuggerData>[] {
    let base_return = [this, new EnableRestartButton];
    switch (t) {
      case "init":
        this.setupInputs();
      // Intentional fall through
      case "TransportHandler_restartParser": {
        this.deleteParser();


        if (!data.parser_host.editor.state.doc.toString())
          return [...base_return, new DebuggerError("data is empty")];

        if (!data.bytecode)
          return [...base_return, new DebuggerError("Bytecode is invalid")];

        if (!data.grammar_ctx.db)
          return [...base_return, new DebuggerError("Database is invalid")];

        let parser_input = data.debugger_entry_selection.value;

        let bc = data.bytecode.bytecode;
        let ep = data.bytecode.entry_points;



        let input = "hello world";




        try {
          this.parser = radlr.JSByteCodeParser.new(data.parser_host.editor.state.doc.toString(), data.bytecode);

          this.parser.init(parser_input);

          debugger_bus.announce({
            type: "parser-created",
            parser: this.parser
          })

          debugger_bus.announce({
            type: "parser-reset",
            parser: this.parser
          })
        } catch (e) {
          console.error(e)
          return [...base_return, new DebuggerError("Parser Compiler Error")];
        }

        this.resetData(data);
        this.PARSING = true;

        return [...base_return, new EnableTransportButtons];
      }
      case "TransportHandler_stepInstruction": {
        this.step(data, true);
        this.clearSteps()
        return base_return
      }
      case "TransportHandler_stepAction": {
        this.step(data, true, true);
        this.clearSteps()
        return base_return
      }
      case "TransportHandler_togglePlay": {
        this.allow_play = !this.allow_play;
        if (this.allow_play) {
          this.emit("TransportHandler_play");
        }
        return base_return
      }
      case "TransportHandler_step": {
        if (this.PARSING)
          this.step(data, true);


        this.clearSteps()
        return base_return
      }
      case "TransportHandler_play": {
        if (this.allow_play && this.PARSING) {
          for (let i = 0; i < 10000 && this.PARSING; i++) {
            if (!this.step(data, false, false)) {
              break
            };
          }

          this.step(data, true, true);

          if (this.PARSING) { this.emit("TransportHandler_play"); }
        } else {
          this.allow_play = false;
        }
        this.clearSteps()
        return base_return
      }
      case "Input_changed": {
        return base_return;
      }
      case "TransportHandler_disableTransportButtons":
        return [...base_return, new DisableTransportButtons];
      case "db_deleted":
      case "config_changed": {
        this.deleteParser();
        this.removeInputs();
        return [new DisableTransportButtons, new DisableRestartButton];
      }
      default:
        return base_return;
    }
  }
}
