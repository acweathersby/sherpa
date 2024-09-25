import { JSDebugEvent } from "js/radlr/radlr_wasm";
import * as radlr from "js/radlr/radlr_wasm.js";
import { WasmBytecodeCompiler } from "js/lab/wasm_bytecode_compiler_host";

export async function sleep(time_in_ms: number) {
  return new Promise(function (res) {
    setTimeout(res, time_in_ms)
  })
}

class PipelineNode<event_names = any> {
  private enables: PipelineNode<any>[] = [];
  private enabled_by: PipelineNode<any>[] = [];
  protected ENABLED = false;
  listeners: Map<any, ((arg: any) => void)[]> = new Map

  constructor(enabled_by: PipelineNode<any>[] = []) {
    for (const e of enabled_by) {
      e.enables.push(<any>this);
    }
    this.enabled_by = enabled_by
  }

  protected enable() {
    this.ENABLED = true;
    this.signal();
  }

  protected disable() {
    this.ENABLED = false;
    this.signal();
  }


  protected async changed(source: string, data: any) { }
  protected async start(data: object) { }
  protected async stop() { }

  private signal() {
    for (const enable of this.enables) {
      enable.pre_check();
    }
  }

  private async pre_check() {
    let data: any = {};
    for (const enabled_by of this.enabled_by) {
      if (enabled_by.ENABLED != true) {
        if (this.ENABLED == true) {
          this.ENABLED = false;
          this.signal();
          this.stop();
        }
        return;
      } else {
        data[enabled_by.name()] = enabled_by.data();
      }
    }

    await this.start(data);
  }


  protected data(): any { }

  protected name(): string { return ""; }

  protected emit<T extends keyof event_names, A = event_names[T], D = (arg: A) => void>(event: T, data: A) {
    for (const listener of this.listeners.get(event) ?? []) {
      (listener)(<any>data)
    }
  }

  protected haveListeners<T extends keyof event_names, A = event_names[T], D = (arg: A) => void>(event: T): boolean {
    return !!this.listeners.get(event)
  }

  addListener<T extends keyof event_names, A = event_names[T], D = (arg: A) => void>(event: T, listener: D) {
    if (!this.listeners.get(event)) {
      this.listeners.set(event, [<any>listener]);
    } else {
      this.listeners.get(event)?.push(<any>listener);
    }
  }

  removeListener<T extends keyof event_names, A = event_names[T], D = (arg: A) => void>(event: T, listener: D) {
    let listeners = this.listeners.get(event);
    if (listeners) {
      let index = listeners.findIndex(<any>listener);
      if (index > 0) {
        listeners.splice(index, 1)
      }
    }
  }
}

export class InputNode extends PipelineNode {
  input_string: string = "";

  protected name() { return "InputNode" }
  protected data() { return this.input_string; }

  update(grammar_string: string) {
    this.input_string = grammar_string;

    if (this.input_string) {
      this.enable();
    } else {
      this.disable();
    }
  }
}


export class GrammarDB extends PipelineNode<{
  "loading": void
  "loaded": void
  "failed": void
  "bytecode_ready": string
}> {
  static worker_path: string = ""

  parser_db: radlr.JSBytecodeParserDB | null = null;
  config: radlr.JSParserConfig | null = null;
  compile_nonce = 0
  compiler = new WasmBytecodeCompiler(GrammarDB.worker_path);

  DEDUP_ACTIVE = false

  protected name() { return "GrammarDB" }
  protected data() { return { parser_db: this.parser_db }; }

  constructor(...args: any[]) {
    super(...args);

    console.log("AA");

    this.compiler.addListener("grammar_compiled", () => {
      console.log("Grammar Compiled");
    });

    this.compiler.addListener("parser_compiled", bytecode_db_export => {
      try {
        if (this.parser_db) { this.parser_db.free(); this.parser_db = null }

        this.parser_db = radlr.import_bytecode_db(bytecode_db_export);


        if (this.haveListeners("bytecode_ready")) {
          this.emit("bytecode_ready", radlr.create_bytecode_disassembly(this.parser_db));
        }

        this.emit("loaded", void 0);

        this.enable()
      } catch (error) {
        console.log(error);
        var e = <radlr.PositionedErrors><any>error;
        console.log(e.get_error_at(0)?.message)
        this.emit("failed", void 0);
      }
    });
  }

  async load(compile_nonce: number, data: any) {
    if (compile_nonce != this.compile_nonce) {
      return;
    }

    this.emit("loading", void 0);
    this.compiler.compileGrammar(data.InputNode, {});

    return;
  }

  async start(data: any) {

    if (!await this.compiler.ready()) {
      console.log("Compiler not ready");
      return;
    }

    // Submit job for the compiler
    this.compile_nonce++;

    let compile_nonce = this.compile_nonce;

    await sleep(200);

    this.load(compile_nonce, data)
  }
}


export class Parser extends PipelineNode<{
  "error": {
    msg: string
  }
  "eof": void;
  "destroyed": void;
  "reset": void;
  "ready": void;
  "complete": void;
  "parser-db-created": {
    /*     bc_db: JSBytecodeParserDB */
  },
  "states-created": {
    /*     states: JSIRParser, */
  },
  "parser-created": {
    /*     parser: JSByteCodeParser */
  },
  "parser-reset": {
    /*     parser: JSByteCodeParser */
  },
  "step": {
    data: radlr.JSDebugPacket,
    input: string
  }
}> {
  input: string = ""
  parser: radlr.JSByteCodeParser | null = null
  db: radlr.JSBytecodeParserDB | null = null
  PARSING: boolean = false
  debugger_offset: number = -1;
  debugger_steps: radlr.JSDebugPacket[] = [];
  allow_play: boolean = false;
  play_interval: number = -1;
  active_search_symbols: Set<string> = new Set();
  last_step: any = null;
  active_scanner_state_source = '';
  step_to_next_action: boolean = false;
  parser_off: [number, number] = [0, 0];
  scanner_off: [number, number] = [0, 0];
  _restartParser: any;
  _stepInstruction: any;
  _stepAction: any;
  _togglePlay: any;

  protected name() { return "Parser" }
  protected data() { return {}; }

  protected async start(data: any) {
    // Submit job for the compiler

    if (this.parser) {
      this.parser.free; this.parser = null;
      this.emit("destroyed", { type: "destroyed" });
    }

    if (data.GrammarDB) {
      this.db = data.GrammarDB.parser_db;
      this.restart();
    }

    this.enable();
  }

  private createParser() {
    if (this.parser) {
      this.parser.free; this.parser = null;
      this.emit("destroyed", void 0);
    }

    if (this.input && this.db) {
      this.parser = radlr.JSByteCodeParser.new(this.input, this.db);
    }
  }

  private next(key_frame: boolean = false, step_to_next_action: boolean = false): boolean {

    let parser = this.parser;
    let input = this.input;

    if (!parser || !input)
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

      this.emit("step", { data: step, input });

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
          this.emit("complete", { type: "complete" });
          this.PARSING = false;
        } break outer;

        case JSDebugEvent.Error: {
          this.emit("error", { type: "error" });
          this.PARSING = false;
        } break outer;

        case JSDebugEvent.EndOfFile: {
          this.emit("eof", { type: "eof" });
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

  resetDebuggerData() {

  }

  private async play() {
    /*     while (this.allow_play && this.PARSING) {
          for (let i = 0; i < 10000 && this.PARSING; i++) {
            if (!this.next(false, false)) {
              break
            };
          }
    
          this.next(true, true);
        }
    
        this.allow_play = false;
    
        this.clearSteps() */
  }



  public restart(input: string = this.input, entry_point: string = "default") {
    if (input != this.input) {
      this.input = input;
      this.createParser();

      if (this.parser) {
        this.parser.init(entry_point);
        this.emit("reset", void 0);
      }
    }
  }

  public step() {
    if (this.ENABLED) {
      debugger
      this.next(false, false);
    }
  }
  public stepToNextParseState() { }
}

