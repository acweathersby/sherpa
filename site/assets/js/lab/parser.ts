import { JSDebugEvent } from "js/radlr/radlr_wasm";
import * as radlr from "js/radlr/radlr_wasm.js";

export class Parser {
  input: string = ""
  parser: radlr.JSByteCodeParser | null = null
  db: radlr.JSBytecodeParserDB
  PARSING: boolean = false
  INITIALIZED: boolean = false

  on_state: ((dbg: radlr.JSDebugPacket) => { should_stop: boolean }) | null = null;

  on_instruction: ((dbg: radlr.JSDebugPacket) => { should_stop: boolean }) | null = null;

  on_shift: ((shift_data: {
    byte_offset: number,
    byte_len: number,
    col: number,
    line: number,
    token: string,
    token_id: number,
    db: radlr.JSBytecodeParserDB
  }) => void) | null = null;

  on_reduce: ((reduce_data: {
    non_terminal_id: number,
    rule_id: number,
    symbols: number, db: radlr.JSBytecodeParserDB
  }) => void) | null = null;

  on_error: ((arg: radlr.JSDebugPacket) => void) | null = null;

  on_complete: (() => void) | null = null;

  constructor(db: radlr.JSBytecodeParserDB, input: string) {
    this.db = db;
    this.input = input;
  }

  destroy() {
    if (this.parser)
      this.parser.free();
  }

  private next(key_frame: boolean = false, step_to_next_action: boolean = false): boolean {

    let parser = this.parser;
    let input = this.input;
    let db = this.db;

    if (!parser)
      return false;

    let step: radlr.JSDebugPacket | undefined = undefined;

    let i = 0;

    outer: while (true) {
      i++;
      step_to_next_action = false;

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

      switch (step.event) {

        case JSDebugEvent.ExecuteState: {
          if (this.on_state && this.on_state(step).should_stop) {
            break outer
          }
          break
        };

        case JSDebugEvent.ExecuteInstruction: {


          if (this.on_instruction && !this.on_instruction(step).should_stop) {
            break outer;
          }

          break;
        };

        case JSDebugEvent.Complete: {
          if (this.on_complete) {
            this.on_complete();
          }
          this.PARSING = false;
        } break outer;

        case JSDebugEvent.Error: {
          if (this.on_error) {
            this.on_error(step);
          }
          this.PARSING = false;
        } break outer;

        case JSDebugEvent.EndOfFile: {
          if (this.on_error) {
            this.on_error(step);
          }
          this.PARSING = false;
        } break outer;
        case JSDebugEvent.Shift: {
          if (this.on_shift) {
            this.on_shift({
              byte_offset: step.offset_start,
              byte_len: step.offset_end - step.offset_start,
              col: 0,
              line: 0,
              token: this.input.slice(step.offset_start, step.offset_end),
              token_id: step.ctx.tok_id,
              db
            })
          }
        } break
        case JSDebugEvent.Reduce: {
          if (this.on_reduce) {
            this.on_reduce({
              non_terminal_id: step.nonterminal_id,
              rule_id: step.rule_id,
              symbols: step.symbol_count, db
            });
          }
        } break
        case JSDebugEvent.Skip: break
        case JSDebugEvent.Undefined:
        default: break outer;
      }
    }

    if (step) {
      step.free();
    }

    return true;
  }

  public init(parser_entry_name: string = "default", input: string = this.input) {
    this.INITIALIZED = false;
    this.input = input;

    let selected_entry = "default";

    for (const [name, entry] of this.db.entry_points) {
      if (name == parser_entry_name) {
        selected_entry = name;
        break;
      }
    }

    if (this.parser) {
      this.parser.free();
    }

    try {
      this.parser = radlr.JSByteCodeParser.new(this.input, this.db);
      this.parser.init(selected_entry, false);
      this.INITIALIZED = true;
      this.PARSING = true;
    } catch (e) {
      console.error(e);
    }

  }

  public play() {
    if (this.INITIALIZED && this.PARSING) {
      this.next(false, false);
    }
  }
}


