import * as radlr from "js/radlr/radlr_wasm.js";
import * as pipeline from "./pipeline";
import { NBContentField, NBEditorField } from "./notebook";
import { Controls } from "./control";

type StatesLU = Map<number, { pseudo_code: string }>;

class StatesView {
  parse_states: number[] = [];
  scan_states: number[] = [];
  ele: HTMLDivElement

  constructor() {
    this.ele = document.createElement("div");
    this.ele.classList.add("stack-view");
  }

  handleInstruction(dv: StatefullDV, is_scanner: boolean) {
    switch (dv.u8()) {
      case radlr.Opcode.Pass: {
        if (is_scanner) {
          this.scan_states.pop();
        } else {
          this.parse_states.pop();
        }

        this.write();
      } break
      case radlr.Opcode.Fail: {
        if (is_scanner) {
          this.scan_states.pop();
        } else {
          this.parse_states.pop();
        }

        this.write();
      } break
      case radlr.Opcode.Accept: {
        if (is_scanner) {
          this.scan_states.pop();
        } else {
          this.parse_states.pop();
        }

        this.write();
      } break
      case radlr.Opcode.PushGoto: {
        let parse_mode = dv.u8();
        let address = dv.u32();

        if (is_scanner) {
          this.scan_states.push(address);
        } else {
          this.parse_states.push(address);
        }

        this.write();

      } break;
    }
  }

  reset() {
    this.ele.innerHTML = ""
    this.parse_states.length = 0;
    this.scan_states.length = 0;
  }

  write() {
    let data = "";

    for (const state of this.parse_states) {
      data += "<div class='parse-state-goto'>" + state.toString(16) + "</div>"
    }

    for (const state of this.scan_states) {
      data += "<div class='scan-state-goto'>" + state.toString(16) + "</div>"
    }

    this.ele.innerHTML = data;
  }
}

class ContextView {
  ele: HTMLDivElement
  fields: {
    type: "dbg" | "ctx" | "other", val: string, input: HTMLDivElement
  }[] = []

  constructor() {
    let ele = document.createElement("div");
    ele.append((<HTMLTemplateElement>document.querySelector("#context-view-template")).content.cloneNode(true));
    this.ele = <HTMLDivElement>ele.firstElementChild!;

    for (const ele of <HTMLDetailsElement[]>Array.from(this.ele.querySelectorAll(".context-view-field"))) {
      if (ele.dataset.ctx) {
        this.fields.push({
          type: "ctx",
          val: ele.dataset.ctx,
          input: <HTMLDivElement>ele.querySelector(".context-view-field-value")
        })
      } else if (ele.dataset.dbg) {
        this.fields.push({
          type: "dbg",
          val: ele.dataset.dbg,
          input: <HTMLDivElement>ele.querySelector(".context-view-field-value")
        })
      } else if (ele.dataset.other) {
        this.fields.push({
          type: "other",
          val: ele.dataset.other,
          input: <HTMLDivElement>ele.querySelector(".context-view-field-value")
        })
      }
    }
  }

  reset() {
    for (const field of this.fields) {
      if (field.type == "ctx") {
        //@ts-ignore
        field.input.innerHTML = "";
      } else if (field.type == "dbg") {
        //@ts-ignore
        field.input.innerHTML = "";
      } else if (field.type == "other") {
        field.input.innerHTML = "";
      }
    }
  }

  handle_debug_info(debug_info: radlr.JSDebugPacket, input: string) {
    let ctx = debug_info.ctx;
    for (const field of this.fields) {
      if (field.type == "ctx") {
        //@ts-ignore
        field.input.innerHTML = <string>ctx[field.val];
      } else if (field.type == "dbg") {
        //@ts-ignore
        field.input.innerHTML = <string>debug_info[field.val];
      } else if (field.type == "other") {
        switch (field.val) {
          case ("tok"): {
            let start = ctx.sym_ptr;
            let end = ctx.input_ptr;
            let str = input.slice(start, end);
            field.input.innerHTML = str;
          } break;
        }
      }
    }
  }
}

class BytecodeView {
  ele: HTMLDivElement
  instruction_area: HTMLDivElement
  inst_map: Map<number, number> = new Map
  instructions: HTMLDivElement[] = []
  active_instruction: number = -1;


  constructor() {
    let ele = document.createElement("div");
    ele.append((<HTMLTemplateElement>document.querySelector("#bytecode-view-template")).content.cloneNode(true));
    this.ele = <HTMLDivElement>ele.firstElementChild!;
    this.instruction_area = <HTMLDivElement>this.ele.querySelector(".bytecode-instructions")
  }

  diss(address: number, mnemonic: string): HTMLDivElement {
    let ele = document.createElement("div");
    ele.classList.add("bytecode-view-instruction")
    ele.innerHTML = `
    <div class=address>${address.toString(16)}</div>
    <div class=mnemonic>${mnemonic}</div>
    `;
    this.instruction_area.append(ele)
    this.instructions.push(ele)
    this.inst_map.set(address, this.instructions.length - 1);
    return ele
  }


  reset() {
    this.instruction_area.innerHTML = "";
    this.instructions.length = 0;
    this.inst_map.clear();
  }

  extra(label: string, value: any): HTMLElement {
    let ele = document.createElement("div");
    ele.classList.add("bytecode-view-info")
    ele.innerHTML = `
    <div class=label>${label}</div>
    <div class=value>${value}</div>
    `
    return ele;
  }

  mnemonic(label: string): HTMLElement {
    let ele = document.createElement("div");
    ele.classList.add("mnemonic")
    ele.innerHTML = label;
    return ele;
  }

  address(address: number, label: string = ""): HTMLElement {
    let ele = document.createElement("div");
    ele.classList.add("address-label")


    if (label) {
      ele.innerHTML = `
      <div class=label>${label}</div>
      <div class=address>${address.toString(16)}</div>

      `
    } else {
      ele.innerHTML = `
      <div class=address>${address.toString(16)}</div>
      `
    }

    ele.addEventListener("click", () => {
      console.log(address)
      this.show_address(address);
    })

    return ele;
  }

  mark_scanners(dv: StatefullDV) {
    let len = dv.dv.byteLength;
    outer: while (dv.off < len) {
      let instruction_address = dv.off;
      let instruction_byte = dv.u8();
      let string = "";
      switch (instruction_byte) {
        case radlr.Opcode.PushGoto: {
          let parse_mode = dv.u8();
          this.diss(instruction_address, "PUSH").append(
            this.address(dv.u32())
          )
        } break;
        case radlr.Opcode.Goto: {
          let parse_mode = dv.u8()
          this.diss(instruction_address, "GOTO").append(this.address(dv.u32()))
        } break;
        case radlr.Opcode.AssignToken: {
          this.diss(instruction_address, "TK_ID").append(
            this.extra("tok_id", dv.u32()),
          )
        } break;
        case radlr.Opcode.Reduce: {
          this.diss(instruction_address, "REDUCE").append(
            this.extra("nonterm", dv.u32()),
            this.extra("rule", dv.u32()),
            this.extra("sym count", dv.u16()),
          )
        } break;
        case radlr.Opcode.VectorBranch: {
          this.generate_table_string(dv, this.diss(instruction_address, "V_BR"));
        } break;
        case radlr.Opcode.HashBranch: {
          this.generate_table_string(dv, this.diss(instruction_address, "H_BR"));
        } break;
        case radlr.Opcode.ByteSequence: {
          let off = dv.off - 1;
          let len = dv.u16();
          let offset = dv.u32();
          let data = new Uint8Array(dv.dv.buffer, dv.off, len);
          let data_str = String.fromCodePoint(...data);

          let success_address = dv.off + len;

          this.diss(instruction_address, "BYTES").append(
            this.extra("match", data_str),
            this.address(success_address, "pass"),
            (offset > 0) ? this.address(off + offset, "fail") : this.extra("fail", 0),
          )
        } break;
        case radlr.Opcode.Fork: {
          this.diss(instruction_address, "FORK")
        } break;
        case radlr.Opcode.ReadCodepoint: {
          this.diss(instruction_address, "CP")
        } break;
        default: {
          break;
        }
      }
    }
  }

  create_debug_info(dv: StatefullDV) {
    let len = dv.dv.byteLength;
    outer: while (dv.off < len) {
      let instruction_address = dv.off;
      let instruction_byte = dv.u8();
      let string = "";
      switch (instruction_byte) {
        case radlr.Opcode.NoOp: {
          this.diss(instruction_address, "NOOP")
        } break;
        case radlr.Opcode.Pass: {
          this.diss(instruction_address, "PASS")
        } break;
        case radlr.Opcode.Fail: {
          this.diss(instruction_address, "FAIL")
        } break;
        case radlr.Opcode.ShiftChar: {
          this.diss(instruction_address, "SH_CH")
        } break;
        case radlr.Opcode.ShiftToken: {
          this.diss(instruction_address, "SH_TK")
        } break;
        case radlr.Opcode.PeekToken: {
          this.diss(instruction_address, "PK_TK")
        } break;
        case radlr.Opcode.PeekTokenScanless: {
          this.diss(instruction_address, "PK_TK_P")
        } break;
        case radlr.Opcode.SkipToken: {
          this.diss(instruction_address, "SK_TK")
        } break;
        case radlr.Opcode.SkipTokenScanless: {
          this.diss(instruction_address, "SK_TK_P")
        } break;
        case radlr.Opcode.PeekSkipToken: {
          this.diss(instruction_address, "PK_SK_TK")
        } break;
        case radlr.Opcode.PeekReset: {
          this.diss(instruction_address, "RST")
        } break;
        case radlr.Opcode.Accept: {
          this.diss(instruction_address, "ACPT")
        } break;
        case radlr.Opcode.PopGoto: {
          this.diss(instruction_address, "POP")
        } break;
        case radlr.Opcode.PushGoto: {
          let parse_mode = dv.u8();
          this.diss(instruction_address, "PUSH").append(
            this.address(dv.u32())
          )
        } break;
        case radlr.Opcode.Goto: {
          let parse_mode = dv.u8()
          this.diss(instruction_address, "GOTO").append(this.address(dv.u32()))
        } break;
        case radlr.Opcode.AssignToken: {
          this.diss(instruction_address, "TK_ID").append(
            this.extra("tok_id", dv.u32()),
          )
        } break;
        case radlr.Opcode.Reduce: {
          this.diss(instruction_address, "REDUCE").append(
            this.extra("nonterm", dv.u32()),
            this.extra("rule", dv.u32()),
            this.extra("sym count", dv.u16()),
          )
        } break;
        case radlr.Opcode.VectorBranch: {
          this.generate_table_string(dv, this.diss(instruction_address, "V_BR"));
        } break;
        case radlr.Opcode.HashBranch: {
          this.generate_table_string(dv, this.diss(instruction_address, "H_BR"));
        } break;
        case radlr.Opcode.ByteSequence: {
          let off = dv.off - 1;
          let len = dv.u16();
          let offset = dv.u32();
          let data = new Uint8Array(dv.dv.buffer, dv.off, len);
          let data_str = String.fromCodePoint(...data);

          let success_address = dv.off + len;

          this.diss(instruction_address, "BYTES").append(
            this.extra("match", data_str),
            this.address(success_address, "pass"),
            (offset > 0) ? this.address(off + offset, "fail") : this.extra("fail", 0),
          )
        } break;
        case radlr.Opcode.Fork: {
          this.diss(instruction_address, "FORK")
        } break;
        case radlr.Opcode.ReadCodepoint: {
          this.diss(instruction_address, "CP")
        } break;
        default: {
          break;
        }
      }
    }
  }

  generate_table_string(dv: StatefullDV, instruction_ele: HTMLDivElement) {
    let out_string = "";

    let { table_base_address, input_type, scan_address, table_length, table_start_iter, default_address } = getHashData(dv);

    let val = "tok";
    let convert_val_to_string = (val: number): string => val.toString();
    let convert_codepoint = (val: number): string => `\"${String.fromCodePoint(val)}\"`;

    switch (input_type) {
      case radlr.MatchInputType.NonTerminal: {
        val = "NTERM"
      } break;
      case radlr.MatchInputType.Token: {
      } break;
      case radlr.MatchInputType.Class: {
        val = "CLASS"
      } break;
      case radlr.MatchInputType.Codepoint: {
        val = "CP"
        convert_val_to_string = convert_codepoint;
      } break;
      case radlr.MatchInputType.Byte: {
        val = "BYTE"
        convert_val_to_string = convert_codepoint;
      } break;
      case radlr.MatchInputType.EndOfFile: {
        val = "EOF"
      } break;
      case radlr.MatchInputType.ByteScanless: {
        val = "byte"
        convert_val_to_string = convert_codepoint;
      } break;
      case radlr.MatchInputType.CodepointScanless: {
        val = "cp"
        convert_val_to_string = convert_codepoint;
      } break;
      case radlr.MatchInputType.ClassScanless: {
        val = "cp_class"
      } break;
      case radlr.MatchInputType.ByteSequence: {
        throw "HUH?";
      } break;
      case radlr.MatchInputType.CSTNode: {
        throw "HUH?";
      } break;
    }

    instruction_ele.append(this.mnemonic(val));

    out_string += `\nmatch ${val}:`;


    for (let i = 0; i < table_length; i++) {
      let entry = table_start_iter.u32();

      let val_id = (entry & 0x7FF);
      let address_offset = ((entry >> 11) & 0x7FF);
      //let meta = ((entry >> 22) & 0x3FF) - 512;
      let address = table_base_address + address_offset;

      instruction_ele.append(this.address(address, convert_val_to_string(val_id)))
    }

    if (default_address < dv.dv.byteLength) {
      instruction_ele.append(this.address(default_address, "default"))
    }
  }

  handle_debug_info(dv: StatefullDV, debug_info: radlr.JSDebugPacket, input: string) {
    let instruction = dv.off;

    let index = this.inst_map.get(instruction);
    if (index) {
      if (this.active_instruction >= 0) {
        this.instructions[this.active_instruction].classList.remove("active")
      }

      this.active_instruction = index

      let ele = this.instructions[index];
      ele.classList.add("active")

      this.show_address(instruction);


    }
  }

  private show_address(address: number) {
    let index = this.inst_map.get(address);
    if (index) {

      let ele = this.instructions[index];

      let height = this.instruction_area.getBoundingClientRect().height;
      let top = this.instruction_area.scrollTop;

      let ele_top = ele.offsetTop;

      if (ele_top - top > height || ele_top - top < 0) {
        ele.scrollIntoView();
      }
    }
  }
}

export function init(
  parser_info_field: NBContentField,
  parser_input_field: NBEditorField,
  grammar_pipeline_node: pipeline.GrammarDBNode,
  parser_player_node: pipeline.ParserPlayerNode,
  controls: Controls
) {
  let db: radlr.JSBytecodeParserDB | null = null;
  let bytecode: StatefullDV | null = null;

  let states_view = new StatesView();
  let instruction_view = new ContextView();
  let bytecode_view = new BytecodeView();


  parser_info_field.body.appendChild(instruction_view.ele);
  parser_info_field.body.appendChild(states_view.ele);
  parser_info_field.body.appendChild(bytecode_view.ele);
  parser_info_field.body.id = "parser-info"

  grammar_pipeline_node.addListener("loading", _ => {
    parser_info_field.set_loading(true);
    states_view.reset();
    instruction_view.reset();
    bytecode_view.reset();
  })

  grammar_pipeline_node.addListener("bytecode_db", async new_db => {
    parser_info_field.set_loading(false);
    db = new_db;

    bytecode = new StatefullDV(new DataView(db.bytecode.buffer), 0);
    bytecode_view.create_debug_info(bytecode.to());
    states_view.reset();
  })

  parser_player_node.addListener("execute_state", debug_info => {
    instruction_view.handle_debug_info(debug_info, parser_player_node.input);
    if (bytecode) {
      bytecode_view.handle_debug_info(bytecode.to(debug_info.instruction), debug_info, parser_player_node.input);
    }
  })

  parser_player_node.addListener("execute_instruction", debug_info => {
    if (bytecode) {
      states_view.handleInstruction(bytecode.to(debug_info.instruction), debug_info.is_scanner)
      bytecode_view.handle_debug_info(bytecode.to(debug_info.instruction), debug_info, parser_player_node.input);
    }
    instruction_view.handle_debug_info(debug_info, parser_player_node.input);
  })

  parser_player_node.addListener("execute_instruction", debug_info => {
    let ctx = debug_info.ctx;

    parser_input_field.remove_character_classes();
    parser_input_field.add_character_class(ctx.input_ptr, ctx.input_ptr + 1, "dbg-input-pos");
    parser_input_field.add_character_class(ctx.anchor_ptr, ctx.anchor_ptr + 1, "dbg-anchor-pos");
    parser_input_field.add_character_class(ctx.begin_ptr, ctx.begin_ptr + 1, "dbg-begin-pos");
    parser_input_field.add_character_class(ctx.end_ptr, ctx.end_ptr + 1, "dbg-end-pos");

    if (debug_info.is_scanner) {
      parser_input_field.add_character_class(ctx.sym_ptr, ctx.sym_ptr + 1, "dbg-sym-pos");
      parser_input_field.add_character_class(ctx.sym_ptr, ctx.input_ptr, "dbg-sym");
    } else if (ctx.sym_len > 0) {
      parser_input_field.add_character_class(ctx.sym_ptr, ctx.sym_ptr + ctx.sym_len, "dbg-sym");
    }
  });


  controls.addListener("reset", () => {
    parser_input_field.remove_character_classes();
    parser_player_node.reset();
    states_view.reset();
  });
}

export function disassemble_bytecode(parser_db: radlr.JSBytecodeParserDB): StatesLU {
  let bytecode = parser_db.bytecode;

  let dv = new DataView(bytecode.buffer);
  let off = 8;

  let states: StatesLU = new Map;

  ProcessState(new StatefullDV(dv, off), states);

  return states
}

function ProcessState(dv: StatefullDV, states: StatesLU, is_scanner: boolean = false) {
  let state_address = dv.off;
  if (states.has(state_address)) {
    return;
  }

  let data = { pseudo_code: "" };
  states.set(state_address, data);

  let gotos: string[] = [];

  let pseudo_code = "";
  pseudo_code += `state@${dv.off.toString(16)}${is_scanner ? " SCANNER " : ""}(lex, ctx) {`;
  pseudo_code += merge_lines(process_instructions(dv, gotos, states, data).split("\n",));
  pseudo_code += merge_lines(gotos);
  pseudo_code += "\n}";

  data.pseudo_code = pseudo_code;
}

function merge_lines(internal_data: string[], prefix: string = "\n  ") {
  let pseudo_code: string = "";
  for (const goto of internal_data) {
    if (!goto) continue
    pseudo_code += prefix + goto;
  }
  return pseudo_code;
}

function process_instructions(dv: StatefullDV, gotos: string[], states: StatesLU, current_state: object, root_name: string = ""): string {
  let pseudo_code: string = ""
  let i = 0;
  let have_root = false

  outer: while (i++ < 10) {
    let instruction_address = dv.off;
    let instruction_byte = dv.u8();
    let string = "";
    switch (instruction_byte) {
      case radlr.Opcode.NoOp: {
      } break;
      case radlr.Opcode.Pass: {
        gotos.push("return")
      } break outer;
      case radlr.Opcode.Fail: {
        string += `\nthrow "could not continue"`;
      } break outer;
      case radlr.Opcode.ShiftChar: {
        string += "\nctx.emit(SHIFT_CHAR)";
        string += "\nlex.shift_la(1)";
      } break;
      case radlr.Opcode.ShiftToken: {
        string += "\nlex.shift(ctx.sym_len)";
        string += "\nctx.emit(SHIFT_TOKEN { id: ctx.tk_id, len: ctx.sym_len })";
        string += "\nctx.sym_len = 0";
      } break;
      case radlr.Opcode.PeekToken: {
        string += "\nctx.peek(tok)";
      } break;
      case radlr.Opcode.PeekTokenScanless: {
        string += "\nctx.tok_len = ctx.sym_len\nctx.peek(tok)";
      } break;
      case radlr.Opcode.SkipToken: {
        string += "\nctx.skip()\ngoto " + root_name;
      } break outer;
      case radlr.Opcode.SkipTokenScanless: {
        string += "\nctx.skip()\ngoto " + root_name;
      } break outer;
      case radlr.Opcode.PeekSkipToken: {
        string += "\nctx.peek_skip()\ngoto " + root_name;
      } break outer;
      case radlr.Opcode.PeekReset: {
        string += "\nctx.peek_reset()";
      } break;
      case radlr.Opcode.Accept: {
        string += "\nctx.emit(ACCEPT)";
      } break outer;
      case radlr.Opcode.PopGoto: {
        string += "\nctx.pop(1)";
      } break;
      case radlr.Opcode.PushGoto: {
        let parse_mode = dv.u8();
        let address = dv.u32();
        gotos.unshift(`state@${address.toString(16)}(lex, ctx)`);
        ProcessState(dv.to(address), states);
      } break;
      case radlr.Opcode.Goto: {
        let parse_mode = dv.u8();
        let addressw = dv.u32();
        string += `\nstate@${addressw.toString(16)}(lex, ctx)`;
        ProcessState(dv.to(addressw), states);
      } break;
      case radlr.Opcode.AssignToken: {
        let tok_id = dv.u32();
        string += `\nctx.tok_id = ${tok_id}`;
      } break;
      case radlr.Opcode.Reduce: {
        let nterm = dv.u32();
        let rule_id = dv.u32();
        let symbol_count = dv.u16();

        string += `\nctx.nt_id = ${nterm}`;
        string += `\nctx.emit(REDUCE{ nt_id: ${nterm}, rule: ${rule_id}, sym_count: ${symbol_count} })`;
      } break;
      case radlr.Opcode.VectorBranch: {
        throw "Vector branch not implemented"
        string += generate_table_string(dv, states, current_state);
      } break;
      case radlr.Opcode.HashBranch: {
        string += generate_table_string(dv, states, current_state);
      } break outer;
      case radlr.Opcode.ByteSequence: {
        let off = dv.off - 1;
        let len = dv.u16();
        let offset = dv.u32();
        let data = new Uint8Array(dv.dv.buffer, dv.off, len);
        let data_str = String.fromCodePoint(...data);

        let success_address = dv.off + len;

        string += `if lex.slice(${len}) == "${data_str}":`
        string += `\n  lex.incr(${len})`

        let gotos: string[] = [];
        string += merge_lines(process_instructions(dv.to(success_address), gotos, states, current_state).split("\n"));
        string += merge_lines(gotos);
        string += "\nelse"

        if (offset > 0) {
          let fail_address = off + offset;
          let gotos: string[] = [];
          string += merge_lines(process_instructions(dv.to(fail_address), gotos, states, current_state).split("\n"));
          string += merge_lines(gotos);
        } else {
          string += `\n  throw \"Lexer does not have sequence '${data_str}' at current offset\"`;
        }
      } break outer;
      case radlr.Opcode.Fork: {
        console.log("FORK")
      } break;
      case radlr.Opcode.ReadCodepoint: {
      } break;
      default: {
        break outer;
      }
    }

    states.set(instruction_address, { pseudo_code: string });

    pseudo_code += string;
  }
  return pseudo_code;
}

function buildPseudoCode() { }

function generate_table_string(dv: StatefullDV, states: StatesLU, current_state: object): string {
  let out_string = "";
  let { table_base_address, input_type, scan_address, table_length, table_start_iter, default_address } = getHashData(dv);

  let val = "tok";
  let error = "unrecognized symbol";
  let convert_val_to_string = (val: number): string => val.toString();
  let convert_codepoint = (val: number): string => `\"${String.fromCodePoint(val)}\"`;

  let root_name = `\`root_${table_base_address}`

  switch (input_type) {
    case radlr.MatchInputType.NonTerminal: {
      val = "ctx.nt_id"
      error = "incorrect non-terminal was produced"
    } break;
    case radlr.MatchInputType.Token: {
      if (scan_address < 0xFFFF_FFFF) {
        out_string += `\nstate@${scan_address.toString(16)}(lex, ctx)`;
        ProcessState(dv.to(scan_address), states, true);
      }
      out_string += `\ntok = ctx.tok_id`
    } break;
    case radlr.MatchInputType.Class: {
      val = "cp_class"
      out_string += `\ncp_class = lex.codepoint_class()`
    } break;
    case radlr.MatchInputType.Codepoint: {
      val = "cp"
      out_string += `\ncp = lex.codepoint()`
      convert_val_to_string = convert_codepoint;
    } break;
    case radlr.MatchInputType.Byte: {
      val = "byte"
      out_string += `\nbyte = lex.byte()`
      convert_val_to_string = convert_codepoint;
    } break;
    case radlr.MatchInputType.EndOfFile: {
      val = "eof"
      out_string += `\neof = lex.is_eof()`
    } break;
    case radlr.MatchInputType.ByteScanless: {
      val = "byte"
      out_string += `\nbyte = lex.byte()`
      out_string += `\nctx.sym_len = 1`
      convert_val_to_string = convert_codepoint;
    } break;
    case radlr.MatchInputType.CodepointScanless: {
      val = "cp"
      out_string += `\ncp = lex.codepoint()`
      out_string += `\nctx.sym_len = cp.length`
      convert_val_to_string = convert_codepoint;
    } break;
    case radlr.MatchInputType.ClassScanless: {
      val = "cp_class"
      out_string += `\ncp_class = lex.codepoint_class()`
      out_string += `\nctx.sym_len = lex.codepoint().length`
    } break;
    case radlr.MatchInputType.ByteSequence: {
      throw "HUH?";
    } break;
    case radlr.MatchInputType.CSTNode: {
      throw "HUH?";
    } break;
  }

  out_string += `\nmatch ${val}:`;

  let inlined_default = false;

  let map: Map<number, number[]> = new Map();

  for (let i = 0; i < table_length; i++) {
    let entry = table_start_iter.u32();

    let val_id = (entry & 0x7FF);
    let address_offset = ((entry >> 11) & 0x7FF);
    //let meta = ((entry >> 22) & 0x3FF) - 512;
    let address = table_base_address + address_offset;

    if (!map.get(address)) {
      map.set(address, []);
    }

    let entries = map.get(address)!;
    entries.push(val_id)
  }

  for (const [address, entries] of map.entries()) {
    if (address == default_address) {
      out_string += `\n  default:`;
      inlined_default = true;
    } else {
      for (const entry of entries) {
        out_string += `\n  case ${convert_val_to_string(entry)}:`;
      }
    }

    let gotos: string[] = [];
    out_string += merge_lines(process_instructions(dv.to(address), gotos, states, current_state, root_name).split("\n"), "\n    ");
    out_string += merge_lines(gotos, "\n    ");
  }

  if (!inlined_default) {
    if (default_address < dv.dv.byteLength) {
      out_string += `\n  default:`;
      let gotos: string[] = [];
      out_string += merge_lines(process_instructions(dv.to(default_address), gotos, states, current_state, root_name).split("\n"), "\n    ");
      out_string += merge_lines(gotos, "\n    ");
    } else {

      out_string += `\n  default:\n    throw "${error}"`;
    }

  }

  states.set(table_base_address, {
    pseudo_code: out_string
  });

  return out_string
}

class StatefullDV {
  dv: DataView
  off: number = 0

  constructor(dv: DataView, offset: number) {
    this.off = offset
    this.dv = dv
  }

  u32(): number {
    let off = this.off;
    this.off += 4;
    return this.dv.getUint32(off, true)
  }

  u16(): number {
    let off = this.off;
    this.off += 2;
    return this.dv.getUint16(off, true)
  }

  u8(): number {
    let off = this.off;
    this.off += 1;
    return this.dv.getUint8(off)
  }

  to(offset: number = this.off): StatefullDV {
    return new StatefullDV(this.dv, offset)
  }
}


export class ParserView {
  field: NBContentField;
  states: StatesLU | null = null;
  debugger_data: HTMLDivElement;
  parser_info: HTMLDivElement;

  constructor(field: NBContentField) {
    if (field instanceof NBEditorField) {
      throw "ParserView cannot bind to a NBEditorField";
    }

    this.debugger_data = document.createElement("div");
    this.parser_info = document.createElement("div");

    this.field = field;
    this.field.body.classList.add("debugger-cst-output");
    this.field.body.append(this.parser_info);
    this.field.body.append(this.debugger_data);
  }

  init() {
    this.field.set_content_visible(false);
    this.field.set_loading(true);
  }

  set_active_state(address: number) {
    let val = this.states!.get(address);
    if (val) {

      this.debugger_data.innerHTML = `<pre>${val.pseudo_code}</pre>`
    }
  }

  handle_new_parser(parser_db: radlr.JSBytecodeParserDB) {
    this.field.set_content_visible(true);
    this.field.set_loading(false);

    this.states = disassemble_bytecode(parser_db);
  }

  setContextData(step: radlr.JSDebugPacket) {
    this.parser_info.innerHTML = JSON.stringify({
      instruction: step.instruction,
      is_scanner: step.is_scanner,
      ctx: {
        anchor_ptr: step.ctx.anchor_ptr,
        begin_ptr: step.ctx.begin_ptr,
        end_ptr: step.ctx.end_ptr,
        input_ptr: step.ctx.input_ptr,
        is_scanner: step.ctx.is_scanner,
        sym_len: step.ctx.sym_len,
        sym_ptr: step.ctx.sym_ptr,
        tok_id: step.ctx.tok_id,
        tok_len: step.ctx.tok_len,
      }
    }, undefined, "\n")
  }
}

function getHashData(dv: StatefullDV) {
  let table_base_address = dv.off - 1;
  let input_type = dv.u8();
  let default_delta = dv.u32();
  let scan_address = dv.u32();
  let table_length = dv.u32();
  let table_meta = dv.u32();
  let table_start = table_base_address + 18;
  let table_start_iter = dv.to(table_start);
  let default_address = table_base_address + default_delta;
  return { table_base_address, input_type, scan_address, table_length, table_start_iter, default_address };
}
