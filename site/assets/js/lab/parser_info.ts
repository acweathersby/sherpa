import * as radlr from "js/radlr/radlr_wasm.js";
import * as pipeline from "./pipeline";
import { NBContentField, NBEditorField } from "./notebook";

type StatesLU = Map<number, { pseudo_code: string }>;

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
  pseudo_code += merge_lines(process_instructions(dv, gotos, states).split("\n"));
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

function process_instructions(dv: StatefullDV, gotos: string[], states: StatesLU, root_name: string = ""): string {
  let pseudo_code: string = ""
  let i = 0;
  let have_root = false

  outer: while (i++ < 10) {
    let instruction_byte = dv.u8();
    switch (instruction_byte) {
      case radlr.Opcode.NoOp: {
      } break;
      case radlr.Opcode.Pass: {
        gotos.push("return")
      } break outer;
      case radlr.Opcode.Fail: {
        pseudo_code += `\nthrow "could not continue"`;
      } break outer;
      case radlr.Opcode.ShiftChar: {
        pseudo_code += "\nctx.emit(SHIFT_CHAR)";
        pseudo_code += "\nlex.shift_la(1)";
      } break;
      case radlr.Opcode.ShiftToken: {
        pseudo_code += "\nlex.shift(ctx.sym_len)";
        pseudo_code += "\nctx.emit(SHIFT_TOKEN { id: ctx.tk_id, len: ctx.sym_len })";
        pseudo_code += "\nctx.sym_len = 0";
      } break;
      case radlr.Opcode.PeekToken: {
        pseudo_code += "\nctx.peek(tok)";
      } break;
      case radlr.Opcode.PeekTokenScanless: {
        pseudo_code += "\nctx.tok_len = ctx.sym_len\nctx.peek(tok)";
      } break;
      case radlr.Opcode.SkipToken: {

        pseudo_code += "\nctx.skip()\ngoto " + root_name;
      } break outer;
      case radlr.Opcode.SkipTokenScanless: {

        pseudo_code += "\nctx.skip()\ngoto " + root_name;
      } break outer;
      case radlr.Opcode.PeekSkipToken: {
        pseudo_code += "\nctx.peek_skip()\ngoto " + root_name;
      } break outer;
      case radlr.Opcode.PeekReset: {
        pseudo_code += "\nctx.peek_reset()";
      } break;
      case radlr.Opcode.Accept: {
        pseudo_code += "\nctx.emit(ACCEPT)";
      } break outer;
      case radlr.Opcode.PopGoto: {
        pseudo_code += "\nctx.pop(1)";
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
        pseudo_code += `\nstate@${addressw.toString(16)}(lex, ctx)`;
        ProcessState(dv.to(addressw), states);
      } break;
      case radlr.Opcode.AssignToken: {
        let tok_id = dv.u32();
        pseudo_code += `\nctx.tok_id = ${tok_id}`;
      } break;
      case radlr.Opcode.Reduce: {
        let nterm = dv.u32();
        let rule_id = dv.u32();
        let symbol_count = dv.u16();

        pseudo_code += `\nctx.nt_id = ${nterm}`;
        pseudo_code += `\nctx.emit(REDUCE{ nt_id: ${nterm}, rule: ${rule_id}, sym_count: ${symbol_count} })`;
      } break;
      case radlr.Opcode.VectorBranch: {
        throw "Vector branch not implemented"
        pseudo_code += generate_table_string(dv, states);
      } break;
      case radlr.Opcode.HashBranch: {
        pseudo_code += generate_table_string(dv, states);
      } break outer;
      case radlr.Opcode.ByteSequence: {
        let off = dv.off - 1;
        let len = dv.u16();
        let offset = dv.u32();
        let data = new Uint8Array(dv.dv.buffer, dv.off, len);
        let data_str = String.fromCodePoint(...data);

        let success_address = dv.off + len;

        pseudo_code += `if lex.slice(${len}) == "${data_str}":`
        pseudo_code += `\n  lex.incr(${len})`

        let gotos: string[] = [];
        pseudo_code += merge_lines(process_instructions(dv.to(success_address), gotos, states).split("\n"));
        pseudo_code += merge_lines(gotos);
        pseudo_code += "\nelse"

        if (offset > 0) {
          let fail_address = off + offset;
          let gotos: string[] = [];
          pseudo_code += merge_lines(process_instructions(dv.to(fail_address), gotos, states).split("\n"));
          pseudo_code += merge_lines(gotos);
        } else {
          pseudo_code += `\n  throw \"Lexer does not have sequence '${data_str}' at current offset\"`;
        }
      } break outer;
      case radlr.Opcode.Fork: {
      } break;
      case radlr.Opcode.ReadCodepoint: {
      } break;
      default: {
        break outer;
      }
    }
  }
  return pseudo_code;
}

function buildPseudoCode() { }

function generate_table_string(dv: StatefullDV, states: StatesLU): string {
  let out_string = "";
  let table_base_address = dv.off - 1;
  let input_type = dv.u8();
  let default_delta = dv.u32();
  let scan_address = dv.u32();
  let table_length = dv.u32();
  let table_meta = dv.u32();
  let table_start = table_base_address + 18;
  let table_start_iter = dv.to(table_start);
  let default_address = table_base_address + default_delta;

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
    out_string += merge_lines(process_instructions(dv.to(address), gotos, states, root_name).split("\n"), "\n    ");
    out_string += merge_lines(gotos, "\n    ");
  }

  if (!inlined_default) {
    if (default_address < dv.dv.byteLength) {
      out_string += `\n  default:`;
      let gotos: string[] = [];
      out_string += merge_lines(process_instructions(dv.to(default_address), gotos, states, root_name).split("\n"), "\n    ");
      out_string += merge_lines(gotos, "\n    ");
    } else {

      out_string += `\n  default:\n    throw "${error}"`;
    }

  }
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

  to(offset: number): StatefullDV {
    return new StatefullDV(this.dv, offset)
  }
}


export class ParserView {
  field: NBContentField;
  states: StatesLU | null = null;

  constructor(field: NBContentField) {
    if (field instanceof NBEditorField) {
      throw "ParserView cannot bind to a NBEditorField";
    }

    this.field = field;

    this.field.body.classList.add("debugger-cst-output");
  }

  init() {
    this.field.set_content_visible(false);
    this.field.set_loading(true);
  }

  set_active_state(address: number) {
    let val = this.states!.get(address);
    if (val) {

      this.field.body.innerHTML = `<pre>${val.pseudo_code}</pre>`
    }
  }

  handle_new_parser(parser_db: radlr.JSBytecodeParserDB) {
    this.field.set_content_visible(true);
    this.field.set_loading(false);

    this.states = disassemble_bytecode(parser_db);
  }
}