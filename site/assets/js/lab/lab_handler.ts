import { basicSetup } from "codemirror";
import { Container, Thing, CMThing } from "../common/layout"
import { DebuggerButton, DebuggerCheckbox, DebuggerField } from "./debugger/debugger_io";
import { get_grammar } from "js/common/session_storage";
import init, * as radlr from "js/radlr/radlr_wasm.js";
import { CSTNodeRoot } from "./ast_editor/node";
import { GrammarContext } from "./grammar_context";
import { initDebugger } from "./debugger/debugger";
import { DividerHandler } from ".";
import { DividerHandle } from "js/controls/divider";
import { debugger_bus } from "./debugger/debugger_bus";
import { JSDebugEvent } from "js/radlr/radlr_wasm";
import { CSTNode } from "./debugger/cst";
import { ast_parser } from "./radlr_ts/parser_kernel";
import { BINARY } from "./radlr_ts/parser"
import { reduce_rules } from "./radlr_ts/ast";

export async function lab_handler(div: HTMLDivElement) {
  await init();


  let root_ctr = new Container("root");
  root_ctr.direction = "horizontal"
  root_ctr.attach(div);

  let inputs = new Container("inputs");
  let outputs = new Container("outputs");

  inputs.ele.classList.add("adjustable");
  outputs.ele.classList.add("adjustable");
  inputs.attach(root_ctr);
  outputs.attach(root_ctr);

  let grammar_editor = new CMThing("grammar-editor", "radlr grammar", get_grammar());

  grammar_editor.attach(inputs);

  let parse_input_editor = new CMThing("div", "parser input", "");
  parse_input_editor.attach(inputs);

  let debugger_ = new Thing("div", "debugger", "DEBUGGER", false);
  let debugger_template = (<HTMLTemplateElement>document.querySelector("#debugger-template")).content.cloneNode(true);

  debugger_.fixed_size = true;
  debugger_.content.append(debugger_template);
  debugger_.attach(outputs);


  createParserMetrics(outputs);
  createIrView(outputs);
  createDisassemblyView(outputs);
  createSyntaxHighlight(outputs, parse_input_editor);
  createCSTView(outputs, parse_input_editor, grammar_editor);
  updateTrackers(parse_input_editor, grammar_editor);

  DebuggerButton.gatherButtons(debugger_.ele);
  DebuggerCheckbox.gatherCheckBoxes(debugger_.ele);
  DebuggerField.gatherFields(debugger_.ele);

  const ctx = new GrammarContext();

  let config = radlr.JSParserConfig.cst_editor();

  grammar_editor.onUpdate((data: any) => {
    ctx.addGrammar(data, "/", config)
  });

  initDebugger(ctx, parse_input_editor, document.createElement("select"));
}

function createParserMetrics(outputs: Container) {
  let parser_metrics = new Thing("div", "parser-metrics", "DEBUGGER", false);

  debugger_bus.addListener("error", e => {
    parser_metrics.content.innerHTML = e.msg;
  })

  debugger_bus.addListener("clear-error", e => {
    parser_metrics.content.innerHTML = "";
  })

  debugger_bus.addListener("states-created", m => {
    parser_metrics.content.innerHTML = `
      ${m.states.classification.get_type()}
    `;
  })

  parser_metrics.fixed_size = true;
  parser_metrics.attach(outputs);
}


function updateTrackers(input_view: CMThing, grammar_view: CMThing) {
  debugger_bus.addListener("step", ({
    db,
    bc_db,
    debug_data,
    states,
    input,
    key_frame
  }) => {
    if (!key_frame) return;

    let end_ptr = debug_data.ctx.end_ptr;
    let input_ptr = debug_data.ctx.input_ptr;
    let sym_ptr = debug_data.ctx.sym_ptr;
    let begin_ptr = debug_data.ctx.begin_ptr;
    let anchor_ptr = debug_data.ctx.anchor_ptr;
    let tok_len = Math.max(debug_data.ctx.tok_len, 1);



    input_view.clearHighlightClass("pos-test", "skip-test", "scan-test", "tok-test");

    input_view.addHighlight(sym_ptr, 1, "pos-test");

    if (sym_ptr > anchor_ptr) {
      input_view.addHighlight(anchor_ptr, Math.max(1, (sym_ptr - anchor_ptr)), "skip-test");
    }

    if (input_ptr > sym_ptr) {
      input_view.addHighlight(sym_ptr, Math.max(1, (input_ptr - sym_ptr)), "scan-test");
    }

    if (tok_len > 0) {
      input_view.addHighlight(sym_ptr, tok_len, "tok-test");
    }


  });
}




function createCSTView(ast_cst_container: Container, input_view: CMThing, grammar_view: CMThing) {
  let cst_view = new Thing("div", "debugger-cst-output", "concrete syntax", false);
  cst_view.vertical_scroll();
  let cst_nodes: any[] = [];

  function render_cst() {
    let ele = cst_view.content;
    if (ele) {
      ele.innerHTML = "";
      for (const node of cst_nodes) {
        ele.appendChild(node.toDOM());
      }
    }
  }


  debugger_bus.addListener("parser-reset", (s) => {
    let ele = cst_view.content;
    if (ele) {
      ele.innerHTML = "";
    }

    cst_nodes.length = 0;
  });

  debugger_bus.addListener("step", ({
    db,
    bc_db,
    debug_data,
    states,
    input,
    key_frame
  }) => {
    if (debug_data.event == JSDebugEvent.Shift) {
      let { offset_start, offset_end } = debug_data;
      cst_nodes.push(new CSTNode(input.slice(offset_start, offset_end), "", true, offset_start, offset_end - offset_start, input_view));
      render_cst();
    } else if (debug_data.event == JSDebugEvent.Reduce) {
      let { nonterminal_id, rule_id, symbol_count } = debug_data;
      var [_, ...expr] = radlr.get_rule_expression_string(rule_id, db).split(">");

      //@ts-ignore
      expr = "> " + expr.join("");

      let [start, end] = radlr.get_rule_location(rule_id, db);


      let name = radlr.get_nonterminal_name_from_id(nonterminal_id, db);


      let children = cst_nodes.slice(-symbol_count);

      let off_start = children[0].offset;
      let length = children[children.length - 1].offset + children[children.length - 1].length - off_start;

      let node = new CSTNode(name, expr, false, off_start, length, input_view, grammar_view, start, end - start);

      node.children = cst_nodes.slice(-symbol_count);


      cst_nodes.length -= symbol_count;
      cst_nodes.push(node);
    }

    if (key_frame) {
      render_cst();
    }
  });

  cst_view.attach(ast_cst_container);
}

function createSyntaxHighlight(ast_cst_container: Container, input_view: CMThing) {
  let cst_view = new Thing("div", "syntax-highlighter", "syntax highlight", false);
  cst_view.vertical_scroll();
  let cst_nodes: any[] = [];
  let names: string[] = [];

  /// Maps a non-term id to highlighting operations that should be applied to all or select symbols of that rule.
  let lookup = new Map;

  let form = document.createElement("form");

  let name = document.createElement("input");
  name.name = "name"
  name.type = "text";

  let color = document.createElement("input");
  color.name = "color"
  color.type = "color";

  let enter = document.createElement("button");
  enter.innerHTML = "add"

  form.append(name, color, enter)

  cst_view.ele.prepend(form);

  form.addEventListener("submit", e => {
    let color_val = color.value;
    let name_val = name.value;

    try {
      let val = ast_parser(
        BINARY, name_val, ["", 8], reduce_rules
      );

      for (const v of val) {
        let name = v.name;
        lookup.set(name, { v, color_val });
      }
      console.log(val)
    } catch (e) {
      console.log(e)
    }

    e.preventDefault();
    e.stopImmediatePropagation();
    e.stopPropagation();

    return false;
  })

  let parser_db: radlr.JSParserDB | null = null;

  debugger_bus.addListener("db-created", (s) => {
    parser_db = s.db;
    if (parser_db) {
      names = radlr.get_nonterminal_names_from_db(parser_db);

      cst_view.content.innerHTML = ""
      let i = 0;
      for (const name of names) {
        cst_view.content.innerHTML += `<p>${name} - {${lookup.get(name)}}</p>`
      }
    }
  });

  debugger_bus.addListener("parser-reset", (s) => {
    input_view.clearHighlightClass("syntax-highlight");
    cst_nodes.length = 0;
  });

  debugger_bus.addListener("step", ({
    debug_data,
  }) => {
    if (debug_data.event == JSDebugEvent.Shift) {
      let { offset_start, offset_end, } = debug_data;
      cst_nodes.push({ offset_start, offset_end, is_nonterm: false, first_index: cst_nodes.length });
    } else if (debug_data.event == JSDebugEvent.Reduce) {

      let { nonterminal_id, rule_id, symbol_count } = debug_data;

      let last_symbol = cst_nodes[cst_nodes.length - 1];
      let first_symbol = last_symbol;
      let symbols = [];
      let count = 0;

      while (true) {
        if (first_symbol.is_nonterm) {
          first_symbol = cst_nodes[first_symbol.first_index];
        } else {
          symbols.push(first_symbol);
          if (++count >= symbol_count) {
            break
          } else {
            first_symbol = cst_nodes[first_symbol.first_index - 1];
          }
        }
      }

      symbols.reverse()

      let offset_end = last_symbol.offset_end;
      let offset_start = first_symbol.offset_start;

      cst_nodes.push({ offset_start, offset_end, is_nonterm: true, id: nonterminal_id, symbols: symbol_count, first_index: first_symbol.first_index });

      let col_val = lookup.get(names[nonterminal_id]);

      if (col_val) {
        let { v, color_val } = col_val;
        let node = cst_nodes[cst_nodes.length - 1];

        if (v.sym) {
          for (const sym of v.sym) {
            let child = symbols[sym >= 0 ? (sym % symbols.length) : Math.abs((symbols.length + sym) % symbols.length)];

            if (child) {
              let start = child.offset_start;
              let end = child.offset_end;

              // find the first terminal 
              input_view.addHighlight(start, end - start, "syntax-highlight", { style: "color:" + color_val });
            }
          }
        } else {
          let start = node.offset_start;
          let end = node.offset_end;

          // find the first terminal 
          input_view.addHighlight(start, end - start, "syntax-highlight", { style: "color:" + color_val });
        }
      }
    }
  });

  cst_view.attach(ast_cst_container);
}


function createIrView(debugger_state: Container) {
  let ir_state = new Thing("div", "debugger-ir-state", "active ir states", false);
  ir_state.vertical_scroll();

  let parser: radlr.JSByteCodeParser | null = null;
  let bc_db: radlr.JSBytecodeParserDB | null = null;
  let states: radlr.JSIRParser | null = null;
  let parser_db: radlr.JSParserDB | null = null;
  let active_search_symbols: Set<string> = new Set();

  let scanner_off = [0, 0];
  let parser_off = [0, 0];

  let active_scanner_state_source = "";
  let active_state_source = "";

  function markSource(source: string, offsets: number[]) {
    return source.slice(0, offsets[0]) + "<span class=source-match>" + source.slice(...offsets) + "</span>" + source.slice(offsets[1]);
  }

  debugger_bus.addListener("db-created", (s) => {
    parser_db = s.db
  })

  debugger_bus.addListener("states-created", (s) => {
    states = s.states
  })

  debugger_bus.addListener("parser-db-created", (s) => {
    bc_db = s.bc_db
  })

  debugger_bus.addListener("parser-created", (s) => {
    parser = s.parser
  })

  debugger_bus.addListener("parser-reset", (s) => {
    if (bc_db && states && parser_db) {
      setView(8, JSDebugEvent.ExecuteState, false, parser_db, bc_db, states);
    }
  });


  debugger_bus.addListener("step", ({
    db,
    bc_db,
    debug_data,
    states,
    key_frame
  }) => {


    setView(debug_data.instruction, debug_data.event, debug_data.is_scanner, db, bc_db, states, key_frame);

  });

  function setView(
    instruction: number, event: any,
    is_scanner: boolean,
    db: radlr.JSParserDB,
    bc_db: radlr.JSBytecodeParserDB,
    states: radlr.JSIRParser,
    key_frame: boolean = false
  ) {

    if (instruction == 0) return;

    if (event == JSDebugEvent.ExecuteInstruction) {
      let token_offset = radlr.get_debug_tok_offsets(instruction, bc_db);
      if (token_offset) {
        if (is_scanner) {
          scanner_off[0] = token_offset.start - 1;
          scanner_off[1] = token_offset.end - 1;
        } else {
          parser_off[0] = token_offset.start - 1;
          parser_off[1] = token_offset.end - 1;
        }
      }
    } else if (event == JSDebugEvent.ExecuteState) {

      if (!is_scanner) {

        active_state_source = radlr.get_debug_state_name(instruction, bc_db, db);

      } else {

        active_scanner_state_source = radlr.get_debug_state_name(instruction, bc_db, db);
      }
    }
    if (key_frame)
      if (is_scanner) {
        //@ts-ignore
        ir_state.content.innerHTML = markSource(radlr.get_state_source_string(active_state_source, states), parser_off)
          + "\n\n"
          + markSource(radlr.get_state_source_string(active_scanner_state_source, states), scanner_off);
      } else {

        //@ts-ignore
        ir_state.content.innerHTML = markSource(radlr.get_state_source_string(active_state_source, states), parser_off);
      }

  }

  ir_state.attach(debugger_state);
}

function createDisassemblyView(debugger_state: Container) {

  let byte_code_state = new Thing("div", "debugger-disassembly-", "disassembly", false);

  byte_code_state.vertical_scroll();

  let prev_line: null | HTMLElement = null;

  debugger_bus.addListener("parser-reset", (s) => {

    let val: null | HTMLElement = byte_code_state.ele.querySelector("#disassembly-" + 8);

    if (val) {
      if (prev_line) {
        prev_line.style.backgroundColor = "";
        prev_line.style.color = "";
      }

      prev_line = val;

      val.style.backgroundColor = "rgb(12,0,0)";
      val.style.color = "white";
      val.scrollIntoView({
        block: "center",
        inline: "center"
      });
    }
  });


  debugger_bus.addListener("parser-db-created", (s) => {

    byte_code_state.content.innerHTML = "";

    let data = radlr.create_bytecode_disassembly(s.bc_db);

    for (const line of data.split("\n")) {

      let address = line.match(/([\w]{6})\|/);

      if (address) {
        let ele = document.createElement("p");

        ele.innerHTML = `<span class="diss-address">${parseInt(address[1], 16).toString(16)}</span> ${line.slice(7)}`;

        ele.classList.add("diss-line")

        byte_code_state.content.appendChild(ele);

        let val = parseInt(address[1], 16);
        ele.id = "disassembly-" + val;
      }
    }
  });


  debugger_bus.addListener("step", ({ debug_data, key_frame }) => {

    if (!key_frame) return;

    if (debug_data.instruction == 0) return;

    let val: null | HTMLElement = byte_code_state.ele.querySelector("#disassembly-" + debug_data.instruction);

    if (val) {
      if (prev_line) {
        prev_line.style.backgroundColor = "";
        prev_line.style.color = "";
      }

      prev_line = val;

      val.style.backgroundColor = "rgb(12,0,0)";
      val.style.color = "white";
      val.scrollIntoView({
        block: "center",
        inline: "center"
      });
    }

  });

  byte_code_state.attach(debugger_state);
}
