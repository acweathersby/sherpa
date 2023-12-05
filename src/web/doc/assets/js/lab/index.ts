import init_radlr, * as radlr from "js/radlr/radlr_wasm.js";
import { basicSetup, EditorView, } from 'codemirror';
import { ScrollHandler } from "../controls/scroll";
import docs_handler from './docs_handler';
import { radlrLang } from './radlr_lang';
import { GrammarContext } from './grammar_context';
import { get_grammar, get_input, init, set_grammar_update_handler, set_parser_update_handler } from "../common/session_storage";
import { DebuggerButton, DebuggerCheckbox, DebuggerField } from "./debugger/debugger_io";
import { initDebugger } from "./debugger/debugger";
import { log } from "js/common/logger";
import { DividerHandler } from "js/controls/divider";


export { docs_handler, ScrollHandler, DividerHandler };
export { lab_handler } from "./lab_handler"


export default async function (
  {
    codemirror_grammar_host,
    codemirror_parser_host,
    disassembly_output,
    debugger_output,
    debugger_entry_selection,
  }: {
    codemirror_grammar_host: Element;
    codemirror_parser_host: Element;
    disassembly_output: Element;
    debugger_output: HTMLDivElement,
    debugger_entry_selection: HTMLSelectElement,
  }
) {
  try {
    await init_radlr();
    log("Radlr WASM Runtime initialized");
  } catch {
    alert("Radlr Failed to Load");
  }

  DebuggerButton.gatherButtons();
  DebuggerCheckbox.gatherCheckBoxes();
  DebuggerField.gatherFields();

  init(window);


  const ctx = new GrammarContext();
  initDebugger(ctx, codemirror_parser_host, debugger_entry_selection);

  const grammar_editor = new EditorView({
    doc: get_grammar(),

    extensions: [basicSetup,
      radlrLang(ctx),
      EditorView.editorAttributes.of({ class: "Codemirror" }),
    ],
    parent: codemirror_grammar_host
  });

  set_grammar_update_handler(grammar_str => grammar_editor.dispatch({ changes: { from: 0, to: grammar_editor.state.doc.length, insert: grammar_str } }));


}

