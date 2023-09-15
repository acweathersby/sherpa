import init_sherpa, * as sherpa from "js/sherpa/sherpa_wasm.js";
import { basicSetup, EditorView, } from 'codemirror';
import { ScrollHandler } from "../controls/scroll";
import docs_handler from './docs_handler';
import { sherpaLang } from './sherpa_lang';
import { GrammarContext } from './grammar_context';
import { get_grammar, get_input, init, set_grammar_update_handler, set_parser_update_handler } from "../common/session_storage";
import { DebuggerButton, DebuggerCheckbox } from "./debugger/debugger_buttons";
import { initDebugger } from "./debugger/debugger";
import { log } from "js/common/logger";



export { docs_handler, ScrollHandler };

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
        await init_sherpa();
        log("Sherpa WASM Runtime initialized");
    } catch {
        alert("Sherpa Failed to Load");
    }

    DebuggerButton.gatherButtons();
    DebuggerCheckbox.gatherCheckBoxes();

    init(window);


    const ctx = new GrammarContext();

    const grammar_editor = new EditorView({
        doc: get_grammar(),

        extensions: [basicSetup,
            sherpaLang(ctx),
            EditorView.editorAttributes.of({ class: "Codemirror" }),
        ],
        parent: codemirror_grammar_host
    });

    set_grammar_update_handler(grammar_str => grammar_editor.dispatch({ changes: { from: 0, to: grammar_editor.state.doc.length, insert: grammar_str } }));

    let sherpa_debugger = initDebugger(ctx, codemirror_parser_host, debugger_entry_selection);

}

