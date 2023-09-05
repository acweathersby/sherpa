import init_sherpa, * as sherpa from "js/sherpa/sherpa_wasm.js";
import { basicSetup, EditorView, } from 'codemirror';
import { log } from 'js/lab/logger';
import { ScrollHandler } from "../controls/scroll";
import docs_handler from './docs_handler';
import { sherpaLang } from './sherpa_lang';
import { parserHost } from './parser';
import { GrammarContext } from './grammar_context';
import { get_grammar, get_input, init, set_grammar_update_handler, set_parser_update_handler } from "./session_storage";
import { DebuggerButton } from "./debugger_buttons";



export { docs_handler, ScrollHandler };

export default async function (
    {
        codemirror_grammar_host,
        codemirror_parser_host,
        disassembly_output,
        debugger_output,
        debugger_entry_selection,
        debugger_optimize_checkbox
    }: {
        codemirror_grammar_host: Element;
        codemirror_parser_host: Element;
        disassembly_output: Element;
        debugger_output: HTMLDivElement,
        debugger_entry_selection: HTMLSelectElement,
        debugger_optimize_checkbox: HTMLInputElement,
    }
) {
    try {
        await init_sherpa();
        log("Sherpa WASM Runtime initialized");
    } catch {
        alert("Sherpa Failed to Load");
    }

    DebuggerButton.gatherButtons();

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

    const parser_editor = new EditorView({
        doc: get_input(),
        extensions: [basicSetup, parserHost(ctx, {
            debugger_output,
            debugger_entry_selection,
            debugger_optimize_checkbox
        })],
        parent: codemirror_parser_host
    });

    set_parser_update_handler(parser_str => { console.log(parser_str); parser_editor.dispatch({ changes: { from: 0, to: parser_editor.state.doc.length, insert: parser_str } }) });

}

