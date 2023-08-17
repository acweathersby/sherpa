import init_sherpa, * as sherpa from "js/sherpa/sherpa_wasm.js";
import { basicSetup, EditorView, } from 'codemirror';
import { log } from 'js/lab/logger';
import { ScrollHandler } from "../controls/scroll";
import docs_handler from './docs_handler';
import { sherpaLang } from './sherpa_lang';
import { parserHost } from './parser';
import { GrammarContext } from './grammar_context';
import { get_grammar, get_input } from "./session_storage";

export { docs_handler, ScrollHandler };

export default async function (
    {
        codemirror_grammar_host,
        codemirror_parser_host,
        disassembly_output,
        debugger_start_stop_button,
        debugger_step_button,
        debugger_into_button,
        debugger_out_button,
        debugger_output,
        debugger_entry_selection,
        debugger_optimize_checkbox
    }: {
        codemirror_grammar_host: Element;
        codemirror_parser_host: Element;
        disassembly_output: Element;
        debugger_start_stop_button: HTMLButtonElement,
        debugger_step_button: HTMLButtonElement,
        debugger_into_button: HTMLButtonElement,
        debugger_out_button: HTMLButtonElement,
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

    const ctx = new GrammarContext();

    const parser_editor = new EditorView({
        doc: get_input(),
        extensions: [basicSetup, parserHost(ctx, {
            debugger_start_stop_button,
            debugger_step_button,
            debugger_into_button,
            debugger_out_button,
            debugger_output,
            debugger_entry_selection,
            debugger_optimize_checkbox
        })],
        parent: codemirror_parser_host
    });

    const grammar_editor = new EditorView({
        doc: get_grammar(),

        extensions: [basicSetup,
            sherpaLang(ctx),
            EditorView.editorAttributes.of({ class: "Codemirror" }),
        ],
        parent: codemirror_grammar_host
    });

}

