import init_sherpa, { create_soup, JSSoup } from "js/sherpa/sherpa_wasm.js";
import { basicSetup, EditorView, } from 'codemirror';
import { linter, Diagnostic } from "@codemirror/lint";
import { log } from 'js/lab/logger';
import { ViewUpdate, } from '@codemirror/view';
import { AnnotationType, Transaction } from '@codemirror/state';
import { ScrollHandler } from "../controls/scroll";
import docs_handler from './docs_handler';
import { sherpaLang } from './sherpa_lang';

export { docs_handler, ScrollHandler };

let soup: JSSoup;

export default async function (
    {
        codemirror_grammar_host,
        codemirror_syntax_host,
        disassembly_output,
    }: {
        codemirror_grammar_host: Element;
        codemirror_syntax_host: Element;
        disassembly_output: Element;
    }
) {
    const default_grammar = get_grammar();

    try {
        await init_sherpa();
        soup = create_soup();
        log("Sherpa WASM Runtime initialized");
    } catch {
        alert("Sherpa Failed to Load");
    }

    const grammar_editor = new EditorView({
        doc: default_grammar,

        extensions: [basicSetup,
            sherpaLang(soup),
            EditorView.editorAttributes.of({ class: "Codemirror" }),
        ],
        parent: codemirror_grammar_host
    });
}




function get_grammar() {
    // Check for url encoded grammar
    let sessionText = sessionStorage.getItem("lab-data");
    return sessionText || "<> A > 'B'";
}