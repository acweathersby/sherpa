import init, { parse_input } from "./sherpa/sherpa_wasm.js";

import { basicSetup, EditorView } from 'codemirror';
export default async function (
    {
        codemirror_grammar_host,
        codemirror_syntax_host
    }: {
        codemirror_grammar_host: Element;
        codemirror_syntax_host: Element;
    }
) {

    await init();

    console.log("Sherpa WASM Runtime initialized");

    let guard = false;
    function parse(input: string) {
        if (guard) return;
        guard = true;
        parse_input(input);
        guard = false;
    }

    new EditorView({
        doc: "<> A > 'B'",
        extensions: [basicSetup, EditorView.updateListener.of((e) => {
            if (e.focusChanged && !e.view.hasFocus) {
                parse(e.state.doc.toString());
            }
        })],
        parent: codemirror_grammar_host
    });


    new EditorView({
        doc: "B",
        extensions: [basicSetup],
        parent: codemirror_syntax_host
    });


}

console.log("hello world"); 