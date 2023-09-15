
const LAB_GRAMMAR_KEY = "lab-data";
const LAB_INPUT_KEY = "lab-input";

let grammar_update_handler = (..._: any[]) => { };
let parser_update_handler = (..._: any[]) => { };

export function set_grammar(grammar: string, win: Window = window) {
    win.sessionStorage.setItem(LAB_GRAMMAR_KEY, grammar);
}

export function set_input(input: string, win: Window = window) {
    win.sessionStorage.setItem(LAB_INPUT_KEY, input);
}

export function set_grammar_update_handler(handler: (_: string) => void) {
    grammar_update_handler = handler;
}

export function set_parser_update_handler(handler: (_: string) => void) {
    parser_update_handler = handler;
}

export function get_grammar() {
    // Check for url encoded grammar
    let sessionText = sessionStorage.getItem(LAB_GRAMMAR_KEY);
    return sessionText || `<> A > 'B' C? \n\n<> C > "D"`;
}

export function get_input() {
    // Check for url encoded grammar
    let sessionText = sessionStorage.getItem(LAB_INPUT_KEY);
    return sessionText || `BD`;
}

export function init(window: Window) {
    window.addEventListener("storage", e => {

        if (new URL(e.url).pathname == document.location.pathname) return;

        switch (e.key) {
            case LAB_GRAMMAR_KEY: {
                grammar_update_handler(e.newValue)
            } break;
            case LAB_INPUT_KEY: {
                parser_update_handler(e.newValue)
            } break;
        }
    })
}
