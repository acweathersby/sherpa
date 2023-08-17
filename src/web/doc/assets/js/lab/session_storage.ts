
const LAB_GRAMMAR_KEY = "lab-data";
const LAB_INPUT_KEY = "lab-input-key";

export function get_grammar() {
    // Check for url encoded grammar
    let sessionText = sessionStorage.getItem(LAB_GRAMMAR_KEY);
    return sessionText || `<> A > 'B' C? \n\n<> C > "D"`;
}

export function set_grammar(grammar: string) {
    sessionStorage.setItem(LAB_GRAMMAR_KEY, grammar);
}

export function get_input() {
    // Check for url encoded grammar
    let sessionText = sessionStorage.getItem(LAB_INPUT_KEY);
    return sessionText || `BD`;
}

export function set_input(input: string) {
    sessionStorage.setItem(LAB_INPUT_KEY, input);
}