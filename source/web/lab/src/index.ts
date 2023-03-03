import init_sherpa, { compile_grammar, JournalWrap } from "./sherpa/sherpa_wasm.js";
import { basicSetup, EditorView, } from 'codemirror';
import { linter, Diagnostic } from "@codemirror/lint";
import { log } from './logger.js';
import { ViewUpdate } from '@codemirror/view';
import { AnnotationType, Transaction } from '@codemirror/state';

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
    const default_grammar = "<> A > 'B'";

    try {
        await init_sherpa();
        log("Sherpa WASM Runtime initialized");
    } catch {
        alert("Sherpa Failed to Load");
    }

    const grammar_sys = new GrammarInterface();

    let PENDING_LINT_CHANGES = false;

    const grammar_editor = new EditorView({
        doc: default_grammar,
        extensions: [basicSetup,
            setupGrammarLinting(grammar_sys, (v): boolean => {
                for (const t of v.transactions) {
                    if (t.isUserEvent("grammar.built")) {
                        return true;
                    }
                }
                return false;
            }),
            setupAutoBuild(grammar_sys)
        ],
        parent: codemirror_grammar_host
    });

    grammar_sys.on("valid-build", (g) => {
        g.build_states();
        grammar_editor.dispatch({ userEvent: "grammar.built" });
    });

    grammar_sys.on("invalid-build", (g) => {
        g.build_states();
        PENDING_LINT_CHANGES = true;
        grammar_editor.dispatch({ userEvent: "grammar.built" });
    });


    grammar_sys.on("bytecode-ready", (g) => {
        disassembly_output.innerHTML = g.disassembly;
    });

    grammar_sys.parse(default_grammar);

    const source_editor = new EditorView({
        doc: "B",
        extensions: [basicSetup],
        parent: codemirror_syntax_host
    });
}


function setupGrammarLinting(g: GrammarInterface, trigger: (update: ViewUpdate) => boolean) {
    return linter((v): readonly Diagnostic[] => {
        let diagnostics: Diagnostic[] = [];
        if (g.HAVE_GRAMMAR && !g.BUILDING && !g.VALID_GRAMMAR) {
            for (const error of g.get_grammar_errors()) {
                diagnostics.push({
                    from: error.start,
                    to: error.end,
                    severity: "error",
                    message: error.msg,
                });
            }
        }
        return diagnostics;
    }, {
        needsRefresh: trigger,
        delay: 100
    });
}

function setupAutoBuild(g: GrammarInterface) {
    let TRIGGER = 0;
    return EditorView.updateListener.of((e) => {

        if (e.docChanged == false || e.transactions.some(t => t.isUserEvent("grammar"))) {
            return;
        } if (TRIGGER != 0) {
            clearTimeout(TRIGGER);
            TRIGGER = 0;
        }

        g.invalidate();

        let string = e.state.doc.toString();

        TRIGGER = setTimeout(() => g.parse(string), 500);
    });
}

interface GrammarError {
    type: string, msg: string, start: number, end: number;
};

/**
 * Signals
 * - `valid-build` - Indicates the resulting grammar after a call to `parse` is valid.
 * - `invalid-build` - Indicates the result grammar after a call to `parse` is invalid and contains errors.
 * - `parser-busy` - Indicates the grammar parser is busy and cannot be used to parse another grammar at this time.
 * - `parser-ready` - Indicates the grammar parser is available to parse another input.
 * - `states-ready` - Indicates the parse states data is available for reading.
 * - `bytecode-ready` - Indicates the bytecode data is available for reading.
 */
class GrammarInterface {

    constructor() {
        this.WRITE_ACTIVE = false;
        this.active_grammar = null;
        this.event_listener = new Map();
    }


    get_grammar_errors(): GrammarError[] {
        if (this.WRITE_ACTIVE) {
            return [];
        } else {
            return JSON.parse(this.active_grammar?.get_grammar_errors() ?? "[]");
        }
    };

    async parse(grammar: string) {
        return this.write_guard(
            () => {
                this.invalidate();

                this.active_grammar = compile_grammar(grammar);

                let VALID = this.active_grammar.is_valid();

                log(`Compiled grammar is ${VALID ? "valid!" : "not valid!"}`);

                this.WRITE_ACTIVE = false;

                if (VALID) {
                    this.signal("valid-build");
                } else {
                    this.signal("invalid-build");
                }
            }
        );

    }

    async build_states() {
        return this.write_guard(
            () => {
                if (this.active_grammar) {
                    this.active_grammar?.compile_states(true);
                    log(`Compiled grammar states`);
                    this.active_grammar?.compile_bytecode(true);
                    log(`Compiled grammar bytecode`);
                    this.WRITE_ACTIVE = false;
                    this.signal("states-ready");
                    this.signal("bytecode-ready");
                }
            }
        );
    }

    invalidate() {
        if (this.active_grammar) {
            this.active_grammar.free();
            this.active_grammar = null;
        }
    }

    on(event: string, listener: (g: GrammarInterface) => void) {
        if (!this.event_listener.has(event)) {
            this.event_listener.set(event, []);
        }
        this.event_listener.get(event)?.push(listener);
    }

    get disassembly(): string {
        return this.active_grammar?.generate_disassembly() ?? "";
    }

    get HAVE_GRAMMAR(): boolean {
        return this.active_grammar != null;
    }

    get VALID_GRAMMAR(): boolean {
        return this.active_grammar?.is_valid() ?? true;
    }

    get BUILDING(): boolean {
        return this.WRITE_ACTIVE;
    }

    private async write_guard(guarded_fn: () => void) {
        if (this.WRITE_ACTIVE) {
            this.signal("parser-busy");
            return this;
        };
        this.WRITE_ACTIVE = true;
        try {
            await guarded_fn();
        } catch (err) {
            console.log(err);
        } finally {
            this.WRITE_ACTIVE = false;
            this.signal("parser-ready");
            return this;
        }
    };

    private signal(event: string) {
        this.event_listener.get(event)?.forEach(l => l(this));
    }

    private WRITE_ACTIVE: boolean = false;
    private active_grammar: JournalWrap | null = null;

    private event_listener: Map<string, ((g: GrammarInterface) => void)[]>;
}
