import init_sherpa, { compile_grammar, JournalWrap } from "./sherpa/sherpa_wasm.js";
import { basicSetup, EditorView } from 'codemirror';
import { log } from './logger.js';

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
    try {
        await init_sherpa();
        log("Sherpa WASM Runtime initialized");
    } catch {
        alert("Sherpa Failed to Load");
    }

    const grammar_sys = new GrammarInterface();


    const grammar_editor = new EditorView({
        doc: "<> A > 'B'",
        extensions: [basicSetup,
            ((PENDING_BUILD: number, g: GrammarInterface) => EditorView.updateListener.of((e) => {
                PENDING_BUILD |= +e.docChanged;
                if (e.focusChanged && !e.view.hasFocus && PENDING_BUILD) {
                    PENDING_BUILD = 0;
                    g.parse(e.state.doc.toString());
                }
            }))(1, grammar_sys)],
        parent: codemirror_grammar_host
    });

    grammar_sys.on("valid-build", (g) => {
        g.build_states();
    });

    grammar_sys.on("bytecode-ready", (g) => {
        disassembly_output.innerHTML = g.disassembly;
    });

    const source_editor = new EditorView({
        doc: "B",
        extensions: [basicSetup],
        parent: codemirror_syntax_host
    });
}


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
        this.parse_guard = false;
        this.active_grammar = null;
        this.event_listener = new Map();
    }

    async parse(grammar: string) {
        return this.write_guard(
            () => {
                this.active_grammar = compile_grammar(grammar);

                let VALID = this.active_grammar.is_valid();

                log(`Compiled grammar is ${VALID ? "valid!" : "not valid!"}`);

                this.parse_guard = false;

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
                this.active_grammar?.compile_states(true);
                log(`Compiled grammar states`);
                this.active_grammar?.compile_bytecode(true);
                log(`Compiled grammar bytecode`);
                this.parse_guard = false;
                this.signal("states-ready");
                this.signal("bytecode-ready");
            }
        );
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

    private async write_guard(guarded_fn: () => void) {
        if (this.parse_guard) {
            this.signal("parser-busy");
            return this;
        };
        this.parse_guard = true;
        try {
            await guarded_fn();
        } catch (err) {
            console.log(err);
        } finally {
            this.parse_guard = false;
            this.signal("parser-ready");
            return this;
        }
    };

    private signal(event: string) {
        this.event_listener.get(event)?.forEach(l => l(this));
    }

    private parse_guard: boolean = false;
    private active_grammar: JournalWrap | null = null;

    private event_listener: Map<string, ((g: GrammarInterface) => void)[]>;
}
