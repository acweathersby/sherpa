/**
 * Provides functionality for parsing inputs using compiled parsers 
 */
import * as sherpa from "js/sherpa/sherpa_wasm.js";
import { EventType, GrammarContext } from "./grammar_context";
import { ViewPlugin, DecorationSet, ViewUpdate } from "@codemirror/view"
import { StateField, StateEffect, Range } from "@codemirror/state"
import { EditorView, Decoration } from "@codemirror/view"
import { set_input } from "./session_storage";
import { DebuggerButton } from "./debugger_buttons";


const head_dec = Decoration.mark({ attributes: { style: "background-color: red" } });
const scan_dec = Decoration.mark({ attributes: { style: "background-color: blue" } });
const end_dec = Decoration.mark({ attributes: { style: "background-color: green" } });

const highlight_effect = StateEffect.define<Range<Decoration>[]>();
const filter_effects = StateEffect.define<((from: number, to: number) => boolean)>();

export function parserHost(ctx: GrammarContext, {
    debugger_output,
    debugger_entry_selection,
    debugger_optimize_checkbox
}: {
    debugger_output: HTMLDivElement,
    debugger_entry_selection: HTMLSelectElement,
    debugger_optimize_checkbox: HTMLInputElement,
}) {
    let view: EditorView | null = null;
    let bytecode: sherpa.JSBytecode | null = null;
    let states: sherpa.JSParseStates | null = null;
    let parser: sherpa.JSByteCodeParser | null = null;
    let PARSING: boolean = false;
    let PARSER_VALID: boolean = false;
    let debugger_steps: any[] = [];
    let debugger_offset: number = -1;
    let play_interval = -1;
    let optimize = false;
    let active_search_symbols: Set<string> = new Set();
    let active_state_source = '';
    let active_scanner_state_source = '';
    let parser_off: [number, number] = [0, 0];
    let scanner_off: [number, number] = [0, 0];

    ctx.addListener(EventType.GrammarAdded, ctx => {
        console.log("Grammar Added")
        states = null;
    })

    ctx.addListener(EventType.DBDeleted, ctx => {
        console.log("DBDeleted")
        destroy_states();
    })

    ctx.addListener(EventType.DBCreated, ctx => {
        console.log("DBCreated")

        // Now we can create a parser. 
        let db = ctx.db;

        if (!db) return;

        configure_entry_options(db);
    })

    function create_parser_data() {

        if (states) return;
        if (!ctx.db) return;

        let db = ctx.db;

        try {
            states = sherpa.create_parser_states(db, optimize);
            console.log(states);
            bytecode = sherpa.create_bytecode(states);
            // Build the soup.
            let output = document.getElementById("bytecode-output");
            if (output) {
                output.innerText = sherpa.create_bytecode_disassembly(bytecode);
            }

        } catch (e) {
            console.log(e)
        }
        if (db) return;
    }

    function configure_entry_options(db: sherpa.JSParserDB) {
        debugger_entry_selection.innerHTML = "";

        for (const entry_name of sherpa.get_entry_names(db)) {
            let option = document.createElement("option");
            option.innerText = entry_name;
            option.value = entry_name;
            debugger_entry_selection.appendChild(option);
        }
    }

    function destroy_parser() {
        if (parser) {
            parser.free();
            parser = null;
        }
    }

    function destroy_states() {
        if (states) {
            states.free();
            states = null;
        }
    }

    function create_parser() {
        if (!(view && bytecode && ctx.db)) return;
        destroy_parser()
        parser = sherpa.JSByteCodeParser.new(view.state.doc.toString(), bytecode);
        parser.init(debugger_entry_selection.value, bytecode, ctx.db);

    }

    function build_parser() {
        if (PARSER_VALID) return;

        if (!(view && ctx.db && !PARSING)) return;

        create_parser_data();

        if (!(states && bytecode)) return;

        create_parser();

        if (!parser) return;

        PARSER_VALID = true;

        enableParserButtons()
    }

    function invalidate_parser() {
        stop_parser();
        destroy_states();
        disableParserButtons();
        PARSER_VALID = false;
    }

    function disableParserButtons() {
        DebuggerButton.get("step").disable = true;
        DebuggerButton.get("step-action").disable = true;
        DebuggerButton.get("play").disable = true;
        DebuggerButton.get("restart").disable = true;
        DebuggerButton.get("build").active = false;
        DebuggerButton.get("build").disable = false;
    }

    function enableParserButtons() {
        DebuggerButton.get("step").disable = false;
        DebuggerButton.get("step-action").disable = false;
        DebuggerButton.get("play").disable = false;
        DebuggerButton.get("restart").disable = false;
        DebuggerButton.get("build").active = true;
        DebuggerButton.get("build").disable = true;
    }

    function stop_parser() {
        if (!PARSING) return;
        destroy_parser();
        toggle_play(true);
        PARSING = false;
    }

    function restart_parser() {
        stop_parser();
        start_parser();
    }

    function start_parser() {

        if (!PARSER_VALID)
            return;

        if (PARSING) {
            destroy_parser();
        }

        create_parser();

        debugger_offset = -1;
        active_search_symbols.clear();
        debugger_steps.length = 0;
        debugger_output.innerText = "";
        active_state_source = "";
        active_scanner_state_source = "";

        if (view)
            view.dispatch({ userEvent: "debugger.start" })

        PARSING = true;

        step_forward();
    }

    function step_forward() {


        if (view && parser && states && bytecode && ctx.db && PARSING) {

            let db = ctx.db;
            debugger_offset += 1;

            if (debugger_offset >= debugger_steps.length) {
                let result = parser.next();
                if (Array.isArray(result))
                    debugger_steps.push(...result);

                if (debugger_offset >= debugger_steps.length) {
                    toggle_play(true);
                }
            }

            debugger_offset = Math.min(debugger_offset, debugger_steps.length - 1);

            let step;
            outer: while ((step = debugger_steps[debugger_offset])) {
                switch (step.type) {
                    case "ShiftToken":
                        active_search_symbols.clear();
                        break;
                    case "ExecuteInstruction": {


                        if (!step.is_scanner) {
                            active_scanner_state_source = "";

                            let token_offset = sherpa.get_debug_tok_offsets(step.instruction, bytecode);
                            if (token_offset) {
                                parser_off[0] = token_offset.start - 1;
                                parser_off[1] = token_offset.end - 1;
                                break;
                            }

                            let debug_symbols: number[] = sherpa.get_debug_symbol_ids(step.instruction, bytecode);
                            if (debug_symbols.length > 0) {
                                debug_symbols.forEach(s => active_search_symbols.add(sherpa.get_symbol_name_from_id(s, db)));
                                break
                            }

                            let name = sherpa.get_debug_state_name(step.instruction, bytecode);
                            if (name) {
                                active_state_source = sherpa.get_state_source_string(name, states);
                                break
                            }
                        } else {
                            let token_offset = sherpa.get_debug_tok_offsets(step.instruction, bytecode);
                            if (token_offset) {
                                scanner_off[0] = token_offset.start - 1;
                                scanner_off[1] = token_offset.end - 1;
                                break;
                            }


                            let name = sherpa.get_debug_state_name(step.instruction, bytecode);
                            if (name) {
                                active_scanner_state_source = sherpa.get_state_source_string(name, states);
                                break
                            }
                        }

                        debugger_output.innerText = JSON.stringify(step, undefined, 2)
                            + "\n\n"
                            + [...active_search_symbols].join(" | ")
                            + "\n\n"
                            + sherpa.create_instruction_disassembly(step.instruction, bytecode)
                            + "\n\n"
                            + markSource(active_state_source, parser_off)
                            + "\n\n"
                            + markSource(active_scanner_state_source, scanner_off)


                        let effects: any[] = [filter_effects.of((from, to) => false)]

                        let { head_ptr, scan_ptr } = step;

                        effects.push(highlight_effect.of([
                            head_dec.range(head_ptr, head_ptr + 1)
                        ]))

                        if (scan_ptr > head_ptr) {
                            effects.push(highlight_effect.of([
                                scan_dec.range(scan_ptr, scan_ptr + 1)
                            ]))
                        }
                        view.dispatch({ effects })
                    } break outer;
                }

                debugger_offset++;
            }
        }

        function markSource(source: string, offsets: [number, number]) {
            return source.slice(0, offsets[0]) + "|" + source.slice(...offsets) + "|" + source.slice(offsets[1]);
        }
    }

    function toggle_play(force_stop: boolean = false) {
        if (play_interval >= 0 || force_stop) {
            if (play_interval >= 0) {
                clearInterval(play_interval);
                play_interval = -1;
            }
            DebuggerButton.get("play").active = false;
        } else if (PARSING) {
            play_interval = setInterval(step_forward, 1);
            DebuggerButton.get("play").active = true;
        } else {
            restart_parser()
            toggle_play();
        }
    }

    debugger_optimize_checkbox.addEventListener("change", e => {
        invalidate_parser();
        optimize = debugger_optimize_checkbox.checked;
    })

    DebuggerButton.get("restart").addEventListener("click", restart_parser);

    DebuggerButton.get("step").addEventListener("click", step_forward);

    DebuggerButton.get("play").addEventListener("click", e => toggle_play());

    DebuggerButton.get("build").addEventListener("click", e => {
        build_parser();
        start_parser();
    });

    window.addEventListener("keydown", e => {
        if (e.altKey) {
            console.log(e.code, e.key)
            switch (e.key) {
                case "r":
                    if (PARSING) {
                        toggle_play(true);
                        restart_parser();
                    }
                    e.stopImmediatePropagation();
                    e.stopPropagation();
                    e.preventDefault();
                    return false
                case "b":
                    if (!PARSER_VALID) {
                        build_parser();
                        start_parser();
                    }
                    e.stopImmediatePropagation();
                    e.stopPropagation();
                    e.preventDefault();
                    return false
                case "n":
                    if (PARSING) {
                        toggle_play(true);
                        step_forward();
                    }
                    e.stopImmediatePropagation();
                    e.stopPropagation();
                    e.preventDefault();
                    return false
                case " ":
                    if (PARSING) {
                        toggle_play();
                    }
                    e.stopImmediatePropagation();
                    e.stopPropagation();
                    e.preventDefault();
                    return false
            }
        }

    })


    invalidate_parser();

    return [
        ViewPlugin.fromClass(class {
            update(update: ViewUpdate) {
                if (update.transactions.find(e => e.isUserEvent("debugger.start"))) {
                    console.log("Started");
                } else if (update.docChanged) {
                    invalidate_parser();
                }
            }
        }, {}),
        EditorView.updateListener.of(function (e) {
            set_input(e.state.doc.toString());
            view = e.view;
        }),
        StateField.define({
            create() { return Decoration.none },
            update(value, tr) {
                value = value.map(tr.changes)

                for (let effect of tr.effects) {
                    if (effect.is(highlight_effect)) value = value.update({ add: effect.value, sort: true });
                    else if (effect.is(filter_effects)) value = value.update({ filter: effect.value });
                }

                return value
            },
            provide(f) { return EditorView.decorations.from(f) }
        })
    ]
}