/**
 * Provides functionality for parsing inputs using compiled parsers 
 */
import * as sherpa from "js/sherpa/sherpa_wasm.js";
import { JSReductionType } from "js/sherpa/sherpa_wasm.js";
import { EventType, GrammarContext } from "./grammar_context";
import { ViewPlugin, DecorationSet, ViewUpdate } from "@codemirror/view"
import { StateField, StateEffect, Range } from "@codemirror/state"
import { EditorView, Decoration } from "@codemirror/view"
import { set_input } from "./session_storage";
import { DebuggerButton, DebuggerCheckbox } from "./debugger_buttons";


const head_dec = Decoration.mark({ attributes: { style: "background-color: red" } });
const scan_dec = Decoration.mark({ attributes: { style: "background-color: blue" } });
const end_dec = Decoration.mark({ attributes: { style: "background-color: green" } });

const highlight_effect = StateEffect.define<Range<Decoration>[]>();
const filter_effects = StateEffect.define<((from: number, to: number) => boolean)>();


class CSTNode {
    public children: CSTNode[]
    public name: string
    public expression: string
    public terminal: boolean;

    constructor(name: string, expression: string, terminal: boolean) {
        this.children = [];
        this.name = name;
        this.terminal = terminal;
        this.expression = expression;
    }


    toDOM(): HTMLElement {
        const ele = document.createElement("div");
        ele.classList.add("cst-node");
        ele.classList.add("close")



        const name_ele = document.createElement("div");
        name_ele.classList.add("cst-name");
        name_ele.innerText = this.name;
        ele.appendChild(name_ele);

        name_ele.addEventListener("click", e => {
            if (ele.classList.contains("open")) {
                ele.classList.add("close")
                ele.classList.remove("open")
            } else {
                ele.classList.add("open")
                ele.classList.remove("close")
            }
            e.stopImmediatePropagation();
            e.stopPropagation();
            e.preventDefault();
            return false;
        })

        if (this.terminal) {
            ele.classList.add("terminal")
        } else {
            ele.classList.add("nonterminal")
            if (this.expression) {
                let e_ele = document.createElement("span")
                e_ele.innerText = this.expression
                e_ele.classList.add("cst-rule-item-expression")
                name_ele.appendChild(e_ele);
            }
        }

        if (this.children.length > 0) {
            let children = document.createElement("div");
            children.classList.add("cst-children")
            for (const child of this.children) {
                children.appendChild(child.toDOM())
            }
            ele.appendChild(children);
        }

        return ele;
    }
}

export function parserHost(ctx: GrammarContext, {
    debugger_entry_selection,
}: {
    debugger_output: HTMLDivElement,
    debugger_entry_selection: HTMLSelectElement,
}) {
    let view: EditorView | null = null;
    let bytecode: sherpa.JSBytecodePackage | null = null;
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
    let input: string = "";
    let cst_nodes: CSTNode[] = [];

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

    function disableTransportButtons() {
        DebuggerButton.get("step").disable = true;
        DebuggerButton.get("step-action").disable = true;
        DebuggerButton.get("play").disable = true;
        DebuggerButton.get("restart").disable = false;
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
        active_state_source = "";
        active_scanner_state_source = "";

        if (view)
            view.dispatch({ userEvent: "debugger.start" })

        PARSING = true;

        enableParserButtons();


        cst_nodes.length = 0;
        render_cst();

        step_forward();
    }

    function render_cst() {
        let ele = document.getElementById("debugger-cst-output");
        if (ele) {
            ele.innerHTML = "";
            for (const node of cst_nodes) {
                ele.appendChild(node.toDOM())
            }
        }
    }



    function markSource(source: string, offsets: [number, number]) {
        return source.slice(0, offsets[0]) + "<span class=source-match>" + source.slice(...offsets) + "</span>" + source.slice(offsets[1]);
    }

    function print_instruction(step: any | null) {
        if (step && view && parser && states && bytecode && ctx.db && PARSING) {
            document.getElementById("debugger-ir-state").innerHTML = markSource(active_state_source, parser_off)
                + "\n\n"
                + markSource(active_scanner_state_source, scanner_off);

            document.getElementById("debugger-disassembly").innerText = sherpa.create_instruction_disassembly(step.instruction, bytecode)


            document.getElementById("debugger-metrics").innerText = JSON.stringify(step, undefined, 2)
                + "\n\n"
                + [...active_search_symbols].join(" | ");

        }
    }

    let last_step: any = null;

    function step_forward(step_to_next_action: boolean = false) {


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
                    case "ExecuteState": {
                        let ctx = <sherpa.JSCTXState>step.ctx;
                        if (!ctx.is_scanner) {

                            let name = sherpa.get_debug_state_name(step.instruction, bytecode, db);
                            if (name) {
                                active_state_source = sherpa.get_state_source_string(name, states);
                                break
                            }
                        } else {

                            let name = sherpa.get_debug_state_name(step.instruction, bytecode, db);
                            if (name) {
                                active_scanner_state_source = sherpa.get_state_source_string(name, states);
                                break
                            }
                        }

                    } break;
                    case "ExecuteInstruction": {

                        last_step = step;

                        let ctx = <sherpa.JSCTXState>step.ctx;

                        let effects: any[] = [filter_effects.of((from, to) => false)]

                        let { head_ptr, scan_ptr } = ctx;

                        effects.push(highlight_effect.of([
                            head_dec.range(head_ptr, head_ptr + 1)
                        ]))

                        if (scan_ptr > head_ptr) {
                            effects.push(highlight_effect.of([
                                scan_dec.range(scan_ptr, scan_ptr + 1)
                            ]))
                        }

                        view.dispatch({ effects })

                        let token_offset = sherpa.get_debug_tok_offsets(step.instruction, bytecode);
                        if (token_offset) {
                            if (ctx.is_scanner) {
                                scanner_off[0] = token_offset.start - 1;
                                scanner_off[1] = token_offset.end - 1;
                            } else {
                                parser_off[0] = token_offset.start - 1;
                                parser_off[1] = token_offset.end - 1;
                            }
                        }

                        let debug_symbols: number[] | undefined = sherpa.get_debug_symbol_ids(step.instruction, bytecode);
                        if (debug_symbols && debug_symbols.length > 0) {

                            //debug_symbols.forEach(s => active_search_symbols.add(sherpa.get_symbol_name_from_id(s, db)));
                        }

                        if (!ctx.is_scanner) {
                            active_scanner_state_source = "";
                        }

                        if (step_to_next_action) { break }

                        print_instruction(step);


                        let next_step = debugger_steps[debugger_offset + 1];

                        if (next_step && ["Shift", "Reduce", "Skip"].includes(next_step.type)) {
                            break;
                        } else {
                            break outer;
                        }
                    };
                    case "Skip": {


                        print_instruction(last_step);
                    } break outer;;
                    case "Shift": {
                        let { offset_start, offset_end } = step;
                        cst_nodes.push(new CSTNode(input.slice(offset_start, offset_end), "", true));
                        render_cst();
                        print_instruction(last_step);
                    } break outer;;
                    case "Reduce": {
                        let { nonterminal_id, rule_id, symbol_count } = step;
                        let expr = sherpa.get_rule_expression_string(rule_id, db);

                        if (true) {
                            let name = sherpa.get_nonterminal_name_from_id(nonterminal_id, db)
                            let node = new CSTNode(name, expr, false);
                            node.children = cst_nodes.slice(-symbol_count);
                            cst_nodes.length -= symbol_count;
                            cst_nodes.push(node);
                        } else {

                            switch (sherpa.get_rule_reduce_type(rule_id, db)) {
                                case JSReductionType.LeftRecursive:
                                    {
                                        let nodes = cst_nodes.slice(-symbol_count);
                                        let first = nodes.shift();
                                        if (first) {
                                            if (first?.name != sherpa.get_nonterminal_name_from_id(nonterminal_id, db)) {
                                                // intentional fall through;
                                            } else {
                                                first.children.push(...nodes);
                                                cst_nodes.length -= symbol_count;
                                                cst_nodes.push(first);
                                                break
                                            }
                                        }
                                    }
                                case JSReductionType.Mixed:
                                case JSReductionType.SingleTerminal:
                                case JSReductionType.SemanticAction:
                                    {
                                        let name = sherpa.get_nonterminal_name_from_id(nonterminal_id, db)
                                        let node = new CSTNode(name, expr, false);
                                        node.children = cst_nodes.slice(-symbol_count);
                                        cst_nodes.length -= symbol_count;
                                        cst_nodes.push(node);
                                    }
                                    break
                            }
                        }
                        render_cst();
                        print_instruction(last_step);
                    } break outer;;
                    case "Complete": {
                        console.log("COMPLETE");
                        print_instruction(last_step);
                        toggle_play(true);
                        disableTransportButtons();
                        PARSING = false;
                    } break outer;;
                    case "Error": {
                        console.log("FAILURE");
                        print_instruction(last_step);
                        toggle_play(true);
                        disableTransportButtons();
                        PARSING = false;
                    } break outer;;
                    case "EndOfFile": {
                        toggle_play(true);
                        PARSING = false;
                    } break outer;;
                    case "Undefined": {

                    } break outer;
                    case "ShiftToken": {
                        active_search_symbols.clear();
                    } break outer;
                }

                debugger_offset++;
            }
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
    ``

    DebuggerButton.get("restart").addEventListener("click", restart_parser);
    DebuggerButton.get("step").addEventListener("click", () => step_forward());
    DebuggerButton.get("step-action").addEventListener("click", () => step_forward(true));
    DebuggerButton.get("play").addEventListener("click", e => toggle_play());
    DebuggerButton.get("build").addEventListener("click", e => {
        build_parser();
        start_parser();
    }); DebuggerCheckbox.get("optimize").addEventListener("change", e => {
        invalidate_parser();
        optimize = DebuggerCheckbox.get("optimize").ele.checked;
    })

    window.addEventListener("keydown", e => {
        if (e.altKey) {
            switch (e.key) {
                case "r":
                    if (PARSING) {
                        toggle_play(true);
                    }
                    restart_parser();
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
                case "a":
                    if (PARSING) {
                        toggle_play(true);
                        step_forward(true);
                    }
                    e.stopImmediatePropagation();
                    e.stopPropagation();
                    e.preventDefault();
                    return false
                case " ":
                    if (PARSING) {
                        toggle_play();
                    } else if (PARSER_VALID) {
                        restart_parser();
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
                    stop_parser();
                }
            }
        }, {}),
        EditorView.updateListener.of(function (e) {
            input = e.state.doc.toString();
            set_input(input);
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