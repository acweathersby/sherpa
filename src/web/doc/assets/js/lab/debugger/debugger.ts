import { JSParserConfig } from "js/sherpa/sherpa_wasm";
import { DebuggerButton, DebuggerCheckbox, DebuggerField } from "./debugger_io";
import { FlowNode, RootFlowNode } from "../../common/flow";
import { EventType as GrammarEventType, GrammarContext } from "../grammar_context";
import { get_input } from "../../common/session_storage";
import * as sherpa from "js/sherpa/sherpa_wasm.js";
import { StateField } from "@codemirror/state"
import { EditorView } from "@codemirror/view"
import { basicSetup } from "codemirror";
import { TransportHandler } from "./transport_handler";

export type DebuggerData = {
    debugger_entry_selection: HTMLSelectElement,
    states: sherpa.JSParseStates | null,
    bytecode: sherpa.JSBytecodePackage | null,
    parser_editor: EditorView | null,
    parser_host: Element,
    grammar_ctx: GrammarContext,
    PARSER_VALID: boolean,
    PARSING: boolean,
    config: JSParserConfig
};

export class ClearDebuggerError extends FlowNode<DebuggerData> {
    error_bar: HTMLElement;
    error_blackout: HTMLElement;

    constructor() {
        super();
        this.error_bar = <any>document.getElementById("error-bar");
        this.error_blackout = <any>document.getElementById("error-blackout");
        this.error_blackout.classList.remove("show");
        this.error_bar.classList.remove("show");
    }

    update(t: string, data: DebuggerData) {
        return []
    }
}


export class DebuggerError extends FlowNode<DebuggerData> {
    error_bar: HTMLElement;
    error_blackout: HTMLElement;

    constructor(message: string) {
        super();
        this.error_bar = <any>document.getElementById("error-bar");
        this.error_blackout = <any>document.getElementById("error-blackout");
        this.error_blackout.classList.add("show");
        this.error_bar.classList.add("show");
        this.error_bar.innerHTML = message;
    }

    update(t: string, data: DebuggerData) {
        return []
    }
}

class DebuggerRoot extends RootFlowNode<DebuggerData> { }

class InitCodeMirror extends FlowNode<DebuggerData> {
    update(t: string, state: DebuggerData) {

        if (t == "init") {

            state.parser_editor = new EditorView({
                doc: get_input(),
                extensions: [basicSetup],
                parent: state.parser_host
            });

            this.updateState(state);

            return [new GrammarDocListener()];
        } else {
            return [new DisableButtons()]
        }
    }
}

class GrammarDocListener extends FlowNode<DebuggerData> {

    configure_entry_options(db: sherpa.JSParserDB, entries: HTMLSelectElement) {
        entries.innerHTML = "";

        for (const entry_name of sherpa.get_entry_names(db)) {
            let option = document.createElement("option");
            option.innerText = entry_name;
            option.value = entry_name;
            entries.appendChild(option);
        }
    }

    update(t: string, data: DebuggerData) {
        switch (t) {
            case "init": {
                data.grammar_ctx.addListener(GrammarEventType.DBCreated, this.db_created.bind(this));
                data.grammar_ctx.addListener(GrammarEventType.DBDeleted, this.db_deleted.bind(this));
                data.grammar_ctx.addListener(GrammarEventType.MalformedGrammar, this.malformed_grammar.bind(this));
            } break;
            case "db_created": {
                if (data.grammar_ctx.db) {
                    this.configure_entry_options(data.grammar_ctx.db, data.debugger_entry_selection)
                    return [this, new ParseBuilder]
                } else {
                    return [this, new DebuggerError("Grammar is invalid")];
                }
            };
            case "malformed_grammar":
                return [this, new DebuggerError("grammar is invalid")];
        }

        return [this];
    }

    db_created(ctx: GrammarContext) {
        this.emit("db_deleted");
        this.emit("db_created");
    }

    db_deleted(ctx: GrammarContext) {
        this.emit("db_deleted");
    }

    malformed_grammar(ctx: GrammarContext) {
        this.emit("malformed_grammar");
    }
}

class ParseBuilder extends FlowNode<DebuggerData> {
    config: JSParserConfig = new JSParserConfig;
    _updateConfig: any;
    _handleKeyEvents: any;
    _buildParserSignal: any
    optimize: boolean = false;
    parser_valid: boolean = false;

    constructor() {
        super()
        this._buildParserSignal = this.buildParserSignal.bind(this);
        this._updateConfig = this.updateConfig.bind(this);
        this._handleKeyEvents = this.handleKeyEvents.bind(this);
    }

    buildParserSignal(e: Event) {
        this.emit("ParseBuilder_build")
    }

    handleKeyEvents(e: KeyboardEvent) {
        if (e.altKey) {
            switch (e.key) {
                case "b":
                    this.buildParserSignal(e);
                    e.stopImmediatePropagation();
                    e.stopPropagation();
                    e.preventDefault();
                    return false
            }
        }
    }

    updateConfig() {
        this.setConfig();
        this.emit("config_changed");
    }

    private setConfig() {
        this.config.CONTEXT_FREE = DebuggerCheckbox.get("cf-enable").ele.checked;
        this.config.ALLOW_LR = DebuggerCheckbox.get("lr-enable").ele.checked;
        this.config.ALLOW_LOOKAHEAD_MERGE = DebuggerCheckbox.get("la-enable").ele.checked;
        this.config.ALLOW_RECURSIVE_DESCENT = DebuggerCheckbox.get("rd-enable").ele.checked;
        this.config.ALLOW_FORKING = DebuggerCheckbox.get("fk-enable").ele.checked;
        this.config.ALLOW_PEEKING = DebuggerCheckbox.get("pk-enable").ele.checked;
        this.optimize = DebuggerCheckbox.get("op-enable").ele.checked;
    }

    setupInputs() {
        DebuggerCheckbox.get("cf-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("lr-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("la-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("rd-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("fk-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("pk-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("op-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("op-enable").addEventListener("change", this._handleKeyEvents);
        DebuggerButton.get("build").addEventListener("click", this._buildParserSignal);
        window.addEventListener("keydown", this._handleKeyEvents);
    }

    destroyInputs() {
        DebuggerCheckbox.get("op-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("cf-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("lr-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("la-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("rd-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("fk-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("pk-enable").removeEventListener("change", this._updateConfig);
        DebuggerButton.get("build").removeEventListener("click", this._buildParserSignal);
        window.removeEventListener("keydown", this._handleKeyEvents);
        this.config.free();
    }

    update(t: string, data: DebuggerData): FlowNode<DebuggerData>[] {
        switch (t) {
            case "init": {
                this.setupInputs();
                this.setConfig();
                this.parser_valid = false;
                return [this, new DisableBuildButton, new EnableBuildButton, new DebuggerError(
                    `grammar looks good, parser needs to be compiled
                    &nbsp; <i class="fa-solid fa-wrench hover inactive"></i> (alt+b)`
                )];
            };

            case "config_changed": {
                DebuggerField.get("parser-type").ele.value = "";
                DebuggerField.get("num-of-states").ele.value = "";
                data.grammar_ctx.setParserErrors([]);
                this.parser_valid = false;
                return [this, new DisableBuildButton, new EnableBuildButton, new DebuggerError(
                    `configuration changed, parser needs to be compiled
                    &nbsp; <i class="fa-solid fa-wrench hover inactive"></i> (alt+b)`
                )];
            };

            case "db_deleted":
                DebuggerField.get("parser-type").ele.value = "";
                DebuggerField.get("num-of-states").ele.value = "";
                data.grammar_ctx.setParserErrors([]);
                this.destroyInputs();
                return [new DisableBuildButton];


            case "ParseBuilder_build": {
                if (this.parser_valid) {
                    return [this]
                }

                let db = data.grammar_ctx.db;
                if (!db)
                    return [new DebuggerError("Database is invalid")];


                if (data.states) { data.states.free(); data.states = null }
                if (data.bytecode) { data.bytecode.free(); data.bytecode = null }

                let states, bytecode;

                try {
                    states = sherpa.create_parser_states(db, this.optimize, this.config);

                    DebuggerField.get("parser-type").ele.value = states.parser_metrics.classification.get_type();
                    DebuggerField.get("num-of-states").ele.value = states.parser_metrics.num_of_states + "";

                    bytecode = sherpa.create_bytecode(states);
                    data.states = states;
                    data.bytecode = bytecode;
                    this.parser_valid = true;
                    return [this, new LockBuildButton, new TransportHandler, new ClearDebuggerError];
                } catch (e) {
                    if (e instanceof sherpa.PositionedErrors) {
                        let errors = [];
                        for (let i = 0; i < e.length; i++) {
                            let error = e.get_error_at(i);
                            if (error)
                                errors.push(error);
                        }
                        e.free();
                        data.grammar_ctx.setParserErrors(errors);
                        return [this, new DebuggerError(`Failed to compile parser data <<< check grammar errors <<<`)]
                    } else {
                        console.log(e);
                        if (states) states.free();
                        if (bytecode) bytecode.free();
                        return [this, new DebuggerError(`Failed to compile parser data, unexpected error ${e}`)]
                    }
                }

            }
            default:
                return [this]
        }
    }
}

class LockBuildButton extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("build").active = true;
        DebuggerButton.get("build").disable = true;
        return []
    }
}

class DisableBuildButton extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("build").active = false;
        DebuggerButton.get("build").disable = true;
        return []
    }
}

class EnableBuildButton extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("build").active = false;
        DebuggerButton.get("build").disable = false;
        return []
    }
}

class DisableButtons extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        return [new DisableBuildButton, new DisableTransportButtons, new DisableRestartButton]
    }
}

export class DisableTransportButtons extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("step").disable = true;
        DebuggerButton.get("step-action").disable = true;
        DebuggerButton.get("play").disable = true;
        return []
    }
}

export class EnableTransportButtons extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("step").disable = false;
        DebuggerButton.get("step-action").disable = false;
        DebuggerButton.get("play").disable = false;
        return []
    }
}

export class EnableRestartButton extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("restart").disable = false;
        return []
    }
}

export class DisableRestartButton extends FlowNode<DebuggerData> {
    update(t: string, _: DebuggerData) {
        DebuggerButton.get("restart").disable = true;
        return []
    }
}

class Play extends FlowNode<DebuggerData> { }

export function initDebugger(
    grammar_ctx: GrammarContext, parser_host: Element,
    debugger_entry_selection: HTMLSelectElement): DebuggerRoot {

    // Setup default values;

    var default_config = new JSParserConfig();

    DebuggerCheckbox.get("cf-enable").ele.checked = default_config.CONTEXT_FREE;
    DebuggerCheckbox.get("lr-enable").ele.checked = default_config.ALLOW_LR;
    DebuggerCheckbox.get("la-enable").ele.checked = default_config.ALLOW_LOOKAHEAD_MERGE;
    DebuggerCheckbox.get("rd-enable").ele.checked = default_config.ALLOW_RECURSIVE_DESCENT;
    DebuggerCheckbox.get("fk-enable").ele.checked = default_config.ALLOW_FORKING;
    DebuggerCheckbox.get("pk-enable").ele.checked = default_config.ALLOW_PEEKING;

    DebuggerCheckbox.get("fk-enable").disable = true;

    default_config.free();

    return new DebuggerRoot(
        {
            debugger_entry_selection,
            bytecode: null,
            states: null,
            parser_editor: null,
            parser_host,
            grammar_ctx,
            config: new JSParserConfig(),
            PARSER_VALID: false,
            PARSING: false,
        },
        [
            new InitCodeMirror(),
            new DisableButtons(),
            new DebuggerError("Parser needs to be compiled"),
        ]
    );
}