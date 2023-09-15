import { JSParserConfig } from "js/sherpa/sherpa_wasm";
import { DebuggerButton, DebuggerCheckbox } from "./debugger_buttons";
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

export class DebuggerError extends FlowNode<DebuggerData> {
    message: string

    constructor(message: string) {
        super();
        this.message = message
    }

    update(t: string, data: DebuggerData) {
        alert(this.message);
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
                data.grammar_ctx.addListener(GrammarEventType.DBDeleted, this.db_created.bind(this));
            } break;
            case "db_created": {
                if (data.grammar_ctx.db) {
                    this.configure_entry_options(data.grammar_ctx.db, data.debugger_entry_selection)
                    return [this, new ParseBuilder]
                } else {
                    return [this, new DebuggerError("Grammar Database is invalid")];
                }
            };
            case "db_deleted": return [this];
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
}

class ParseBuilder extends FlowNode<DebuggerData> {
    buttonListener: any
    config: JSParserConfig = new JSParserConfig;
    _updateConfig: any;
    optimize: boolean = false;

    constructor() {
        super()
        this.buttonListener = this.buildParserSignal.bind(this);
        this._updateConfig = this.updateConfig.bind(this);
    }

    setupInputs() {
        DebuggerCheckbox.get("cf-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("lr-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("la-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("rd-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("fk-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("pk-enable").addEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("op-enable").addEventListener("change", this._updateConfig);
        DebuggerButton.get("build").addEventListener("click", this.buttonListener);
    }

    destroyInputs() {
        DebuggerCheckbox.get("op-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("cf-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("lr-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("la-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("rd-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("fk-enable").removeEventListener("change", this._updateConfig);
        DebuggerCheckbox.get("pk-enable").removeEventListener("change", this._updateConfig);
        DebuggerButton.get("build").removeEventListener("click", this.buttonListener);
        this.config.free();
    }

    updateConfig() {
        this.config.CONTEXT_FREE = DebuggerCheckbox.get("cf-enable").ele.checked;
        this.config.ALLOW_LR = DebuggerCheckbox.get("lr-enable").ele.checked;
        this.config.ALLOW_LOOKAHEAD_MERGE = DebuggerCheckbox.get("la-enable").ele.checked;
        this.config.ALLOW_RECURSIVE_DESCENT = DebuggerCheckbox.get("rd-enable").ele.checked;
        this.config.ALLOW_FORKING = DebuggerCheckbox.get("fk-enable").ele.checked;
        this.config.ALLOW_FORKING = DebuggerCheckbox.get("pk-enable").ele.checked;
        this.optimize = DebuggerCheckbox.get("op-enable").ele.checked;
        this.emit("config_changed");
    }

    update(t: string, data: DebuggerData): FlowNode<DebuggerData>[] {
        switch (t) {
            case "init": {
                this.setupInputs();
                this.updateConfig();
                return [this];
            };

            case "config_changed": {
                return [this, new DisableBuildButton, new EnableBuildButton];
            };

            case "db_deleted":
                this.destroyInputs();
                return [new DisableBuildButton];


            case "ParseBuilder_build": {
                let db = data.grammar_ctx.db;
                if (!db)
                    return [new DebuggerError("Database is invalid")];

                if (data.states) data.states.free();
                if (data.bytecode) data.bytecode.free();

                let states, bytecode;

                try {
                    states = sherpa.create_parser_states(db, this.optimize);
                    bytecode = sherpa.create_bytecode(states);
                    data.states = states;
                    data.bytecode = bytecode;
                } catch (e) {
                    if (states) states.free();
                    if (bytecode) bytecode.free();
                    return [new DebuggerError(`Failed to compile parser data ${e}`)]
                }

                return [this, new LockBuildButton, new TransportHandler];
            }
            default:
                return [this]
        }
    }

    buildParserSignal(e: Event) {
        this.emit("ParseBuilder_build")
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
    DebuggerCheckbox.get("pk-enable").ele.checked = default_config.ALLOW_FORKING;

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
        ]
    );
}