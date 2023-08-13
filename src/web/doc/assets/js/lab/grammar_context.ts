import * as sherpa from "js/sherpa/sherpa_wasm.js";

export const enum EventType {
    DBDeleted = "db-deleted",
    DBCreated = "db-created",
    GrammarAdded = "grammar-added",
}

/// Maintains a general context that tracks the state of the grammar
/// and its derivatives
export class GrammarContext {

    private soup_: sherpa.JSSoup;

    private db_: sherpa.JSParserDB | null;

    public listeners: Map<EventType, Set<{ (ctx: GrammarContext): void }>>;

    private pending_events: EventType[];

    private RUNNING_EVENTS: boolean;

    private current_parse_errors: sherpa.JSSherpaSourceError[];
    private current_db_errors: sherpa.JSSherpaSourceError[];

    /// Should only be called after sherpa is initialized
    constructor() {
        this.soup_ = sherpa.create_soup();
        this.listeners = new Map();
        this.pending_events = [];
        this.RUNNING_EVENTS = false;
        this.db_ = null;
        this.current_parse_errors = [];
        this.current_db_errors = [];
    }

    get soup(): sherpa.JSSoup {
        return this.soup_;
    }

    get db(): sherpa.JSParserDB | null {
        return this.db_;
    }

    get parse_errors(): sherpa.JSSherpaSourceError[] {
        return this.current_parse_errors;
    }

    get db_errors(): sherpa.JSSherpaSourceError[] {
        return this.current_db_errors;
    }

    public addGrammar(input_string: string, grammar_name: string) {

        this.current_parse_errors.map(e => e.free());
        this.current_parse_errors.length = 0;

        try {
            this.soup.add_grammar(input_string, grammar_name);
            this.signal(EventType.GrammarAdded);
        } catch (e) {
            if (e instanceof sherpa.PositionedErrors) {
                for (let i = 0; i < e.length; i++) {
                    let error = e.get_error_at(i);
                    if (error)
                        this.current_parse_errors.push(error);
                }
                e.free();
            } else {
                console.log(e)
            }
        }
    }

    public createDB(grammar_name: string): boolean {
        if (this.db_) {
            this.db_.free();
            this.db_ = null;
            this.signal(EventType.DBDeleted, false);
        }

        this.current_db_errors.map(e => e.free());
        this.current_db_errors.length = 0;

        try {
            this.db_ = sherpa.create_parse_db(grammar_name, this.soup_);
            this.signal(EventType.DBCreated, false);
            return true;
        } catch (e) {
            if (e instanceof sherpa.PositionedErrors) {
                for (let i = 0; i < e.length; i++) {
                    let error = e.get_error_at(i);
                    if (error)
                        this.current_db_errors.push(error);
                }
                e.free();
            } else {
                console.log(e)
            }
        }
        return false;
    }


    public addListener(event: EventType, fn: { (ctx: GrammarContext): void }) {

        if (!this.listeners.has(event))
            this.listeners.set(event, new Set);

        this.listeners.get(event)?.add(fn);
    }

    private signal(event?: EventType, process_rest: boolean = true) {
        if (event)
            this.pending_events.push(event);

        if (this.RUNNING_EVENTS)
            return;

        this.RUNNING_EVENTS = true

        let e;

        while ((e = this.pending_events.shift())) {
            let listeners = this.listeners.get(e) ?? [];

            for (const listener of listeners) {
                listener(this);
            }

            if (!process_rest) break;
        }

        this.RUNNING_EVENTS = false;
    }
}