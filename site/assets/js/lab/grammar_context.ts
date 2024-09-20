import * as radlr from "js/radlr/radlr_wasm.js";

export const enum EventType {
  DBDeleted = "db-deleted",
  DBCreated = "db-created",
  NewErrors = "parser-errors",
  GrammarAdded = "grammar-added",
  MalformedGrammar = "grammar-malformed"
}

/// Maintains a general context that tracks the state of the grammar
/// and its derivatives
export class GrammarContext {

  private soup_: radlr.JSSoup;

  private db_: radlr.JSParserDB | null;

  public listeners: Map<EventType, Set<{ (ctx: GrammarContext): void }>>;

  private pending_events: EventType[];

  private RUNNING_EVENTS: boolean;

  private current_parse_errors: radlr.JSRadlrSourceError[];

  private current_db_errors: radlr.JSRadlrSourceError[];

  private current_parser_errors: radlr.JSRadlrSourceError[];

  /// Should only be called after radlr is initialized
  constructor() {
    this.soup_ = radlr.create_soup();
    this.listeners = new Map();
    this.pending_events = [];
    this.RUNNING_EVENTS = false;
    this.db_ = null;
    this.current_parse_errors = [];
    this.current_parser_errors = [];
    this.current_db_errors = [];
  }

  get soup(): radlr.JSSoup {
    return this.soup_;
  }

  get db(): radlr.JSParserDB | null {
    return this.db_;
  }

  get parse_errors(): radlr.JSRadlrSourceError[] {
    return this.current_parse_errors;
  }

  get db_errors(): radlr.JSRadlrSourceError[] {
    return this.current_db_errors;
  }

  get parser_errors(): radlr.JSRadlrSourceError[] {
    return this.current_parser_errors;
  }

  get ast_errors(): radlr.JSRadlrSourceError[] {
    return []
  }

  private clearErrors() {
    this.clearParseErrors();
    this.clearParserErrors();
    this.clearDBErrors();
  }

  private clearParseErrors() {
    this.current_parse_errors.map(e => e.free());
    this.current_parse_errors.length = 0;
  }

  private clearDBErrors() {
    this.current_db_errors.map(e => e.free());
    this.current_db_errors.length = 0;
  }

  private clearParserErrors() {
    this.current_parser_errors.map(e => e.free());
    this.current_parser_errors.length = 0;
  }

  public addGrammar(input_string: string, grammar_name: string, config: radlr.JSParserConfig) {


    this.clearErrors();

    try {
      this.soup.add_grammar(input_string, grammar_name);
      this.signal(EventType.GrammarAdded);
    } catch (e) {
      if (e instanceof radlr.PositionedErrors) {
        for (let i = 0; i < e.length; i++) {
          let error = e.get_error_at(i);
          if (error)
            this.current_parse_errors.push(error);
        }
        e.free();
        this.signal(EventType.MalformedGrammar);
        this.signal(EventType.NewErrors);
      } else {
        console.log(e)
      }
      return;
    }

    this.createDB("/", config);
  }


  createDB(grammar_name: string, config: radlr.JSParserConfig): boolean {
    if (this.db_) {
      try {
        this.signal(EventType.DBDeleted, false);
      } catch (e) {
        console.error(e);
      }
      this.db_.free();
      this.db_ = null;
    }

    try {
      this.db_ = radlr.create_parse_db(grammar_name, this.soup_, config);
      this.signal(EventType.DBCreated, false);
      return true;
    } catch (e) {
      if (e instanceof radlr.PositionedErrors) {
        for (let i = 0; i < e.length; i++) {
          let error = e.get_error_at(i);
          if (error)
            this.current_db_errors.push(error);
        }
        e.free();
        this.signal(EventType.NewErrors);
      } else {
        console.error(e)
      }
    }
    return false;
  }

  public setParserErrors(e: radlr.JSRadlrSourceError[] | radlr.JSRadlrSourceError) {
    this.clearParserErrors();

    this.current_parser_errors.length = 0;
    if (Array.isArray(e)) {
      this.current_parser_errors.push(...e)
    } else {
      this.current_parser_errors.push(e)
    }
    this.signal(EventType.NewErrors);
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