// worker.js

import radlr_init, * as radlr from "js/radlr/radlr_wasm";
import { RadlrError } from "./error";
import { createConnection } from "./lab_mode_client";




let grammar_db: radlr.JSParserDB | null = null;
let states: radlr.JSIRParser | null = null;
let connection = createConnection();

// Handle incoming messages
self.addEventListener('message', async function (event) {

  const { type, eventData, eventId } = event.data;

  if (type === "init") {
    try {
      await radlr_init()
    } catch (e) {
      console.log(e);
    }

    postMessage({ type: "ready" });

  } else if (type === "compile_grammar") {

    let { grammar, config } = <{ grammar: string, config: any }>eventData;

    let soup = null, cfg: radlr.JSParserConfig | null = null;

    try {

      if (grammar_db) {
        grammar_db.free(); grammar_db = null;
      }

      if (states) {
        states.free(); states = null;
      }

      cfg = radlr.JSParserConfig.import(config);

      if (await connection.is_valid()) {
        await connection.build_grammar(grammar, cfg);
      }

      console.log(cfg)

      let optimize = true;

      soup = radlr.create_soup();

      soup.add_grammar(grammar, "main");
      grammar_db = radlr.create_parse_db("main", soup, cfg);

      postMessage({ type: "grammar_ready" });
      states = radlr.create_parser_states(grammar_db, optimize, cfg);

      postMessage({ type: "states_ready", classification: states.classification.to_string() });

      let bytecode_db_export = radlr.export_bytecode_db(states);

      postMessage({ type: "parser_compiled", bytecode_db_export }, { transfer: [bytecode_db_export] });

    } catch (e) {
      if (e instanceof radlr.PositionedErrors) {
        let l = e.length;
        let error;
        let errors: RadlrError[] = [];

        for (let i = 0; i < l; i++) {
          if (error = new RadlrError(e.get_error_at(i))) {
            errors.push(error)
          }
        }

        e.free();

        postMessage({ type: "grammar_compile_errors", errors }, { transfer: [] });
      } else {
        console.error(e);
        throw e;
      }
    } finally {
      try {

        if (grammar_db) {
          grammar_db.free(); grammar_db = null;
        }

        if (states) {
          states.free(); states = null;
        }

        if (soup) {
          soup.free();
        }

        if (cfg) {
          cfg.free();
        }
      } catch (e) {
        console.error(e)
      }
    }
  }

}, false);

// Test -- Conne