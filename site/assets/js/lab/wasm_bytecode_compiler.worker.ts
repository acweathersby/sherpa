// worker.js

import radlr_init, * as radlr from "js/radlr/radlr_wasm";


let grammar_db: radlr.JSParserDB | null = null;
let states: radlr.JSIRParser | null = null;

// Handle incoming messages
self.addEventListener('message', async function (event) {

  const { type, eventData, eventId } = event.data;

  if (type === "init") {
    console.log("Initializing radlr");
    try {
      await radlr_init()
    } catch (e) {
      console.log(e);
    }

    console.log("Worker initialized");

    this.self.postMessage({ type: "ready" });

  } else if (type === "compile_grammar") {

    let soup = null, config: radlr.JSParserConfig | null = null;

    try {

      if (grammar_db) {
        grammar_db.free(); grammar_db = null;
      }

      if (states) {
        states.free(); states = null;
      }

      config = radlr.JSParserConfig.cst_editor();

      let optimize = true;


      soup = radlr.create_soup();;
      soup.add_grammar(eventData.grammar, "main");

      grammar_db = await radlr.create_parse_db("main", soup, config);

      this.self.postMessage({ type: "grammar_ready" });

      states = radlr.create_parser_states(grammar_db, optimize, config);

      this.self.postMessage({ type: "states_ready" });

      let bytecode_db_export = radlr.export_bytecode_db(states);

      this.self.postMessage({ type: "parser_compiled", bytecode_db_export }, { transfer: [bytecode_db_export] });

    } catch (e) {
      console.log(e);
    } finally {
      if (soup) {
        soup.free();
      }
      if (config) {
        config.free();
      }
    }

    // Creates a grammar from the givin input data and stores the resultant data 
    // within the worker
  } else if (type === "build_bytecode") {
    // Takes the resident grammar and state data and generates a bytecode packet that 
    // can be run on the host.
  }

}, false);