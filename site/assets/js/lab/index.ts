import "./settings-panel";
import "./config-panel";
import * as pipeline from "./pipeline";
import radlr_init, * as radlr from "js/radlr/radlr_wasm.js";
import { NB, NBContentField, NBEditorField, NBField } from "./notebook";
import { Controls } from "./control";
import { LocalStoreKeys, getLocalValue, dataStorageWorkflowsEnabled, setLocalValue } from "./settings-panel";
import { setupConfig } from "./config-panel";
import { Debounce } from "./debounce";
import { CSTView } from "./cst";

const DefaultGrammar =
  /**/
  `IGNORE { c:sp }  
  
<> entry > "Hello" "World"
`.trim();

const DefaultParserInput = "Hello World";

export async function init(compiler_worker_path: string) {
  let rad_init = radlr_init();

  let nb = new NB(2);

  let grammar_input_field = nb.addField(new NBEditorField("Grammar"))
  grammar_input_field.setContentVisible(true);
  grammar_input_field.setText(DefaultGrammar);

  let bytecode_output = nb.addField(new NBEditorField("Bytecode Output"), 1);
  bytecode_output.setContentVisible(false);

  pipeline.GrammarDB.worker_path = compiler_worker_path;

  let parser_input_field = nb.addField(new NBEditorField("Parser Input"), 1);
  parser_input_field.setContentVisible(true);




  let ast = nb.addField(new NBContentField("AST Nodes"), 1);
  {
    let parent = ast.body;

    let canvas = document.createElement("canvas");
    canvas.classList.add("field-canvas");
    let ctx = canvas.getContext("2d");

    parent.appendChild(canvas);
  }

  let cst = nb.addField(new NBContentField("CST Nodes"), 1);
  let cst_view = new CSTView(cst)

  let formatting_rules = new NBEditorField("Formatting Rules");
  let highlighting_rules = new NBEditorField("Highlighting Rules");


  const controls = new Controls();

  const grammar_input = new pipeline.InputNode();
  const parser_input = new pipeline.InputNode();
  const config_input = new pipeline.ConfigNode();
  const grammar = new pipeline.GrammarDB([grammar_input, config_input],);
  const parser = new pipeline.Parser([grammar, parser_input]);

  parser_input_field.setText(DefaultParserInput)


  let grammar_input_debounce = new Debounce(() => {
    let text = grammar_input_field.getText();
    if (text) {
      setLocalValue(LocalStoreKeys.GrammarInput, text);
      grammar_input.update(text)
      grammar_input_field.removeHighlight();
      grammar_input_field.removeMsgs();
    }
  });

  let parser_input_debounce = new Debounce(() => {
    let text = parser_input_field.getText();
    if (text) {
      setLocalValue(LocalStoreKeys.ParserInput, text);
      parser_input.update(text)
      parser_input_field.removeHighlight();
      parser_input_field.removeMsgs();
    }
  });

  grammar_input_field.addListener("text_changed", grammar_input => {
    grammar_input_debounce.call();
  })

  parser_input_field.addListener("text_changed", grammar_input => {
    parser_input_debounce.call();
  })

  parser_input_field.addListener("text_changed", parser_input => {
    let text = parser_input.getText();
    if (text) {
      setLocalValue(LocalStoreKeys.ParserInput, text);
      //parser.restart(text);
    }
  })

  let error_reporter = <HTMLDivElement>document.getElementById("error-reporter");
  let grammar_classification = <HTMLDivElement>document.getElementById("controls")?.querySelector(".classification");

  grammar.addListener("loading", _ => {
    bytecode_output.setText("");
    bytecode_output.setContentVisible(false);
    bytecode_output.setLoading(true);

    error_reporter.innerText = "";
    grammar_classification.innerHTML = "";
  })

  grammar.addListener("failed", errors => {
    for (const error of errors) {
      if (error.origin == radlr.ErrorOrigin.Grammar) {
        alert(error.msg);
        error_reporter.innerText = (`Unhandled Error: \n${radlr.ErrorOrigin[error.origin]}\n${error.msg}\n${error.line}:${error.col}`);
        grammar_input_field.addHighlight(error.start_offset, error.end_offset, "red");
        grammar_input_field.addMsg(error.start_offset, error.end_offset, error.msg);
      } else {
        alert(error.msg);
      }

    }
    bytecode_output.setContentVisible(false);
    bytecode_output.setLoading(false);
    controls.setActive(false);
  })

  grammar.addListener("bytecode_ready", async bytecode => {
    await pipeline.sleep(100);
    bytecode_output.setText(bytecode);
    await pipeline.sleep(100);
    bytecode_output.setLoading(false);
    await pipeline.sleep(500);
    bytecode_output.setContentVisible(true);
  })

  grammar.addListener("grammar-classification", classification => {
    grammar_classification.innerHTML = classification;
    controls.setActive(true);
  })

  parser.addListener("destroyed", data => {
    // Disable transport controls
    console.log("Parser Destroyed");
    controls.setActive(false);
  })

  parser.addListener("reset", data => {
    // Enable transport controls
    console.log("Parser Reset");
    cst_view.reset();
    controls.setActive(true);
  })

  parser.addListener("complete", action => {
    console.log("complete")
  });

  parser.addListener("eof", action => {
    console.log("eof")
  });

  parser.addListener("error", action => {
    console.log("error")
  });

  parser.addListener("execute_instruction", debug_info => {
    let ctx = debug_info.ctx;

    parser_input_field.removeCharacterClasses();
    parser_input_field.addCharacterClass(ctx.input_ptr, ctx.input_ptr + 1, "dbg-input-pos");
    parser_input_field.addCharacterClass(ctx.anchor_ptr, ctx.anchor_ptr + 1, "dbg-anchor-pos");
    parser_input_field.addCharacterClass(ctx.begin_ptr, ctx.begin_ptr + 1, "dbg-begin-pos");
    parser_input_field.addCharacterClass(ctx.end_ptr, ctx.end_ptr + 1, "dbg-end-pos");

    if (debug_info.is_scanner) {
      parser_input_field.addCharacterClass(ctx.sym_ptr, ctx.sym_ptr + 1, "dbg-sym-pos");
      parser_input_field.addCharacterClass(ctx.sym_ptr, ctx.input_ptr, "dbg-sym");
    } else if (ctx.sym_len > 0) {
      parser_input_field.addCharacterClass(ctx.sym_ptr, ctx.sym_ptr + ctx.sym_len, "dbg-sym");
    }


  });

  parser.addListener("reduce", reduce => {
    cst_view.handle_reduce(reduce);
  })

  parser.addListener("shift", shift => {
    cst_view.handle_shift(shift);
  })

  controls.addListener("step", () => {
    parser.step();
  });

  controls.addListener("reset", () => {
    parser_input_field.removeCharacterClasses();
    parser.reset();
  });

  controls.addListener("play", () => {
    console.log("playing parser");
  });

  await pipeline.sleep(10);

  nb.calculateHeights();

  var text = getLocalValue(LocalStoreKeys.ParserInput);
  if (text) {
    parser_input_field.setText(text);
  }

  var text = getLocalValue(LocalStoreKeys.GrammarInput) || DefaultGrammar;
  if (text) {
    grammar_input_field.setText(text);
    grammar_input.update(text)
  }

  await rad_init;

  setupConfig(config => {
    config_input.update(config);
  });

}

