import "./settings-panel";
import "./config-panel";
import * as pipeline from "./pipeline";
import radlr_init, * as radlr from "js/radlr/radlr_wasm.js";
import { NB, NBContentField, NBEditorField, NBField } from "./notebook";
import { Controls } from "./control";
import { LocalStoreKeys, getLocalValue, localStorageEnabled, setLocalValue } from "./settings-panel";
import { setupConfig } from "./config-panel";

const DefaultGrammar =
  /**/
  `IGNORE { c:sp }  
  
<> entry > "Hello" "World"
`.trim();

const DefaultParserInput = "Hello World";

export async function init(compiler_worker_path: string) {
  let rad_init = radlr_init();

  let nb = new NB(2);

  let grammar_input = nb.addField(new NBEditorField("Grammar"))
  grammar_input.setContentVisible(true);
  grammar_input.setText(DefaultGrammar);

  let bytecode_output = nb.addField(new NBEditorField("Bytecode Output"), 1);
  bytecode_output.setContentVisible(false);

  pipeline.GrammarDB.worker_path = compiler_worker_path;

  let parser_input = nb.addField(new NBEditorField("Parser Input"), 1);
  parser_input.setContentVisible(true);


  let ast = nb.addField(new NBContentField("AST Nodes"), 1);
  {
    let parent = ast.body;

    let canvas = document.createElement("canvas");
    canvas.classList.add("field-canvas");
    let ctx = canvas.getContext("2d");

    parent.appendChild(canvas);
  }

  let cst = nb.addField(new NBContentField("CST Nodes"), 1);

  let formatting_rules = new NBEditorField("Formatting Rules");
  let highlighting_rules = new NBEditorField("Highlighting Rules");


  const controls = new Controls();

  const input = new pipeline.InputNode();
  const config_input = new pipeline.ConfigNode();
  const grammar = new pipeline.GrammarDB([input, config_input],);
  const parser = new pipeline.Parser([grammar]);

  parser_input.setText(DefaultParserInput)


  grammar_input.addListener("text_changed", grammar_input => {
    let text = grammar_input.getText();
    if (text) {
      setLocalValue(LocalStoreKeys.GrammarInput, text);
      input.update(text)
      grammar_input.removeHighlight();
      grammar_input.removeMsgs();
    }
  })

  parser_input.addListener("text_changed", parser_input => {
    let text = parser_input.getText();
    if (text) {
      setLocalValue(LocalStoreKeys.ParseInput, text);
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
        grammar_input.addHighlight(error.start_offset, error.end_offset, "red");
        grammar_input.addMsg(error.start_offset, error.end_offset, error.msg);
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
  })

  parser.addListener("destroyed", data => {
    // Disable transport controls
    console.log("Parser Destroyed");
    controls.setActive(false);
  })

  parser.addListener("parser-created", data => {
    // Enable transport controls
    controls.setActive(true);
  })


  parser.addListener("reset", data => {
    // Enable transport controls
    console.log("Parser Reset");
    parser.step();
  })

  parser.addListener("step", action => {
    console.log("Parser Stepped", action);
  });

  await pipeline.sleep(10);

  nb.calculateHeights();

  var text = getLocalValue(LocalStoreKeys.ParseInput);
  if (text) {
    parser_input.setText(text);
  }

  var text = getLocalValue(LocalStoreKeys.GrammarInput) || DefaultGrammar;
  if (text) {
    grammar_input.setText(text);
    input.update(text)
  }

  await rad_init;

  setupConfig(config => {
    config_input.update(config);
  });

}
