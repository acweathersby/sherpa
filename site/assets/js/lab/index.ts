import * as pipeline from "./pipeline";
import radlr_init, * as radlr from "js/radlr/radlr_wasm.js";
import { EditorField } from "./journal_elements";

export async function init(compiler_worker_path: string) {
  radlr_init();


  pipeline.GrammarDB.worker_path = compiler_worker_path;

  let grammar_input = new EditorField("Grammar");
  grammar_input.setContentVisible(true);

  let bytecode_output = new EditorField("Bytecode Output");
  bytecode_output.setContentVisible(false);

  let parser_input = new EditorField("Parser Input");
  parser_input.setContentVisible(true);

  let formatting_rules = new EditorField("Formatting Rules");
  let highlighting_rules = new EditorField("Highlighting Rules");
  let ast_atat = new EditorField("Ascript AST @@");

  ast_atat.setText("temp");

  const input = new pipeline.InputNode();
  const grammar = new pipeline.GrammarDB([input],);
  const parser = new pipeline.Parser([grammar]);

  parser_input.setText("name name name")
  parser_input.addHighlight(0, 5, "red");
  parser_input.addMsg(0, 5, "test1");
  parser_input.addMsg(5, 4, "test2");

  grammar_input.setText("<> name > \"names\"+");


  grammar_input.addListener("text_changed", grammar_input => {
    input.update(grammar_input.getText())
  })

  grammar.addListener("loading", _ => {
    bytecode_output.setText("");
    bytecode_output.setContentVisible(false);
    bytecode_output.setLoading(true);
  })

  grammar.addListener("failed", _ => {
    bytecode_output.setContentVisible(false);
    bytecode_output.setLoading(false);
  })

  grammar.addListener("bytecode_ready", async bytecode => {
    bytecode_output.setText(bytecode);

    await pipeline.sleep(500);

    bytecode_output.setLoading(false);
    bytecode_output.setContentVisible(true);
  })


  parser_input.addListener("text_changed", data => {
    parser.restart(data.getText());
  })

  parser.addListener("destroyed", data => {
    // Disable transport controls
    console.log("Parser Destroyed");
  })

  parser.addListener("reset", data => {
    // Enable transport controls
    console.log("Parser Reset");
    parser.step();
  })

  parser.addListener("step", action => {
    console.log("Parser Stepped", action);
  });
}
