import "./settings-panel";
import * as pipeline from "./pipeline";
import radlr_init, * as radlr from "js/radlr/radlr_wasm.js";
import { NB, NBEditorField } from "./notebook";

export async function init(compiler_worker_path: string) {
  radlr_init();

  let nb = new NB(2);

  let grammar_input = nb.addField(new NBEditorField("Grammar"))
  grammar_input.setContentVisible(true);
  grammar_input.setText("<> name > \"names\"+");

  let grammar_input2 = nb.addField(new NBEditorField("Grammar"))
  grammar_input2.setContentVisible(true);
  grammar_input2.setText("<> name > \"names\"+");

  let bytecode_output = nb.addField(new NBEditorField("Bytecode Output"), 1);
  bytecode_output.setContentVisible(false);

  let bytecode_output2 = nb.addField(new NBEditorField("Bytecode Output"), 1);
  bytecode_output2.setContentVisible(false);


  pipeline.GrammarDB.worker_path = compiler_worker_path;

  let parser_input = nb.addField(new NBEditorField("Parser Input"), 1);
  parser_input.setContentVisible(true);

  let formatting_rules = new NBEditorField("Formatting Rules");
  let highlighting_rules = new NBEditorField("Highlighting Rules");
  let ast_atat = new NBEditorField("Ascript AST @@");

  ast_atat.setText("temp");

  const input = new pipeline.InputNode();
  const grammar = new pipeline.GrammarDB([input],);
  const parser = new pipeline.Parser([grammar]);

  parser_input.setText("name name name")
  parser_input.addHighlight(0, 5, "red");
  parser_input.addMsg(0, 5, "test1");
  parser_input.addMsg(5, 4, "test2");



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
    await pipeline.sleep(100);
    bytecode_output.setText(bytecode);
    await pipeline.sleep(600);
    bytecode_output.setLoading(false);
    await pipeline.sleep(600);
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

  await pipeline.sleep(10);

  nb.calculateHeights()
}
