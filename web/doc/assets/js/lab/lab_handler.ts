import { basicSetup } from "codemirror";
import { Container, Thing, ButtonThing, InputThing, CMThing } from "../common/layout"
import { DebuggerButton } from "./debugger/debugger_io";
import { get_grammar } from "js/common/session_storage";
import init, * as radlr from "js/radlr/radlr_wasm.js";
import { CSTNodeRoot } from "./ast_editor/node";

export async function lab_handler(div: HTMLDivElement) {
  await init();


  let root_ctr = new Container("root");
  root_ctr.direction = "horizontal"
  root_ctr.attach(div);

  let inputs = new Container("inputs");
  let outputs = new Container("outputs");

  inputs.attach(root_ctr);
  outputs.attach(root_ctr);

  let button_input = new ButtonThing("input-switch", "", false);
  button_input.io.innerText = "Test";
  button_input.attach(inputs);

  let grammar_editor = new CMThing("grammar-editor", "grammar-editor", get_grammar(), [
    basicSetup
  ])

  grammar_editor.attach(inputs);

  let lsp_base_test = new Thing("div", "lsp-test", "LSP TEST", false);

  lsp_base_test.attach(outputs);

  let grammar = radlr.create_soup();
  grammar.add_grammar(`

  IGNORE { c:sp }
  <> B > A(+) ";"

  <> A > "Hello" "{" tk:(c:id+)(+) "}" 
  
  
  `, "/");
  let config = radlr.JSParserConfig.cst_editor();
  let db = radlr.create_parse_db("/", grammar, config);
  let ir = radlr.create_parser_states(db, true, config);
  let parser = radlr.create_bytecode(ir);

  let root_node = new CSTNodeRoot(parser);


  // create a base entry for an editor
  root_node.attach(lsp_base_test.ele)
}