import { basicSetup } from "codemirror";
import { Container, Thing, ButtonThing, InputThing, CMThing } from "../common/layout"
import { DebuggerButton } from "./debugger/debugger_io";
import { get_grammar } from "js/common/session_storage";
import init, * as sherpa from "js/sherpa/sherpa_wasm.js";

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
  console.log(DebuggerButton.buttons)

  let lsp_base_test = new Thing("div", "lsp-test", "LSP TEST", false);

  lsp_base_test.io.contentEditable = "true";

  lsp_base_test.attach(outputs);

  let grammar = sherpa.create_soup();
  grammar.add_grammar(`

  IGNORE { c:sp }
  <> B > A(+) ";"

  <> A > "Hello " "{ " tk:(c:id+)(+) " }" 
  
  
  `, "/");
  let db = sherpa.create_parse_db("/", grammar);
  let config = new sherpa.JSParserConfig;
  let ir = sherpa.create_parser_states(db, true, config);
  let parser = sherpa.create_bytecode(ir);
  let graph = sherpa.EditGraph.new(sherpa.LSPSystem.new(parser));

  graph.parse("Hello { world }");

  lsp_base_test.io.innerText = graph.to_string();

  lsp_base_test.io.addEventListener("input", e => {
    let selections = window.getSelection();
    if (selections) {
      let selection_offset = selections.anchorOffset;
      console.log({ selection_offset })
    }
  });


}