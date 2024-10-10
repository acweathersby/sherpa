import { JSBytecodeParserDB, get_nonterminal_name_from_id } from "js/radlr/radlr_wasm";
import { NBContentField, NBEditorField } from "./notebook";
import { Parser } from "./parser";
import { GrammarDBNode, InputNode } from "./pipeline";

import Graph from "graphology";
import Sigma from "sigma";
import { SigmaNodeEventPayload } from "sigma/dist/declarations/src/types";

type Hooks = { on_node_enter: ((arg: SigmaNodeEventPayload) => void) | null };

export function init(
  cst_field: NBContentField,
  parser_input_field: NBEditorField,
  grammar_pipeline: GrammarDBNode,
) {

  const graph = new Graph();
  const hooks: Hooks = { on_node_enter: null }

  let ele = document.createElement("div");
  ele.classList.add("ast-graph");
  cst_field.body.appendChild(ele);

  const renderer = new Sigma(
    graph,
    ele,
    {
      allowInvalidContainer: true,
      autoRescale: true,
      autoCenter: true,
      minEdgeThickness: 1,
    }
  );

  renderer.on("enterNode", node => {
    if (hooks.on_node_enter) {
      hooks.on_node_enter(node);
    }
  })

  let input_string: string = "";
  let db: null | JSBytecodeParserDB = null;

  parser_input_field.addListener("text_changed", field => {
    input_string = field.get_text();

    if (!cst_field.is_mini)
      run_ast_render(parser_input_field, input_string, db, renderer, hooks);
  })

  grammar_pipeline.addListener("loading", _ => {
    db = null;
    cst_field.set_loading(true)
  });

  grammar_pipeline.addListener("failed", _ => {
    cst_field.set_loading(false);
  })

  grammar_pipeline.addListener("bytecode_db", new_db => {
    db = new_db;
    cst_field.set_loading(false);

    if (!cst_field.is_mini)
      run_ast_render(parser_input_field, input_string, db, renderer, hooks);
  });
}

function run_ast_render(input_field: NBEditorField, input: string, db: JSBytecodeParserDB | null, renderer: Sigma, hooks: Hooks) {

  if (!input || !db) return;
  const graph = new Graph();

  let parser = new Parser(db, input);

  type Node = { id: number, nodes: Node[], width: number, from: number, to: number };

  let symbols: Node[] = [];
  let node_count = 0;

  let id = 0;

  parser.on_reduce = reduce_data => {
    //if (reduce_data.symbols == 1) return;

    let offset = symbols.length - reduce_data.symbols;
    let r_syms = symbols.splice(offset, reduce_data.symbols);

    let name = get_nonterminal_name_from_id(reduce_data.non_terminal_id, db);

    let from = r_syms[0].from;
    let to = r_syms[r_syms.length - 1].to;

    graph.addNode(id, { label: name, x: 0, y: 0, size: 8, color: "blue" });
    let width = 0;
    for (const sym of r_syms) {
      graph.addEdge(id, sym.id, { size: 1, color: "purple" });
      width += sym.width;
    }

    symbols.push({ id, nodes: r_syms, width, from, to });
    id++;
  };

  parser.on_shift = shift_data => {
    graph.addNode(id, { label: `"${shift_data.token}"`, x: 0, y: 0, size: 8, color: "green" });
    symbols.push({ id, nodes: [], width: 1, from: shift_data.byte_offset, to: shift_data.byte_offset + shift_data.byte_len });
    id++;
  };


  parser.init("default", input);
  parser.play();

  parser.on_reduce = null;
  parser.on_shift = null;

  parser.destroy();

  let positions = new Array(id);

  function map_positions(symbols: Node[], lvl: number = 0, positions: [number, number, number, number][], x: number = 0) {
    let total_width = symbols.reduce((v, n) => v + n.width, 0)
    let pos_x = x - (total_width / 2);

    for (const node of symbols) {

      let half_width = node.width / 2;

      let x = pos_x + half_width;

      if (node.nodes.length > 0) {

        map_positions(node.nodes, lvl + 1, positions, x);

        positions[node.id] = [x, lvl * -1, node.from, node.to];

      } else {
        positions[node.id] = [x, lvl * -1, node.from, node.to];

      }

      pos_x += node.width
    }
  }

  map_positions(symbols, 0, positions);
  hooks.on_node_enter = node => {

    let id = parseInt(node.node);
    let [, , from, to] = positions[id];

    input_field.remove_specific_character_classes("ast-node-target");
    input_field.add_character_class(from, to, "ast-node-target");
  }


  graph.updateEachNodeAttributes((node_id, node) => {
    let [x, y] = positions[<any>node_id];
    node.x = x;
    node.y = y;
    return node
  });


  renderer.setGraph(graph);
}