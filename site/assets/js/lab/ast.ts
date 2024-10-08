import { JSBytecodeParserDB, get_nonterminal_name_from_id } from "js/radlr/radlr_wasm";
import { NBContentField, NBEditorField } from "./notebook";
import { Parser } from "./parser";
import { GrammarDBNode, InputNode } from "./pipeline";

import Graph from "graphology";
import Sigma from "sigma";
import { SigmaEdgeEventPayload, SigmaNodeEventPayload } from "sigma/dist/declarations/src/types";


export function init(
  ast_field: NBContentField,
  parser_input_field: NBEditorField,
  grammar_pipeline: GrammarDBNode,
) { }