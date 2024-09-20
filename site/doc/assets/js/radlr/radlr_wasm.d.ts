/* tslint:disable */
/* eslint-disable */
/**
* @returns {any}
*/
export function get_nonterminal_names(): any;
/**
* Creates an empty grammar soup object.
* Use soup modifiers to add grammars and nonterminals
*
* Pass soup to parser compiler functions to create parsers, generate bytecode,
* and construct ASCript AST and CST structures.
* @returns {JSRadlrGrammar}
*/
export function create_soup(): JSRadlrGrammar;
/**
* Creates a parser db from a soup and a root grammar, or returns semantic
* errors.
* @param {string} grammar_id
* @param {JSRadlrGrammar} soup
* @param {JSParserConfig} config
* @returns {JSParserDB}
*/
export function create_parse_db(grammar_id: string, soup: JSRadlrGrammar, config: JSParserConfig): JSParserDB;
/**
* Temporary simple AST output implementation.
* @param {JSParserDB} js_db
* @returns {string}
*/
export function create_rust_ast_output(js_db: JSParserDB): string;
/**
* Temporary simple AST output implementation.
* @param {JSParserDB} js_db
* @param {boolean} optimize_states
* @param {JSParserConfig} config
* @returns {JSIRParser}
*/
export function create_parser_states(js_db: JSParserDB, optimize_states: boolean, config: JSParserConfig): JSIRParser;
/**
* Temporary simple disassembly implementation.
* @param {JSIRParser} states
* @returns {JSBytecodeParserDB}
*/
export function create_bytecode(states: JSIRParser): JSBytecodeParserDB;
/**
* Temporary simple disassembly implementation.
* @param {JSBytecodeParserDB} pkg
* @returns {string}
*/
export function create_bytecode_disassembly(pkg: JSBytecodeParserDB): string;
/**
* Temporary simple disassembly of a single instruction
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @returns {string}
*/
export function create_instruction_disassembly(address: number, pkg: JSBytecodeParserDB): string;
/**
* Return a list of symbols ids if the opcode of the instruction is
* HashTable or VecTable
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @returns {any}
*/
export function get_debug_symbol_ids(address: number, pkg: JSBytecodeParserDB): any;
/**
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @param {JSParserDB} db
* @returns {any}
*/
export function get_debug_state_name(address: number, pkg: JSBytecodeParserDB, db: JSParserDB): any;
/**
* Return a list of symbols ids if the opcode of the instruction is
* Op::DebugExpectedSymbols
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @returns {any}
*/
export function get_debug_tok_offsets(address: number, pkg: JSBytecodeParserDB): any;
/**
* @param {string} name
* @param {JSIRParser} states
* @returns {any}
*/
export function get_state_source_string(name: string, states: JSIRParser): any;
/**
* Givin an symbol index, returns the symbol's friendly name.
* @param {number} id
* @param {JSParserDB} db
* @returns {any}
*/
export function get_symbol_name_from_id(id: number, db: JSParserDB): any;
/**
* Returns a list of entrypoint names
* @param {JSParserDB} db
* @returns {any}
*/
export function get_entry_names(db: JSParserDB): any;
/**
* @param {number} id
* @param {JSParserDB} db
* @returns {string}
*/
export function get_nonterminal_name_from_id(id: number, db: JSParserDB): string;
/**
* @param {JSParserDB} db
* @returns {any}
*/
export function get_nonterminal_names_from_db(db: JSParserDB): any;
/**
* Returns the offset and length of a token rule.
* @param {number} rule_id
* @param {JSParserDB} db
* @returns {any}
*/
export function get_rule_location(rule_id: number, db: JSParserDB): any;
/**
* Returns a diagram of a grammar rule  
* @param {number} rule_id
* @param {JSParserDB} db
* @returns {string}
*/
export function get_rule_expression_string(rule_id: number, db: JSParserDB): string;
/**
* @param {number} id
* @param {JSParserDB} db
* @returns {JSReductionType}
*/
export function get_rule_reduce_type(id: number, db: JSParserDB): JSReductionType;
/**
*/
export enum JSDebugEvent {
  ExecuteState = 0,
  ExecuteInstruction = 1,
  Skip = 2,
  Shift = 3,
  Reduce = 4,
  Complete = 5,
  Error = 6,
  EndOfFile = 7,
  Undefined = 8,
}
/**
*/
export enum JSReductionType {
/**
* Any reduction resulting in the execution of a some kind of semantic
* action. At this point only `:ast` semantic actions are available.
*/
  SemanticAction = 0,
/**
* A reduction of a terminal symbol to a nonterminal
*/
  SingleTerminal = 1,
/**
* A reduction of single nonterminal symbol to another nonterminal
*/
  SingleNonTerminal = 2,
/**
* A reduction of a left-recursive rule
*/
  LeftRecursive = 3,
/**
* A reduction of more than one symbol to a nonterminal
*/
  Mixed = 4,
}
/**
*/
export class EditGraph {
  free(): void;
/**
* @param {string} input
* @param {JSBytecodeParserDB} db
*/
  constructor(input: string, db: JSBytecodeParserDB);
/**
* @param {JSCSTNode} node
* @param {number} offset
* @param {string} text
* @returns {JSPatchResult | undefined}
*/
  patch_insert(node: JSCSTNode, offset: number, text: string): JSPatchResult | undefined;
/**
* @param {JSCSTNode} node
* @param {number} offset
* @param {number} len
* @returns {JSPatchResult | undefined}
*/
  patch_remove(node: JSCSTNode, offset: number, len: number): JSPatchResult | undefined;
/**
* @param {JSCSTNode} node
* @param {number} offset
* @returns {Uint32Array | undefined}
*/
  get_offset(node: JSCSTNode, offset: number): Uint32Array | undefined;
/**
* @returns {JSCSTNode | undefined}
*/
  get_root(): JSCSTNode | undefined;
/**
* @param {JSCSTNode} par
* @param {JSCSTNode} child
* @param {number} offset
* @returns {JSCSTNode}
*/
  add_child(par: JSCSTNode, child: JSCSTNode, offset: number): JSCSTNode;
/**
* @param {JSCSTNode} par
* @param {number} offset
* @returns {JSCSTNode}
*/
  remove_child(par: JSCSTNode, offset: number): JSCSTNode;
}
/**
* Am iterable parser
*/
export class JSByteCodeParser {
  free(): void;
/**
* @param {string} input
* @param {JSBytecodeParserDB} bytecode
* @returns {JSByteCodeParser}
*/
  static new(input: string, bytecode: JSBytecodeParserDB): JSByteCodeParser;
/**
* @param {string} entry_name
*/
  init(entry_name: string): void;
/**
* @returns {any}
*/
  next(): any;
}
/**
* Bytecode produced from parse states
*/
export class JSBytecodeParserDB {
  free(): void;
/**
* Returns the bytecode of the parser as bytes.
*/
  readonly bytecode: Uint8Array;
/**
* A list of enterble non-terminal names and their respective bytecode entry
* point address address
*/
  readonly entry_points: any;
}
/**
*/
export class JSCSTNode {
  free(): void;
/**
* @returns {string}
*/
  get_type(): string;
/**
* @param {EditGraph} graph
* @returns {string}
*/
  get_text(graph: EditGraph): string;
/**
* @param {number} index
* @returns {JSCSTNode | undefined}
*/
  child_at(index: number): JSCSTNode | undefined;
/**
* @returns {number}
*/
  num_of_children(): number;
/**
* @returns {number}
*/
  len(): number;
}
/**
*/
export class JSCTXState {
  free(): void;
/**
*/
  anchor_ptr: number;
/**
*/
  begin_ptr: number;
/**
*/
  end_ptr: number;
/**
*/
  input_ptr: number;
/**
*/
  is_scanner: boolean;
/**
*/
  sym_len: number;
/**
*/
  sym_ptr: number;
/**
*/
  tok_id: number;
/**
*/
  tok_len: number;
}
/**
*/
export class JSDebugPacket {
  free(): void;
/**
*/
  complete: number;
/**
*/
  ctx: JSCTXState;
/**
*/
  event: JSDebugEvent;
/**
*/
  instruction: number;
/**
*/
  is_scanner: boolean;
/**
*/
  nonterminal_id: number;
/**
*/
  offset_end: number;
/**
*/
  offset_start: number;
/**
*/
  rule_id: number;
/**
*/
  symbol_count: number;
}
/**
* A Grammar Identity
*/
export class JSGrammarIdentities {
  free(): void;
}
/**
* Parser states generated from the compilation of parser db
*/
export class JSIRParser {
  free(): void;
/**
*/
  readonly classification: JSParserClassification;
/**
*/
  readonly num_of_states: any;
/**
*/
  readonly optimized: any;
}
/**
*/
export class JSParserClassification {
  free(): void;
/**
* @returns {string}
*/
  get_type(): string;
/**
*/
  bottom_up: boolean;
/**
* If set to true, then the parser has at least one state that jumps to the
* head state of a specific non-terminal
*/
  calls_present: boolean;
/**
* If set to true, the parser has at least one state that forks the parse
* tree, and performs parsing on separate alternatives in parallel
*/
  forks_present: boolean;
/**
* If set to true then the parser has at least one state that transitions on
* non-terminals as well terminals.
*/
  gotos_present: boolean;
/**
* Maximum peek level used to disambiguate conflicting phrases. If this is
* equal to `u16::MAX`, then peeking failed or a fork was used in its place.
*/
  max_k: number;
/**
* If set to true, the parser has at least one state that performs k>1
* lookaheads before selecting an appropriate alternative action.
*/
  peeks_present: boolean;
}
/**
*/
export class JSParserConfig {
  free(): void;
/**
*/
  constructor();
/**
* @returns {JSParserConfig}
*/
  static cst_editor(): JSParserConfig;
/**
* When enable, recursive descent style `Call` states will be generated
*/
  ALLOW_CALLS: boolean;
/**
* Allow the parser to split its context to handle ambiguous rules. This may
* lead to a CSF (Concrete Syntax Forest) or a CSDAG (Concrete Syntax DAG)
* being returned by the parser instead of a CST
*/
  ALLOW_CONTEXT_SPLITTING: boolean;
/**
* Allow the parser to shift on CST non-term nodes.
*/
  ALLOW_CST_NONTERM_SHIFT: boolean;
/**
* When enable, LR style states can be produced, in general
* allowing more advanced grammar constructs to be parsed, such
* as left recursive rules.
*
* When disabled, grammars with rules that require LR style parse states
* will be rejected, and relevant errors will be reported.
*/
  ALLOW_LR: boolean;
/**
* When enabled, unrestricted lookahead states states will be generated
*
* When disabled, grammars with rules that require a lookahead that is
*  `k>1` will be rejected, and relevant errors will be reported.
*/
  ALLOW_PEEKING: boolean;
/**
* Creates states that directly handle transitions on terminals, allowing the
* creation of parsers that can patch existing CST structures.
*/
  AllOW_CST_MERGING: boolean;
/**
* Creates a single scanner instead of multiple contextual scanners. More
* likely to report terminal conflicts.
*/
  CONTEXT_FREE: boolean;
/**
* Makes entry points for all non-terminals defined in the grammar.
*/
  EXPORT_ALL_NONTERMS: boolean;
/**
* The maximum number of lookead symbols allowed before parser construction
* is aborted or a different disambiguating strategy is employed.
*/
  max_k: number;
}
/**
* A Parser database derived from grammar defined in a JSSoup
*/
export class JSParserDB {
  free(): void;
}
/**
* A Parser database derived from grammar defined in a JSSoup
*/
export class JSParserGraph {
  free(): void;
}
/**
*/
export class JSParserMetrics {
  free(): void;
/**
*/
  classification: JSParserClassification;
/**
*/
  num_of_states: number;
/**
*/
  optimized: boolean;
}
/**
*/
export class JSPatchResult {
  free(): void;
/**
* @param {number} index
* @returns {JSCSTNode | undefined}
*/
  node_at(index: number): JSCSTNode | undefined;
/**
* @returns {number}
*/
  num_of_nodes(): number;
}
/**
* An arbitrary collection of grammars
*/
export class JSRadlrGrammar {
  free(): void;
/**
* Adds or replaces grammar in the soup, or throws an error
* if the grammar is invalid. Returns the grammar
* id if successful.
* @param {string} grammar_source
* @param {string} path
* @returns {JSGrammarIdentities}
*/
  add_grammar(grammar_source: string, path: string): JSGrammarIdentities;
/**
* Adds a non-terminal targeting a specific grammar
* @param {string} _grammar_name
*/
  add_nonterminal(_grammar_name: string): void;
}
/**
*/
export class JSRadlrSourceError {
  free(): void;
/**
*/
  col: number;
/**
*/
  end_offset: number;
/**
*/
  len: number;
/**
*/
  line: number;
/**
*/
  readonly message: string;
/**
*/
  start_offset: number;
}
/**
*/
export class PositionedErrors {
  free(): void;
/**
* @param {number} index
* @returns {JSRadlrSourceError | undefined}
*/
  get_error_at(index: number): JSRadlrSourceError | undefined;
/**
*/
  readonly length: number;
}
/**
*/
export class TokenOffsets {
  free(): void;
/**
*/
  end: number;
/**
*/
  start: number;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_jsbytecodeparser_free: (a: number, b: number) => void;
  readonly jsbytecodeparser_new: (a: number, b: number, c: number) => number;
  readonly jsbytecodeparser_init: (a: number, b: number, c: number) => void;
  readonly jsbytecodeparser_next: (a: number) => number;
  readonly get_nonterminal_names: () => number;
  readonly __wbg_jsctxstate_free: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_is_scanner: (a: number) => number;
  readonly __wbg_set_jsctxstate_is_scanner: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_end_ptr: (a: number) => number;
  readonly __wbg_set_jsctxstate_end_ptr: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_input_ptr: (a: number) => number;
  readonly __wbg_set_jsctxstate_input_ptr: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_sym_ptr: (a: number) => number;
  readonly __wbg_set_jsctxstate_sym_ptr: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_begin_ptr: (a: number) => number;
  readonly __wbg_set_jsctxstate_begin_ptr: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_anchor_ptr: (a: number) => number;
  readonly __wbg_set_jsctxstate_anchor_ptr: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_tok_len: (a: number) => number;
  readonly __wbg_set_jsctxstate_tok_len: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_tok_id: (a: number) => number;
  readonly __wbg_set_jsctxstate_tok_id: (a: number, b: number) => void;
  readonly __wbg_get_jsctxstate_sym_len: (a: number) => number;
  readonly __wbg_set_jsctxstate_sym_len: (a: number, b: number) => void;
  readonly __wbg_jsdebugpacket_free: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_event: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_event: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_ctx: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_ctx: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_instruction: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_instruction: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_offset_start: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_offset_start: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_offset_end: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_offset_end: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_nonterminal_id: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_nonterminal_id: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_rule_id: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_rule_id: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_symbol_count: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_symbol_count: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_complete: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_complete: (a: number, b: number) => void;
  readonly __wbg_get_jsdebugpacket_is_scanner: (a: number) => number;
  readonly __wbg_set_jsdebugpacket_is_scanner: (a: number, b: number) => void;
  readonly __wbg_jsradlrsourceerror_free: (a: number, b: number) => void;
  readonly __wbg_get_jsradlrsourceerror_line: (a: number) => number;
  readonly __wbg_set_jsradlrsourceerror_line: (a: number, b: number) => void;
  readonly __wbg_get_jsradlrsourceerror_col: (a: number) => number;
  readonly __wbg_set_jsradlrsourceerror_col: (a: number, b: number) => void;
  readonly __wbg_get_jsradlrsourceerror_len: (a: number) => number;
  readonly __wbg_set_jsradlrsourceerror_len: (a: number, b: number) => void;
  readonly __wbg_get_jsradlrsourceerror_start_offset: (a: number) => number;
  readonly __wbg_set_jsradlrsourceerror_start_offset: (a: number, b: number) => void;
  readonly __wbg_get_jsradlrsourceerror_end_offset: (a: number) => number;
  readonly __wbg_set_jsradlrsourceerror_end_offset: (a: number, b: number) => void;
  readonly jsradlrsourceerror_message: (a: number, b: number) => void;
  readonly __wbg_positionederrors_free: (a: number, b: number) => void;
  readonly positionederrors_length: (a: number) => number;
  readonly positionederrors_get_error_at: (a: number, b: number) => number;
  readonly __wbg_editgraph_free: (a: number, b: number) => void;
  readonly editgraph_new: (a: number, b: number, c: number) => number;
  readonly editgraph_patch_insert: (a: number, b: number, c: number, d: number, e: number) => number;
  readonly editgraph_patch_remove: (a: number, b: number, c: number, d: number) => number;
  readonly editgraph_get_offset: (a: number, b: number, c: number) => number;
  readonly editgraph_get_root: (a: number) => number;
  readonly editgraph_add_child: (a: number, b: number, c: number, d: number) => number;
  readonly editgraph_remove_child: (a: number, b: number, c: number) => number;
  readonly __wbg_jscstnode_free: (a: number, b: number) => void;
  readonly jscstnode_get_type: (a: number, b: number) => void;
  readonly jscstnode_get_text: (a: number, b: number, c: number) => void;
  readonly jscstnode_child_at: (a: number, b: number) => number;
  readonly jscstnode_num_of_children: (a: number) => number;
  readonly jscstnode_len: (a: number) => number;
  readonly __wbg_jspatchresult_free: (a: number, b: number) => void;
  readonly jspatchresult_node_at: (a: number, b: number) => number;
  readonly jspatchresult_num_of_nodes: (a: number) => number;
  readonly __wbg_jsparserconfig_free: (a: number, b: number) => void;
  readonly jsparserconfig_new: () => number;
  readonly jsparserconfig_cst_editor: () => number;
  readonly jsparserconfig_ALLOW_CALLS: (a: number) => number;
  readonly jsparserconfig_set_ALLOW_CALLS: (a: number, b: number) => void;
  readonly jsparserconfig_ALLOW_LR: (a: number) => number;
  readonly jsparserconfig_set_ALLOW_LR: (a: number, b: number) => void;
  readonly jsparserconfig_ALLOW_PEEKING: (a: number) => number;
  readonly jsparserconfig_set_ALLOW_PEEKING: (a: number, b: number) => void;
  readonly jsparserconfig_max_k: (a: number) => number;
  readonly jsparserconfig_set_max_k: (a: number, b: number) => void;
  readonly jsparserconfig_ALLOW_CONTEXT_SPLITTING: (a: number) => number;
  readonly jsparserconfig_set_ALLOW_CONTEXT_SPLITTING: (a: number, b: number) => void;
  readonly jsparserconfig_CONTEXT_FREE: (a: number) => number;
  readonly jsparserconfig_set_CONTEXT_FREE: (a: number, b: number) => void;
  readonly jsparserconfig_AllOW_CST_MERGING: (a: number) => number;
  readonly jsparserconfig_set_AllOW_CST_MERGING: (a: number, b: number) => void;
  readonly jsparserconfig_ALLOW_CST_NONTERM_SHIFT: (a: number) => number;
  readonly jsparserconfig_set_ALLOW_CST_NONTERM_SHIFT: (a: number, b: number) => void;
  readonly jsparserconfig_EXPORT_ALL_NONTERMS: (a: number) => number;
  readonly jsparserconfig_set_EXPORT_ALL_NONTERMS: (a: number, b: number) => void;
  readonly __wbg_jsparserclassification_free: (a: number, b: number) => void;
  readonly __wbg_get_jsparserclassification_bottom_up: (a: number) => number;
  readonly __wbg_set_jsparserclassification_bottom_up: (a: number, b: number) => void;
  readonly __wbg_get_jsparserclassification_max_k: (a: number) => number;
  readonly __wbg_set_jsparserclassification_max_k: (a: number, b: number) => void;
  readonly __wbg_get_jsparserclassification_gotos_present: (a: number) => number;
  readonly __wbg_set_jsparserclassification_gotos_present: (a: number, b: number) => void;
  readonly __wbg_get_jsparserclassification_calls_present: (a: number) => number;
  readonly __wbg_set_jsparserclassification_calls_present: (a: number, b: number) => void;
  readonly __wbg_get_jsparserclassification_peeks_present: (a: number) => number;
  readonly __wbg_set_jsparserclassification_peeks_present: (a: number, b: number) => void;
  readonly __wbg_get_jsparserclassification_forks_present: (a: number) => number;
  readonly __wbg_set_jsparserclassification_forks_present: (a: number, b: number) => void;
  readonly jsparserclassification_get_type: (a: number, b: number) => void;
  readonly __wbg_jsparsermetrics_free: (a: number, b: number) => void;
  readonly __wbg_get_jsparsermetrics_classification: (a: number) => number;
  readonly __wbg_set_jsparsermetrics_classification: (a: number, b: number) => void;
  readonly __wbg_get_jsparsermetrics_num_of_states: (a: number) => number;
  readonly __wbg_set_jsparsermetrics_num_of_states: (a: number, b: number) => void;
  readonly __wbg_get_jsparsermetrics_optimized: (a: number) => number;
  readonly __wbg_set_jsparsermetrics_optimized: (a: number, b: number) => void;
  readonly __wbg_jsgrammaridentities_free: (a: number, b: number) => void;
  readonly __wbg_jsparserdb_free: (a: number, b: number) => void;
  readonly __wbg_jsparsergraph_free: (a: number, b: number) => void;
  readonly __wbg_jsirparser_free: (a: number, b: number) => void;
  readonly jsirparser_classification: (a: number) => number;
  readonly jsirparser_num_of_states: (a: number) => number;
  readonly jsirparser_optimized: (a: number) => number;
  readonly __wbg_jsradlrgrammar_free: (a: number, b: number) => void;
  readonly __wbg_jsbytecodeparserdb_free: (a: number, b: number) => void;
  readonly jsbytecodeparserdb_bytecode: (a: number) => number;
  readonly jsbytecodeparserdb_entry_points: (a: number) => number;
  readonly jsradlrgrammar_add_grammar: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
  readonly jsradlrgrammar_add_nonterminal: (a: number, b: number, c: number, d: number) => void;
  readonly create_soup: (a: number) => void;
  readonly create_parse_db: (a: number, b: number, c: number, d: number, e: number) => void;
  readonly create_rust_ast_output: (a: number, b: number) => void;
  readonly create_parser_states: (a: number, b: number, c: number, d: number) => void;
  readonly create_bytecode: (a: number, b: number) => void;
  readonly create_bytecode_disassembly: (a: number, b: number) => void;
  readonly create_instruction_disassembly: (a: number, b: number, c: number) => void;
  readonly get_debug_symbol_ids: (a: number, b: number) => number;
  readonly get_debug_state_name: (a: number, b: number, c: number) => number;
  readonly __wbg_tokenoffsets_free: (a: number, b: number) => void;
  readonly __wbg_get_tokenoffsets_start: (a: number) => number;
  readonly __wbg_set_tokenoffsets_start: (a: number, b: number) => void;
  readonly __wbg_get_tokenoffsets_end: (a: number) => number;
  readonly __wbg_set_tokenoffsets_end: (a: number, b: number) => void;
  readonly get_debug_tok_offsets: (a: number, b: number) => number;
  readonly get_state_source_string: (a: number, b: number, c: number) => number;
  readonly get_symbol_name_from_id: (a: number, b: number) => number;
  readonly get_entry_names: (a: number) => number;
  readonly get_nonterminal_name_from_id: (a: number, b: number, c: number) => void;
  readonly get_nonterminal_names_from_db: (a: number) => number;
  readonly get_rule_location: (a: number, b: number) => number;
  readonly get_rule_expression_string: (a: number, b: number, c: number) => void;
  readonly get_rule_reduce_type: (a: number, b: number) => number;
  readonly radlr_free_stack: (a: number, b: number) => void;
  readonly radlr_get_token_class_from_codepoint: (a: number) => number;
  readonly radlr_allocate_stack: (a: number) => number;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
*
* @returns {InitOutput}
*/
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
