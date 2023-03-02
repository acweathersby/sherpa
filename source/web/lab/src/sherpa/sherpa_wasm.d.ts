/* tslint:disable */
/* eslint-disable */
/**
* Compiles a sherpa grammar from a string value.
*
* Returns an error if the `grammar` argument cannot be cast to a string.
* @param {any} grammar
* @returns {JournalWrap}
*/
export function compile_grammar(grammar: any): JournalWrap;
/**
* A Grammar context created after the parsing of an
* input value.
*
* May contain errors and thus be invalid for further
* processing.
*/
export class JournalWrap {
  free(): void;
/**
* Returns `true` if the internal Grammar
* is free of critical errors.
* @returns {boolean}
*/
  is_valid(): boolean;
/**
* @param {boolean} optimize
*/
  compile_states(optimize: boolean): void;
/**
* @param {boolean} optimize
*/
  compile_bytecode(optimize: boolean): void;
/**
* @returns {any}
*/
  generate_disassembly(): any;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly compile_grammar: (a: number, b: number) => void;
  readonly __wbg_journalwrap_free: (a: number) => void;
  readonly journalwrap_is_valid: (a: number) => number;
  readonly journalwrap_compile_states: (a: number, b: number) => void;
  readonly journalwrap_compile_bytecode: (a: number, b: number) => void;
  readonly journalwrap_generate_disassembly: (a: number, b: number) => void;
  readonly sherpa_free_stack: (a: number, b: number) => void;
  readonly sherpa_get_token_class_from_codepoint: (a: number) => number;
  readonly sherpa_allocate_stack: (a: number) => number;
  readonly __wbindgen_malloc: (a: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number) => number;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
