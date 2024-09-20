let wasm;

const heap = new Array(128).fill(undefined);

heap.push(undefined, null, true, false);

function getObject(idx) { return heap[idx]; }

let heap_next = heap.length;

function addHeapObject(obj) {
    if (heap_next === heap.length) heap.push(heap.length + 1);
    const idx = heap_next;
    heap_next = heap[idx];

    if (typeof(heap_next) !== 'number') throw new Error('corrupt heap');

    heap[idx] = obj;
    return idx;
}

const cachedTextDecoder = (typeof TextDecoder !== 'undefined' ? new TextDecoder('utf-8', { ignoreBOM: true, fatal: true }) : { decode: () => { throw Error('TextDecoder not available') } } );

if (typeof TextDecoder !== 'undefined') { cachedTextDecoder.decode(); };

let cachedUint8ArrayMemory0 = null;

function getUint8ArrayMemory0() {
    if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
        cachedUint8ArrayMemory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachedUint8ArrayMemory0;
}

function getStringFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
}

function debugString(val) {
    // primitive types
    const type = typeof val;
    if (type == 'number' || type == 'boolean' || val == null) {
        return  `${val}`;
    }
    if (type == 'string') {
        return `"${val}"`;
    }
    if (type == 'symbol') {
        const description = val.description;
        if (description == null) {
            return 'Symbol';
        } else {
            return `Symbol(${description})`;
        }
    }
    if (type == 'function') {
        const name = val.name;
        if (typeof name == 'string' && name.length > 0) {
            return `Function(${name})`;
        } else {
            return 'Function';
        }
    }
    // objects
    if (Array.isArray(val)) {
        const length = val.length;
        let debug = '[';
        if (length > 0) {
            debug += debugString(val[0]);
        }
        for(let i = 1; i < length; i++) {
            debug += ', ' + debugString(val[i]);
        }
        debug += ']';
        return debug;
    }
    // Test for built-in
    const builtInMatches = /\[object ([^\]]+)\]/.exec(toString.call(val));
    let className;
    if (builtInMatches.length > 1) {
        className = builtInMatches[1];
    } else {
        // Failed to match the standard '[object ClassName]'
        return toString.call(val);
    }
    if (className == 'Object') {
        // we're a user defined class or Object
        // JSON.stringify avoids problems with cycles, and is generally much
        // easier than looping through ownProperties of `val`.
        try {
            return 'Object(' + JSON.stringify(val) + ')';
        } catch (_) {
            return 'Object';
        }
    }
    // errors
    if (val instanceof Error) {
        return `${val.name}: ${val.message}\n${val.stack}`;
    }
    // TODO we could test for more things here, like `Set`s and `Map`s.
    return className;
}

let WASM_VECTOR_LEN = 0;

const cachedTextEncoder = (typeof TextEncoder !== 'undefined' ? new TextEncoder('utf-8') : { encode: () => { throw Error('TextEncoder not available') } } );

const encodeString = (typeof cachedTextEncoder.encodeInto === 'function'
    ? function (arg, view) {
    return cachedTextEncoder.encodeInto(arg, view);
}
    : function (arg, view) {
    const buf = cachedTextEncoder.encode(arg);
    view.set(buf);
    return {
        read: arg.length,
        written: buf.length
    };
});

function passStringToWasm0(arg, malloc, realloc) {

    if (typeof(arg) !== 'string') throw new Error(`expected a string argument, found ${typeof(arg)}`);

    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length, 1) >>> 0;
        getUint8ArrayMemory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len, 1) >>> 0;

    const mem = getUint8ArrayMemory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }

    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
        const view = getUint8ArrayMemory0().subarray(ptr + offset, ptr + len);
        const ret = encodeString(arg, view);
        if (ret.read !== arg.length) throw new Error('failed to pass whole string');
        offset += ret.written;
        ptr = realloc(ptr, len, offset, 1) >>> 0;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

let cachedDataViewMemory0 = null;

function getDataViewMemory0() {
    if (cachedDataViewMemory0 === null || cachedDataViewMemory0.buffer.detached === true || (cachedDataViewMemory0.buffer.detached === undefined && cachedDataViewMemory0.buffer !== wasm.memory.buffer)) {
        cachedDataViewMemory0 = new DataView(wasm.memory.buffer);
    }
    return cachedDataViewMemory0;
}

function dropObject(idx) {
    if (idx < 132) return;
    heap[idx] = heap_next;
    heap_next = idx;
}

function takeObject(idx) {
    const ret = getObject(idx);
    dropObject(idx);
    return ret;
}

function _assertClass(instance, klass) {
    if (!(instance instanceof klass)) {
        throw new Error(`expected instance of ${klass.name}`);
    }
    return instance.ptr;
}

function _assertNum(n) {
    if (typeof(n) !== 'number') throw new Error(`expected a number argument, found ${typeof(n)}`);
}
/**
* @returns {any}
*/
export function get_nonterminal_names() {
    const ret = wasm.get_nonterminal_names();
    return takeObject(ret);
}

function _assertBoolean(n) {
    if (typeof(n) !== 'boolean') {
        throw new Error(`expected a boolean argument, found ${typeof(n)}`);
    }
}

function logError(f, args) {
    try {
        return f.apply(this, args);
    } catch (e) {
        let error = (function () {
            try {
                return e instanceof Error ? `${e.message}\n\nStack:\n${e.stack}` : e.toString();
            } catch(_) {
                return "<failed to stringify thrown value>";
            }
        }());
        console.error("wasm-bindgen: imported JS function that was not marked as `catch` threw an error:", error);
        throw e;
    }
}
/**
* Creates an empty grammar soup object.
* Use soup modifiers to add grammars and nonterminals
*
* Pass soup to parser compiler functions to create parsers, generate bytecode,
* and construct ASCript AST and CST structures.
* @returns {JSRadlrGrammar}
*/
export function create_soup() {
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        wasm.create_soup(retptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
        if (r2) {
            throw takeObject(r1);
        }
        return JSRadlrGrammar.__wrap(r0);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
    }
}

/**
* Creates a parser db from a soup and a root grammar, or returns semantic
* errors.
* @param {string} grammar_id
* @param {JSRadlrGrammar} soup
* @param {JSParserConfig} config
* @returns {JSParserDB}
*/
export function create_parse_db(grammar_id, soup, config) {
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        const ptr0 = passStringToWasm0(grammar_id, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        _assertClass(soup, JSRadlrGrammar);
        if (soup.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertClass(config, JSParserConfig);
        if (config.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.create_parse_db(retptr, ptr0, len0, soup.__wbg_ptr, config.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
        if (r2) {
            throw takeObject(r1);
        }
        return JSParserDB.__wrap(r0);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
    }
}

/**
* Temporary simple AST output implementation.
* @param {JSParserDB} js_db
* @returns {string}
*/
export function create_rust_ast_output(js_db) {
    let deferred2_0;
    let deferred2_1;
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertClass(js_db, JSParserDB);
        if (js_db.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.create_rust_ast_output(retptr, js_db.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
        var r3 = getDataViewMemory0().getInt32(retptr + 4 * 3, true);
        var ptr1 = r0;
        var len1 = r1;
        if (r3) {
            ptr1 = 0; len1 = 0;
            throw takeObject(r2);
        }
        deferred2_0 = ptr1;
        deferred2_1 = len1;
        return getStringFromWasm0(ptr1, len1);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
        wasm.__wbindgen_free(deferred2_0, deferred2_1, 1);
    }
}

/**
* Temporary simple AST output implementation.
* @param {JSParserDB} js_db
* @param {boolean} optimize_states
* @param {JSParserConfig} config
* @returns {JSIRParser}
*/
export function create_parser_states(js_db, optimize_states, config) {
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertClass(js_db, JSParserDB);
        if (js_db.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertBoolean(optimize_states);
        _assertClass(config, JSParserConfig);
        if (config.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.create_parser_states(retptr, js_db.__wbg_ptr, optimize_states, config.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
        if (r2) {
            throw takeObject(r1);
        }
        return JSIRParser.__wrap(r0);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
    }
}

/**
* Temporary simple disassembly implementation.
* @param {JSIRParser} states
* @returns {JSBytecodeParserDB}
*/
export function create_bytecode(states) {
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertClass(states, JSIRParser);
        if (states.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.create_bytecode(retptr, states.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
        if (r2) {
            throw takeObject(r1);
        }
        return JSBytecodeParserDB.__wrap(r0);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
    }
}

/**
* Temporary simple disassembly implementation.
* @param {JSBytecodeParserDB} pkg
* @returns {string}
*/
export function create_bytecode_disassembly(pkg) {
    let deferred2_0;
    let deferred2_1;
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertClass(pkg, JSBytecodeParserDB);
        if (pkg.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.create_bytecode_disassembly(retptr, pkg.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
        var r3 = getDataViewMemory0().getInt32(retptr + 4 * 3, true);
        var ptr1 = r0;
        var len1 = r1;
        if (r3) {
            ptr1 = 0; len1 = 0;
            throw takeObject(r2);
        }
        deferred2_0 = ptr1;
        deferred2_1 = len1;
        return getStringFromWasm0(ptr1, len1);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
        wasm.__wbindgen_free(deferred2_0, deferred2_1, 1);
    }
}

/**
* Temporary simple disassembly of a single instruction
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @returns {string}
*/
export function create_instruction_disassembly(address, pkg) {
    let deferred1_0;
    let deferred1_1;
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertNum(address);
        _assertClass(pkg, JSBytecodeParserDB);
        if (pkg.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.create_instruction_disassembly(retptr, address, pkg.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        deferred1_0 = r0;
        deferred1_1 = r1;
        return getStringFromWasm0(r0, r1);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
* Return a list of symbols ids if the opcode of the instruction is
* HashTable or VecTable
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @returns {any}
*/
export function get_debug_symbol_ids(address, pkg) {
    _assertNum(address);
    _assertClass(pkg, JSBytecodeParserDB);
    if (pkg.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_debug_symbol_ids(address, pkg.__wbg_ptr);
    return takeObject(ret);
}

/**
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @param {JSParserDB} db
* @returns {any}
*/
export function get_debug_state_name(address, pkg, db) {
    _assertNum(address);
    _assertClass(pkg, JSBytecodeParserDB);
    if (pkg.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    _assertClass(db, JSParserDB);
    if (db.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_debug_state_name(address, pkg.__wbg_ptr, db.__wbg_ptr);
    return takeObject(ret);
}

/**
* Return a list of symbols ids if the opcode of the instruction is
* Op::DebugExpectedSymbols
* @param {number} address
* @param {JSBytecodeParserDB} pkg
* @returns {any}
*/
export function get_debug_tok_offsets(address, pkg) {
    _assertNum(address);
    _assertClass(pkg, JSBytecodeParserDB);
    if (pkg.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_debug_tok_offsets(address, pkg.__wbg_ptr);
    return takeObject(ret);
}

/**
* @param {string} name
* @param {JSIRParser} states
* @returns {any}
*/
export function get_state_source_string(name, states) {
    const ptr0 = passStringToWasm0(name, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    const len0 = WASM_VECTOR_LEN;
    _assertClass(states, JSIRParser);
    if (states.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_state_source_string(ptr0, len0, states.__wbg_ptr);
    return takeObject(ret);
}

/**
* Givin an symbol index, returns the symbol's friendly name.
* @param {number} id
* @param {JSParserDB} db
* @returns {any}
*/
export function get_symbol_name_from_id(id, db) {
    _assertNum(id);
    _assertClass(db, JSParserDB);
    if (db.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_symbol_name_from_id(id, db.__wbg_ptr);
    return takeObject(ret);
}

/**
* Returns a list of entrypoint names
* @param {JSParserDB} db
* @returns {any}
*/
export function get_entry_names(db) {
    _assertClass(db, JSParserDB);
    if (db.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_entry_names(db.__wbg_ptr);
    return takeObject(ret);
}

/**
* @param {number} id
* @param {JSParserDB} db
* @returns {string}
*/
export function get_nonterminal_name_from_id(id, db) {
    let deferred1_0;
    let deferred1_1;
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertNum(id);
        _assertClass(db, JSParserDB);
        if (db.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.get_nonterminal_name_from_id(retptr, id, db.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        deferred1_0 = r0;
        deferred1_1 = r1;
        return getStringFromWasm0(r0, r1);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
* @param {JSParserDB} db
* @returns {any}
*/
export function get_nonterminal_names_from_db(db) {
    _assertClass(db, JSParserDB);
    if (db.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_nonterminal_names_from_db(db.__wbg_ptr);
    return takeObject(ret);
}

/**
* Returns the offset and length of a token rule.
* @param {number} rule_id
* @param {JSParserDB} db
* @returns {any}
*/
export function get_rule_location(rule_id, db) {
    _assertNum(rule_id);
    _assertClass(db, JSParserDB);
    if (db.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_rule_location(rule_id, db.__wbg_ptr);
    return takeObject(ret);
}

/**
* Returns a diagram of a grammar rule
* @param {number} rule_id
* @param {JSParserDB} db
* @returns {string}
*/
export function get_rule_expression_string(rule_id, db) {
    let deferred1_0;
    let deferred1_1;
    try {
        const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
        _assertNum(rule_id);
        _assertClass(db, JSParserDB);
        if (db.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        wasm.get_rule_expression_string(retptr, rule_id, db.__wbg_ptr);
        var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
        var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
        deferred1_0 = r0;
        deferred1_1 = r1;
        return getStringFromWasm0(r0, r1);
    } finally {
        wasm.__wbindgen_add_to_stack_pointer(16);
        wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
    }
}

/**
* @param {number} id
* @param {JSParserDB} db
* @returns {JSReductionType}
*/
export function get_rule_reduce_type(id, db) {
    _assertNum(id);
    _assertClass(db, JSParserDB);
    if (db.__wbg_ptr === 0) {
        throw new Error('Attempt to use a moved value');
    }
    const ret = wasm.get_rule_reduce_type(id, db.__wbg_ptr);
    return ret;
}

/**
*/
export const JSDebugEvent = Object.freeze({ ExecuteState:0,"0":"ExecuteState",ExecuteInstruction:1,"1":"ExecuteInstruction",Skip:2,"2":"Skip",Shift:3,"3":"Shift",Reduce:4,"4":"Reduce",Complete:5,"5":"Complete",Error:6,"6":"Error",EndOfFile:7,"7":"EndOfFile",Undefined:8,"8":"Undefined", });
/**
*/
export const JSReductionType = Object.freeze({
/**
* Any reduction resulting in the execution of a some kind of semantic
* action. At this point only `:ast` semantic actions are available.
*/
SemanticAction:0,"0":"SemanticAction",
/**
* A reduction of a terminal symbol to a nonterminal
*/
SingleTerminal:1,"1":"SingleTerminal",
/**
* A reduction of single nonterminal symbol to another nonterminal
*/
SingleNonTerminal:2,"2":"SingleNonTerminal",
/**
* A reduction of a left-recursive rule
*/
LeftRecursive:3,"3":"LeftRecursive",
/**
* A reduction of more than one symbol to a nonterminal
*/
Mixed:4,"4":"Mixed", });

const EditGraphFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_editgraph_free(ptr >>> 0, 1));
/**
*/
export class EditGraph {

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        EditGraphFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_editgraph_free(ptr, 0);
    }
    /**
    * @param {string} input
    * @param {JSBytecodeParserDB} db
    */
    constructor(input, db) {
        const ptr0 = passStringToWasm0(input, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        _assertClass(db, JSBytecodeParserDB);
        if (db.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        const ret = wasm.editgraph_new(ptr0, len0, db.__wbg_ptr);
        this.__wbg_ptr = ret >>> 0;
        EditGraphFinalization.register(this, this.__wbg_ptr, this);
        return this;
    }
    /**
    * @param {JSCSTNode} node
    * @param {number} offset
    * @param {string} text
    * @returns {JSPatchResult | undefined}
    */
    patch_insert(node, offset, text) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(node, JSCSTNode);
        if (node.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertNum(offset);
        const ptr0 = passStringToWasm0(text, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        const ret = wasm.editgraph_patch_insert(this.__wbg_ptr, node.__wbg_ptr, offset, ptr0, len0);
        return ret === 0 ? undefined : JSPatchResult.__wrap(ret);
    }
    /**
    * @param {JSCSTNode} node
    * @param {number} offset
    * @param {number} len
    * @returns {JSPatchResult | undefined}
    */
    patch_remove(node, offset, len) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(node, JSCSTNode);
        if (node.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertNum(offset);
        _assertNum(len);
        const ret = wasm.editgraph_patch_remove(this.__wbg_ptr, node.__wbg_ptr, offset, len);
        return ret === 0 ? undefined : JSPatchResult.__wrap(ret);
    }
    /**
    * @param {JSCSTNode} node
    * @param {number} offset
    * @returns {Uint32Array | undefined}
    */
    get_offset(node, offset) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(node, JSCSTNode);
        if (node.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertNum(offset);
        const ret = wasm.editgraph_get_offset(this.__wbg_ptr, node.__wbg_ptr, offset);
        return takeObject(ret);
    }
    /**
    * @returns {JSCSTNode | undefined}
    */
    get_root() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.editgraph_get_root(this.__wbg_ptr);
        return ret === 0 ? undefined : JSCSTNode.__wrap(ret);
    }
    /**
    * @param {JSCSTNode} par
    * @param {JSCSTNode} child
    * @param {number} offset
    * @returns {JSCSTNode}
    */
    add_child(par, child, offset) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(par, JSCSTNode);
        if (par.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertClass(child, JSCSTNode);
        if (child.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertNum(offset);
        const ret = wasm.editgraph_add_child(this.__wbg_ptr, par.__wbg_ptr, child.__wbg_ptr, offset);
        return JSCSTNode.__wrap(ret);
    }
    /**
    * @param {JSCSTNode} par
    * @param {number} offset
    * @returns {JSCSTNode}
    */
    remove_child(par, offset) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(par, JSCSTNode);
        if (par.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        _assertNum(offset);
        const ret = wasm.editgraph_remove_child(this.__wbg_ptr, par.__wbg_ptr, offset);
        return JSCSTNode.__wrap(ret);
    }
}

const JSByteCodeParserFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsbytecodeparser_free(ptr >>> 0, 1));
/**
* Am iterable parser
*/
export class JSByteCodeParser {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSByteCodeParser.prototype);
        obj.__wbg_ptr = ptr;
        JSByteCodeParserFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSByteCodeParserFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsbytecodeparser_free(ptr, 0);
    }
    /**
    * @param {string} input
    * @param {JSBytecodeParserDB} bytecode
    * @returns {JSByteCodeParser}
    */
    static new(input, bytecode) {
        const ptr0 = passStringToWasm0(input, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        _assertClass(bytecode, JSBytecodeParserDB);
        if (bytecode.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        const ret = wasm.jsbytecodeparser_new(ptr0, len0, bytecode.__wbg_ptr);
        return JSByteCodeParser.__wrap(ret);
    }
    /**
    * @param {string} entry_name
    */
    init(entry_name) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ptr0 = passStringToWasm0(entry_name, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len0 = WASM_VECTOR_LEN;
        wasm.jsbytecodeparser_init(this.__wbg_ptr, ptr0, len0);
    }
    /**
    * @returns {any}
    */
    next() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsbytecodeparser_next(this.__wbg_ptr);
        return takeObject(ret);
    }
}

const JSBytecodeParserDBFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsbytecodeparserdb_free(ptr >>> 0, 1));
/**
* Bytecode produced from parse states
*/
export class JSBytecodeParserDB {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSBytecodeParserDB.prototype);
        obj.__wbg_ptr = ptr;
        JSBytecodeParserDBFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSBytecodeParserDBFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsbytecodeparserdb_free(ptr, 0);
    }
    /**
    * Returns the bytecode of the parser as bytes.
    * @returns {Uint8Array}
    */
    get bytecode() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsbytecodeparserdb_bytecode(this.__wbg_ptr);
        return takeObject(ret);
    }
    /**
    * A list of enterble non-terminal names and their respective bytecode entry
    * point address address
    * @returns {any}
    */
    get entry_points() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsbytecodeparserdb_entry_points(this.__wbg_ptr);
        return takeObject(ret);
    }
}

const JSCSTNodeFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jscstnode_free(ptr >>> 0, 1));
/**
*/
export class JSCSTNode {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSCSTNode.prototype);
        obj.__wbg_ptr = ptr;
        JSCSTNodeFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSCSTNodeFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jscstnode_free(ptr, 0);
    }
    /**
    * @returns {string}
    */
    get_type() {
        let deferred1_0;
        let deferred1_1;
        try {
            if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertNum(this.__wbg_ptr);
            wasm.jscstnode_get_type(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            deferred1_0 = r0;
            deferred1_1 = r1;
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
    * @param {EditGraph} graph
    * @returns {string}
    */
    get_text(graph) {
        let deferred1_0;
        let deferred1_1;
        try {
            if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertNum(this.__wbg_ptr);
            _assertClass(graph, EditGraph);
            if (graph.__wbg_ptr === 0) {
                throw new Error('Attempt to use a moved value');
            }
            wasm.jscstnode_get_text(retptr, this.__wbg_ptr, graph.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            deferred1_0 = r0;
            deferred1_1 = r1;
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
    /**
    * @param {number} index
    * @returns {JSCSTNode | undefined}
    */
    child_at(index) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(index);
        const ret = wasm.jscstnode_child_at(this.__wbg_ptr, index);
        return ret === 0 ? undefined : JSCSTNode.__wrap(ret);
    }
    /**
    * @returns {number}
    */
    num_of_children() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jscstnode_num_of_children(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @returns {number}
    */
    len() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jscstnode_len(this.__wbg_ptr);
        return ret >>> 0;
    }
}

const JSCTXStateFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsctxstate_free(ptr >>> 0, 1));
/**
*/
export class JSCTXState {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSCTXState.prototype);
        obj.__wbg_ptr = ptr;
        JSCTXStateFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSCTXStateFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsctxstate_free(ptr, 0);
    }
    /**
    * @returns {boolean}
    */
    get is_scanner() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_is_scanner(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} arg0
    */
    set is_scanner(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsctxstate_is_scanner(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get end_ptr() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_end_ptr(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set end_ptr(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_end_ptr(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get input_ptr() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_input_ptr(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set input_ptr(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_input_ptr(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get sym_ptr() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_sym_ptr(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set sym_ptr(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_sym_ptr(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get begin_ptr() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_begin_ptr(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set begin_ptr(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_begin_ptr(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get anchor_ptr() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_anchor_ptr(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set anchor_ptr(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_anchor_ptr(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get tok_len() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_tok_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set tok_len(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_tok_len(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get tok_id() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_tok_id(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set tok_id(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_tok_id(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get sym_len() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsctxstate_sym_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set sym_len(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsctxstate_sym_len(this.__wbg_ptr, arg0);
    }
}

const JSDebugPacketFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsdebugpacket_free(ptr >>> 0, 1));
/**
*/
export class JSDebugPacket {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSDebugPacket.prototype);
        obj.__wbg_ptr = ptr;
        JSDebugPacketFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSDebugPacketFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsdebugpacket_free(ptr, 0);
    }
    /**
    * @returns {JSDebugEvent}
    */
    get event() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_event(this.__wbg_ptr);
        return ret;
    }
    /**
    * @param {JSDebugEvent} arg0
    */
    set event(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_event(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {JSCTXState}
    */
    get ctx() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_ctx(this.__wbg_ptr);
        return JSCTXState.__wrap(ret);
    }
    /**
    * @param {JSCTXState} arg0
    */
    set ctx(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(arg0, JSCTXState);
        if (arg0.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        var ptr0 = arg0.__destroy_into_raw();
        wasm.__wbg_set_jsdebugpacket_ctx(this.__wbg_ptr, ptr0);
    }
    /**
    * @returns {number}
    */
    get instruction() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_instruction(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set instruction(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_instruction(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get offset_start() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_offset_start(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set offset_start(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_offset_start(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get offset_end() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_offset_end(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set offset_end(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_offset_end(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get nonterminal_id() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_nonterminal_id(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set nonterminal_id(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_nonterminal_id(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get rule_id() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_rule_id(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set rule_id(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_rule_id(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get symbol_count() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_symbol_count(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set symbol_count(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_symbol_count(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get complete() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_complete(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set complete(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsdebugpacket_complete(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {boolean}
    */
    get is_scanner() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsdebugpacket_is_scanner(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} arg0
    */
    set is_scanner(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsdebugpacket_is_scanner(this.__wbg_ptr, arg0);
    }
}

const JSGrammarIdentitiesFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsgrammaridentities_free(ptr >>> 0, 1));
/**
* A Grammar Identity
*/
export class JSGrammarIdentities {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSGrammarIdentities.prototype);
        obj.__wbg_ptr = ptr;
        JSGrammarIdentitiesFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSGrammarIdentitiesFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsgrammaridentities_free(ptr, 0);
    }
}

const JSIRParserFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsirparser_free(ptr >>> 0, 1));
/**
* Parser states generated from the compilation of parser db
*/
export class JSIRParser {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSIRParser.prototype);
        obj.__wbg_ptr = ptr;
        JSIRParserFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSIRParserFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsirparser_free(ptr, 0);
    }
    /**
    * @returns {JSParserClassification}
    */
    get classification() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsirparser_classification(this.__wbg_ptr);
        return JSParserClassification.__wrap(ret);
    }
    /**
    * @returns {any}
    */
    get num_of_states() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsirparser_num_of_states(this.__wbg_ptr);
        return takeObject(ret);
    }
    /**
    * @returns {any}
    */
    get optimized() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsirparser_optimized(this.__wbg_ptr);
        return takeObject(ret);
    }
}

const JSParserClassificationFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsparserclassification_free(ptr >>> 0, 1));
/**
*/
export class JSParserClassification {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSParserClassification.prototype);
        obj.__wbg_ptr = ptr;
        JSParserClassificationFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSParserClassificationFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsparserclassification_free(ptr, 0);
    }
    /**
    * @returns {boolean}
    */
    get bottom_up() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparserclassification_bottom_up(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} arg0
    */
    set bottom_up(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsparserclassification_bottom_up(this.__wbg_ptr, arg0);
    }
    /**
    * Maximum peek level used to disambiguate conflicting phrases. If this is
    * equal to `u16::MAX`, then peeking failed or a fork was used in its place.
    * @returns {number}
    */
    get max_k() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparserclassification_max_k(this.__wbg_ptr);
        return ret;
    }
    /**
    * Maximum peek level used to disambiguate conflicting phrases. If this is
    * equal to `u16::MAX`, then peeking failed or a fork was used in its place.
    * @param {number} arg0
    */
    set max_k(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsparserclassification_max_k(this.__wbg_ptr, arg0);
    }
    /**
    * If set to true then the parser has at least one state that transitions on
    * non-terminals as well terminals.
    * @returns {boolean}
    */
    get gotos_present() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparserclassification_gotos_present(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * If set to true then the parser has at least one state that transitions on
    * non-terminals as well terminals.
    * @param {boolean} arg0
    */
    set gotos_present(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsparserclassification_gotos_present(this.__wbg_ptr, arg0);
    }
    /**
    * If set to true, then the parser has at least one state that jumps to the
    * head state of a specific non-terminal
    * @returns {boolean}
    */
    get calls_present() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparserclassification_calls_present(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * If set to true, then the parser has at least one state that jumps to the
    * head state of a specific non-terminal
    * @param {boolean} arg0
    */
    set calls_present(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsparserclassification_calls_present(this.__wbg_ptr, arg0);
    }
    /**
    * If set to true, the parser has at least one state that performs k>1
    * lookaheads before selecting an appropriate alternative action.
    * @returns {boolean}
    */
    get peeks_present() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparserclassification_peeks_present(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * If set to true, the parser has at least one state that performs k>1
    * lookaheads before selecting an appropriate alternative action.
    * @param {boolean} arg0
    */
    set peeks_present(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsparserclassification_peeks_present(this.__wbg_ptr, arg0);
    }
    /**
    * If set to true, the parser has at least one state that forks the parse
    * tree, and performs parsing on separate alternatives in parallel
    * @returns {boolean}
    */
    get forks_present() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparserclassification_forks_present(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * If set to true, the parser has at least one state that forks the parse
    * tree, and performs parsing on separate alternatives in parallel
    * @param {boolean} arg0
    */
    set forks_present(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsparserclassification_forks_present(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {string}
    */
    get_type() {
        let deferred1_0;
        let deferred1_1;
        try {
            if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertNum(this.__wbg_ptr);
            wasm.jsparserclassification_get_type(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            deferred1_0 = r0;
            deferred1_1 = r1;
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
}

const JSParserConfigFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsparserconfig_free(ptr >>> 0, 1));
/**
*/
export class JSParserConfig {

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSParserConfig.prototype);
        obj.__wbg_ptr = ptr;
        JSParserConfigFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSParserConfigFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsparserconfig_free(ptr, 0);
    }
    /**
    */
    constructor() {
        const ret = wasm.jsparserconfig_new();
        this.__wbg_ptr = ret >>> 0;
        JSParserConfigFinalization.register(this, this.__wbg_ptr, this);
        return this;
    }
    /**
    * @returns {JSParserConfig}
    */
    static cst_editor() {
        const ret = wasm.jsparserconfig_cst_editor();
        return JSParserConfig.__wrap(ret);
    }
    /**
    * When enable, recursive descent style `Call` states will be generated
    * @returns {boolean}
    */
    get ALLOW_CALLS() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_ALLOW_CALLS(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set ALLOW_CALLS(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_ALLOW_CALLS(this.__wbg_ptr, val);
    }
    /**
    * When enable, LR style states can be produced, in general
    * allowing more advanced grammar constructs to be parsed, such
    * as left recursive rules.
    *
    * When disabled, grammars with rules that require LR style parse states
    * will be rejected, and relevant errors will be reported.
    * @returns {boolean}
    */
    get ALLOW_LR() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_ALLOW_LR(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set ALLOW_LR(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_ALLOW_LR(this.__wbg_ptr, val);
    }
    /**
    * When enabled, unrestricted lookahead states states will be generated
    *
    * When disabled, grammars with rules that require a lookahead that is
    *  `k>1` will be rejected, and relevant errors will be reported.
    * @returns {boolean}
    */
    get ALLOW_PEEKING() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_ALLOW_PEEKING(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set ALLOW_PEEKING(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_ALLOW_PEEKING(this.__wbg_ptr, val);
    }
    /**
    * The maximum number of lookead symbols allowed before parser construction
    * is aborted or a different disambiguating strategy is employed.
    * @returns {number}
    */
    get max_k() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_max_k(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} val
    */
    set max_k(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(val);
        wasm.jsparserconfig_set_max_k(this.__wbg_ptr, val);
    }
    /**
    * Allow the parser to split its context to handle ambiguous rules. This may
    * lead to a CSF (Concrete Syntax Forest) or a CSDAG (Concrete Syntax DAG)
    * being returned by the parser instead of a CST
    * @returns {boolean}
    */
    get ALLOW_CONTEXT_SPLITTING() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_ALLOW_CONTEXT_SPLITTING(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set ALLOW_CONTEXT_SPLITTING(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_ALLOW_CONTEXT_SPLITTING(this.__wbg_ptr, val);
    }
    /**
    * Creates a single scanner instead of multiple contextual scanners. More
    * likely to report terminal conflicts.
    * @returns {boolean}
    */
    get CONTEXT_FREE() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_CONTEXT_FREE(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set CONTEXT_FREE(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_CONTEXT_FREE(this.__wbg_ptr, val);
    }
    /**
    * Creates states that directly handle transitions on terminals, allowing the
    * creation of parsers that can patch existing CST structures.
    * @returns {boolean}
    */
    get AllOW_CST_MERGING() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_AllOW_CST_MERGING(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set AllOW_CST_MERGING(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_AllOW_CST_MERGING(this.__wbg_ptr, val);
    }
    /**
    * Allow the parser to shift on CST non-term nodes.
    * @returns {boolean}
    */
    get ALLOW_CST_NONTERM_SHIFT() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_ALLOW_CST_NONTERM_SHIFT(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set ALLOW_CST_NONTERM_SHIFT(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_ALLOW_CST_NONTERM_SHIFT(this.__wbg_ptr, val);
    }
    /**
    * Makes entry points for all non-terminals defined in the grammar.
    * @returns {boolean}
    */
    get EXPORT_ALL_NONTERMS() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jsparserconfig_EXPORT_ALL_NONTERMS(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} val
    */
    set EXPORT_ALL_NONTERMS(val) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(val);
        wasm.jsparserconfig_set_EXPORT_ALL_NONTERMS(this.__wbg_ptr, val);
    }
}

const JSParserDBFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsparserdb_free(ptr >>> 0, 1));
/**
* A Parser database derived from grammar defined in a JSSoup
*/
export class JSParserDB {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSParserDB.prototype);
        obj.__wbg_ptr = ptr;
        JSParserDBFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSParserDBFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsparserdb_free(ptr, 0);
    }
}

const JSParserGraphFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsparsergraph_free(ptr >>> 0, 1));
/**
* A Parser database derived from grammar defined in a JSSoup
*/
export class JSParserGraph {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSParserGraphFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsparsergraph_free(ptr, 0);
    }
}

const JSParserMetricsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsparsermetrics_free(ptr >>> 0, 1));
/**
*/
export class JSParserMetrics {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSParserMetricsFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsparsermetrics_free(ptr, 0);
    }
    /**
    * @returns {JSParserClassification}
    */
    get classification() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparsermetrics_classification(this.__wbg_ptr);
        return JSParserClassification.__wrap(ret);
    }
    /**
    * @param {JSParserClassification} arg0
    */
    set classification(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertClass(arg0, JSParserClassification);
        if (arg0.__wbg_ptr === 0) {
            throw new Error('Attempt to use a moved value');
        }
        var ptr0 = arg0.__destroy_into_raw();
        wasm.__wbg_set_jsparsermetrics_classification(this.__wbg_ptr, ptr0);
    }
    /**
    * @returns {number}
    */
    get num_of_states() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparsermetrics_num_of_states(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set num_of_states(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsparsermetrics_num_of_states(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {boolean}
    */
    get optimized() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsparsermetrics_optimized(this.__wbg_ptr);
        return ret !== 0;
    }
    /**
    * @param {boolean} arg0
    */
    set optimized(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertBoolean(arg0);
        wasm.__wbg_set_jsparsermetrics_optimized(this.__wbg_ptr, arg0);
    }
}

const JSPatchResultFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jspatchresult_free(ptr >>> 0, 1));
/**
*/
export class JSPatchResult {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSPatchResult.prototype);
        obj.__wbg_ptr = ptr;
        JSPatchResultFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSPatchResultFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jspatchresult_free(ptr, 0);
    }
    /**
    * @param {number} index
    * @returns {JSCSTNode | undefined}
    */
    node_at(index) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(index);
        const ret = wasm.jspatchresult_node_at(this.__wbg_ptr, index);
        return ret === 0 ? undefined : JSCSTNode.__wrap(ret);
    }
    /**
    * @returns {number}
    */
    num_of_nodes() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.jspatchresult_num_of_nodes(this.__wbg_ptr);
        return ret >>> 0;
    }
}

const JSRadlrGrammarFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsradlrgrammar_free(ptr >>> 0, 1));
/**
* An arbitrary collection of grammars
*/
export class JSRadlrGrammar {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSRadlrGrammar.prototype);
        obj.__wbg_ptr = ptr;
        JSRadlrGrammarFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSRadlrGrammarFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsradlrgrammar_free(ptr, 0);
    }
    /**
    * Adds or replaces grammar in the soup, or throws an error
    * if the grammar is invalid. Returns the grammar
    * id if successful.
    * @param {string} grammar_source
    * @param {string} path
    * @returns {JSGrammarIdentities}
    */
    add_grammar(grammar_source, path) {
        try {
            if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertNum(this.__wbg_ptr);
            const ptr0 = passStringToWasm0(grammar_source, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len0 = WASM_VECTOR_LEN;
            const ptr1 = passStringToWasm0(path, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len1 = WASM_VECTOR_LEN;
            wasm.jsradlrgrammar_add_grammar(retptr, this.__wbg_ptr, ptr0, len0, ptr1, len1);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            var r2 = getDataViewMemory0().getInt32(retptr + 4 * 2, true);
            if (r2) {
                throw takeObject(r1);
            }
            return JSGrammarIdentities.__wrap(r0);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
    /**
    * Adds a non-terminal targeting a specific grammar
    * @param {string} _grammar_name
    */
    add_nonterminal(_grammar_name) {
        try {
            if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertNum(this.__wbg_ptr);
            const ptr0 = passStringToWasm0(_grammar_name, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
            const len0 = WASM_VECTOR_LEN;
            wasm.jsradlrgrammar_add_nonterminal(retptr, this.__wbg_ptr, ptr0, len0);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            if (r1) {
                throw takeObject(r0);
            }
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
        }
    }
}

const JSRadlrSourceErrorFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_jsradlrsourceerror_free(ptr >>> 0, 1));
/**
*/
export class JSRadlrSourceError {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(JSRadlrSourceError.prototype);
        obj.__wbg_ptr = ptr;
        JSRadlrSourceErrorFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        JSRadlrSourceErrorFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_jsradlrsourceerror_free(ptr, 0);
    }
    /**
    * @returns {number}
    */
    get line() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsradlrsourceerror_line(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set line(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsradlrsourceerror_line(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get col() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsradlrsourceerror_col(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set col(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsradlrsourceerror_col(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get len() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsradlrsourceerror_len(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set len(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsradlrsourceerror_len(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get start_offset() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsradlrsourceerror_start_offset(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set start_offset(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsradlrsourceerror_start_offset(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get end_offset() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_jsradlrsourceerror_end_offset(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set end_offset(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_jsradlrsourceerror_end_offset(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {string}
    */
    get message() {
        let deferred1_0;
        let deferred1_1;
        try {
            if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            _assertNum(this.__wbg_ptr);
            wasm.jsradlrsourceerror_message(retptr, this.__wbg_ptr);
            var r0 = getDataViewMemory0().getInt32(retptr + 4 * 0, true);
            var r1 = getDataViewMemory0().getInt32(retptr + 4 * 1, true);
            deferred1_0 = r0;
            deferred1_1 = r1;
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(deferred1_0, deferred1_1, 1);
        }
    }
}

const PositionedErrorsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_positionederrors_free(ptr >>> 0, 1));
/**
*/
export class PositionedErrors {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(PositionedErrors.prototype);
        obj.__wbg_ptr = ptr;
        PositionedErrorsFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        PositionedErrorsFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_positionederrors_free(ptr, 0);
    }
    /**
    * @returns {number}
    */
    get length() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.positionederrors_length(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} index
    * @returns {JSRadlrSourceError | undefined}
    */
    get_error_at(index) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(index);
        const ret = wasm.positionederrors_get_error_at(this.__wbg_ptr, index);
        return ret === 0 ? undefined : JSRadlrSourceError.__wrap(ret);
    }
}

const TokenOffsetsFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_tokenoffsets_free(ptr >>> 0, 1));
/**
*/
export class TokenOffsets {

    constructor() {
        throw new Error('cannot invoke `new` directly');
    }

    static __wrap(ptr) {
        ptr = ptr >>> 0;
        const obj = Object.create(TokenOffsets.prototype);
        obj.__wbg_ptr = ptr;
        TokenOffsetsFinalization.register(obj, obj.__wbg_ptr, obj);
        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        TokenOffsetsFinalization.unregister(this);
        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_tokenoffsets_free(ptr, 0);
    }
    /**
    * @returns {number}
    */
    get start() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_tokenoffsets_start(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set start(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_tokenoffsets_start(this.__wbg_ptr, arg0);
    }
    /**
    * @returns {number}
    */
    get end() {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        const ret = wasm.__wbg_get_tokenoffsets_end(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set end(arg0) {
        if (this.__wbg_ptr == 0) throw new Error('Attempt to use a moved value');
        _assertNum(this.__wbg_ptr);
        _assertNum(arg0);
        wasm.__wbg_set_tokenoffsets_end(this.__wbg_ptr, arg0);
    }
}

async function __wbg_load(module, imports) {
    if (typeof Response === 'function' && module instanceof Response) {
        if (typeof WebAssembly.instantiateStreaming === 'function') {
            try {
                return await WebAssembly.instantiateStreaming(module, imports);

            } catch (e) {
                if (module.headers.get('Content-Type') != 'application/wasm') {
                    console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);

                } else {
                    throw e;
                }
            }
        }

        const bytes = await module.arrayBuffer();
        return await WebAssembly.instantiate(bytes, imports);

    } else {
        const instance = await WebAssembly.instantiate(module, imports);

        if (instance instanceof WebAssembly.Instance) {
            return { instance, module };

        } else {
            return instance;
        }
    }
}

function __wbg_get_imports() {
    const imports = {};
    imports.wbg = {};
    imports.wbg.__wbindgen_as_number = function(arg0) {
        const ret = +getObject(arg0);
        return ret;
    };
    imports.wbg.__wbindgen_number_new = function(arg0) {
        const ret = arg0;
        return addHeapObject(ret);
    };
    imports.wbg.__wbindgen_string_new = function(arg0, arg1) {
        const ret = getStringFromWasm0(arg0, arg1);
        return addHeapObject(ret);
    };
    imports.wbg.__wbindgen_object_clone_ref = function(arg0) {
        const ret = getObject(arg0);
        return addHeapObject(ret);
    };
    imports.wbg.__wbg_positionederrors_new = function() { return logError(function (arg0) {
        const ret = PositionedErrors.__wrap(arg0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_tokenoffsets_new = function() { return logError(function (arg0) {
        const ret = TokenOffsets.__wrap(arg0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_jsdebugpacket_new = function() { return logError(function (arg0) {
        const ret = JSDebugPacket.__wrap(arg0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_new_a220cf903aa02ca2 = function() { return logError(function () {
        const ret = new Array();
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_set_673dda6c73d19609 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0)[arg1 >>> 0] = takeObject(arg2);
    }, arguments) };
    imports.wbg.__wbg_push_37c89022f34c01ca = function() { return logError(function (arg0, arg1) {
        const ret = getObject(arg0).push(getObject(arg1));
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_newwithlength_ec548f448387c968 = function() { return logError(function (arg0) {
        const ret = new Uint8Array(arg0 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_8a2cb9ca96b27ec9 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Uint8Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_length_8339fcf5d8ecd12e = function() { return logError(function (arg0) {
        const ret = getObject(arg0).length;
        _assertNum(ret);
        return ret;
    }, arguments) };
    imports.wbg.__wbg_set_d1e79e2388520f18 = function() { return logError(function (arg0, arg1, arg2) {
        getObject(arg0).set(getObject(arg1), arg2 >>> 0);
    }, arguments) };
    imports.wbg.__wbg_new_5d12bfb7e37fc427 = function() { return logError(function (arg0) {
        const ret = new Uint32Array(getObject(arg0));
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_newwithbyteoffsetandlength_874df3e29cb555f9 = function() { return logError(function (arg0, arg1, arg2) {
        const ret = new Uint32Array(getObject(arg0), arg1 >>> 0, arg2 >>> 0);
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbg_buffer_b7b08af79b0b0974 = function() { return logError(function (arg0) {
        const ret = getObject(arg0).buffer;
        return addHeapObject(ret);
    }, arguments) };
    imports.wbg.__wbindgen_debug_string = function(arg0, arg1) {
        const ret = debugString(getObject(arg1));
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
        getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
    };
    imports.wbg.__wbindgen_object_drop_ref = function(arg0) {
        takeObject(arg0);
    };
    imports.wbg.__wbindgen_throw = function(arg0, arg1) {
        throw new Error(getStringFromWasm0(arg0, arg1));
    };
    imports.wbg.__wbindgen_memory = function() {
        const ret = wasm.memory;
        return addHeapObject(ret);
    };

    return imports;
}

function __wbg_init_memory(imports, memory) {

}

function __wbg_finalize_init(instance, module) {
    wasm = instance.exports;
    __wbg_init.__wbindgen_wasm_module = module;
    cachedDataViewMemory0 = null;
    cachedUint8ArrayMemory0 = null;



    return wasm;
}

function initSync(module) {
    if (wasm !== undefined) return wasm;


    if (typeof module !== 'undefined' && Object.getPrototypeOf(module) === Object.prototype)
    ({module} = module)
    else
    console.warn('using deprecated parameters for `initSync()`; pass a single object instead')

    const imports = __wbg_get_imports();

    __wbg_init_memory(imports);

    if (!(module instanceof WebAssembly.Module)) {
        module = new WebAssembly.Module(module);
    }

    const instance = new WebAssembly.Instance(module, imports);

    return __wbg_finalize_init(instance, module);
}

async function __wbg_init(module_or_path) {
    if (wasm !== undefined) return wasm;


    if (typeof module_or_path !== 'undefined' && Object.getPrototypeOf(module_or_path) === Object.prototype)
    ({module_or_path} = module_or_path)
    else
    console.warn('using deprecated parameters for the initialization function; pass a single object instead')

    if (typeof module_or_path === 'undefined') {
        module_or_path = new URL('radlr_wasm_bg.wasm', import.meta.url);
    }
    const imports = __wbg_get_imports();

    if (typeof module_or_path === 'string' || (typeof Request === 'function' && module_or_path instanceof Request) || (typeof URL === 'function' && module_or_path instanceof URL)) {
        module_or_path = fetch(module_or_path);
    }

    __wbg_init_memory(imports);

    const { instance, module } = await __wbg_load(await module_or_path, imports);

    return __wbg_finalize_init(instance, module);
}

export { initSync };
export default __wbg_init;
