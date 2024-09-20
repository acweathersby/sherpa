/**
 * @brief ParseState
 */
class ParseState {

  address: number;

  /**
   * @param {number} address 
   */
  constructor(address: number = 0) {
    /**
     * The bytecode address of this state.
     */
    this.address = address;
  }
}

/**
 * @brief ParserContext
 * 
 * Maintains properties of the current parser state.
 */
export class ParserContext {
  /**
   * @brief Parser State Stack
   */
  states: ParseState[] = [new ParseState(0), new ParseState(8)]

  /// The head of the input window.
  begin_ptr = 0;

  /// Positioned at the end of the last shifted token
  anchor_ptr = 0;

  /// Positioned at the start of the current incoming token, and following any
  /// characters that have been skipped. (This only differs from `anchor_ptr`
  /// when peeking, in which case there may have been skipped tokens that the
  /// peeking process has encountered and shifted over)
  sym_ptr = 0;

  /// The start of all unevaluated bytes. This differs from `sym_ptr` when
  /// using scanner states to evaluate incoming bytes
  input_ptr = 0;

  /// The end of the input window. This is a fixed reference that should
  /// not change during parsing unless the end of the input window has been
  /// reached and a larger window is requested.
  end_ptr = 0;

  /// The number of characters that comprize the current
  /// token. This should be 0 if the tok_id is also 0
  tok_id = 0;

  /// The byte length of the current token
  tok_byte_len = 0;

  /// The byte length of the most current input character. This is usually 1
  /// byte unless the input contains UTF codepoints outside the ASCII range.
  byte_len = 0;

  /** 
   * @brief The parser has entered a final state.
   */
  finished = false

  /**
   * @brief The id of the last non-terminal that has been created
   * 
   * @type {number}
   */
  nonterm = 0

  recovery_tok_id = 0;


  /**
   * Pops the most recent state from the parse stack.
   * @returns {ParseState | null}
   */
  pop_state() {
    return this.states.pop();
  }

  /**
 * Pushes a new state to the top of the parse stack.
 * 
 * @param {ParseState} state
 */
  push_state(state: ParseState) {
    this.states.push(state);
  }
  /**
   * Resets the context and initializes it begin parsing at the given entrypoint.
   * @param ep 
   */
  set_entrypoint(ep: [string, number]) {
    this.states = [new ParseState(0), new ParseState(ep[1])];
    this.begin_ptr = 0;
    this.anchor_ptr = 0;
    this.sym_ptr = 0;
    this.input_ptr = 0;
    this.end_ptr = 0;
    this.tok_id = 0;
    this.tok_byte_len = 0;
    this.byte_len = 0;
    this.finished = false;
  }
}

class ParseAction {
  type: ParseActionType;

  payload: any = {};
  /**
   * @param {ParseActionType} type 
   * @param {any} payload 
   */
  constructor(type: ParseActionType, payload = {}) {
    /**
     * @type{ParseActionType}
     */
    this.type = type;

    this.payload = payload;
  }
}

/** @enum {number} */
export enum ParseActionType {
  Pass = 0,
  Fail = 1,
  Error = 2,
  Shift = 3,
  Peek = 4,
  Skip = 5,
  Reduce = 6,
  Accept = 7
}


/** @enum {number} */
enum ByteCodeOpCode {
  NoOp = 0,
  Pass = 1,
  Fail = 2,
  ShiftChar = 3,
  ShiftToken = 4,
  ShiftTokenScanless = 5,
  PeekToken = 6,
  PeekTokenScanless = 7,
  SkipToken = 8,
  SkipTokenScanless = 9,
  PeekSkipToken = 10,
  PeekSkipTokenScanless = 11,
  PeekReset = 12,
  Accept = 13,
  PopGoto = 14,
  PushGoto = 15,
  PushExceptionHandler = 16,
  Goto = 17,
  AssignToken = 18,
  Reduce = 19,
  VectorBranch = 20,
  HashBranch = 21,
  ByteSequence = 22,
  Fork = 23,
  ReadCodepoint = 24,
}

/**
 * @param base_state - The current parser state
 * @param ctx - An active parser context
 * @param input - String containing input data.
 * @param bc - Parser bytecode
 * @param is_scanner 
 */
function dispatch(base_state: ParseState, ctx: ParserContext, input: string, bc: Uint8Array, is_scanner: boolean)
  : { action: ParseAction | null, next_state: ParseState | null } {

  let ip = base_state.address;

  while (true) {
    let i = bc[ip];

    let result;

    switch (i) {
      case ByteCodeOpCode.NoOp: {
        result = {
          action: null, next_state: new ParseState(ip + 1)
        }
        break
      }
      case ByteCodeOpCode.Pass: {
        result = {
          action: new ParseAction(ParseActionType.Pass), next_state: null
        }
        break
      }
      case ByteCodeOpCode.Fail: {
        result = {
          action: new ParseAction(ParseActionType.Fail), next_state: null
        };
        break
      }
      case ByteCodeOpCode.ShiftChar: {
        result = shift_bytes(ip, ctx);
        break;
      }
      case ByteCodeOpCode.ShiftToken: {
        result = shift_token(ip, ctx);
        break;
      }
      case ByteCodeOpCode.ShiftTokenScanless: {
        result = shift_token_scanless(ip, ctx);
        break;
      }
      case ByteCodeOpCode.PeekToken: {
        result = peek_token(ip, ctx);
        break;
      }
      case ByteCodeOpCode.PeekTokenScanless: {
        result = peek_token_scanless(ip, ctx);
        break;
      }
      case ByteCodeOpCode.SkipToken: {
        result = skip_token(base_state.address, ip, ctx);
        break
      }
      case ByteCodeOpCode.SkipTokenScanless: {
        result = skip_token_scanless(base_state.address, ip, ctx);
        break
      }
      case ByteCodeOpCode.PeekSkipToken: {
        result = peek_skip_token(base_state.address, ip, ctx);
        break
      }
      case ByteCodeOpCode.PeekSkipTokenScanless: {
        result = peek_skip_token_scanless(base_state.address, ip, ctx);
        break
      }
      case ByteCodeOpCode.PeekReset: {
        result = peek_reset(ip, ctx);
        break
      }
      case ByteCodeOpCode.Accept: {
        result = {
          action: new ParseAction(ParseActionType.Accept), next_state: null
        }
        break
      }
      case ByteCodeOpCode.PopGoto: {
        result = pop_state(ip, ctx);
        break;
      }
      case ByteCodeOpCode.PushGoto: {
        result = push_state(ip, ctx, bc);
        break
      }
      case ByteCodeOpCode.PushExceptionHandler: {
        break
      }
      case ByteCodeOpCode.Goto: {
        result = goto(ip, ctx, bc);
        break;
      }
      case ByteCodeOpCode.AssignToken: {
        result = assign_token(ip, ctx, bc);
        break;
      }
      case ByteCodeOpCode.Reduce: {
        result = reduce(ip, ctx, bc);
        break
      }
      case ByteCodeOpCode.VectorBranch: {
        result = vector_branch(ip, ctx, bc, input);
        break
      }
      case ByteCodeOpCode.HashBranch: {
        result = hash_branch(ip, ctx, bc, input);
        break
      }
      case ByteCodeOpCode.ByteSequence: {
        throw "Not implemented yet"
        break
      }
      case ByteCodeOpCode.Fork: {
        break
      }
      case ByteCodeOpCode.ReadCodepoint: {
        result = read_codepoint(ip, ctx, input, bc);
        break
      }
    }

    if (result) {

      let { action, next_state } = result;

      if (action) {
        return result
      } else if (next_state) {
        ip = next_state.address;
        continue
      }
    }

    throw "Unexpected state encountered";
  }
}

function shift_bytes(ip: number, ctx: ParserContext) {
  ctx.input_ptr += ctx.byte_len;
  ctx.byte_len = 0;

  return {
    action: null,
    next_state: new ParseState(ip + 1),
  }
}

function shift_token(ip: number, ctx: ParserContext) {

  let parse_action = new ParseAction(ParseActionType.Shift, {
    byte_offset: ctx.sym_ptr,
    byte_length: ctx.tok_byte_len,
    /*     token_line_count: ctx.start_line_num,
        token_line_offset: ctx.start_line_off, */
    token_id: ctx.tok_id
  });


  /*   ctx.start_line_num = ctx.chkp_line_num;
    ctx.start_line_off = ctx.chkp_line_off;
    ctx.end_line_num = ctx.start_line_num;
    ctx.end_line_off = ctx.end_line_off; */

  let new_offset = ctx.sym_ptr + ctx.tok_byte_len;

  ctx.anchor_ptr = new_offset;
  ctx.sym_ptr = new_offset;
  ctx.input_ptr = new_offset;
  ctx.tok_id = 0;
  ctx.recovery_tok_id = 0;
  ctx.tok_byte_len = 0;

  return {
    action: parse_action,
    next_state: new ParseState(ip + 1),
  }
}

function shift_token_scanless(ip: number, ctx: ParserContext) {
  ctx.tok_byte_len = ctx.byte_len;
  return shift_token(ip, ctx);
}

function peek_token(ip: number, ctx: ParserContext) {
  let offset = ctx.sym_ptr + ctx.tok_byte_len;

  ctx.sym_ptr = offset;
  ctx.input_ptr = offset;
  ctx.tok_id = 0;
  ctx.recovery_tok_id = 0;
  ctx.tok_byte_len = 0;


  return {
    action: null,
    next_state: new ParseState(ip + 1),
  }
}

function peek_token_scanless(ip: number, ctx: ParserContext) {
  ctx.tok_byte_len = ctx.byte_len;
  return peek_token(ip, ctx);
}

function __skip_token_core__(base_ip: number, ip: number, ctx: ParserContext) {
  let original_offset = ctx.sym_ptr;
  let offset = ctx.sym_ptr + ctx.tok_byte_len;
  let tok_len = ctx.tok_byte_len;
  let token_id = ctx.tok_id;
  ctx.input_ptr = offset;
  ctx.sym_ptr = offset;
  ctx.tok_id = 0;
  ctx.recovery_tok_id = 0;

  return {
    action: new ParseAction(ParseActionType.Skip, {
      byte_offset: original_offset,
      byte_length: tok_len,
      /*       token_line_count: ctx.start_line_num,
            token_line_offset: ctx.start_line_off, */
      token_id: token_id,
    }),
    next_state: new ParseState(base_ip),
  }
}

function skip_token(base_ip: number, ip: number, ctx: ParserContext) {
  const result = __skip_token_core__(base_ip, ip, ctx);
  /*   ctx.start_line_num = ctx.chkp_line_num;
    ctx.start_line_off = ctx.chkp_line_off;
    ctx.end_line_num = ctx.start_line_num;
    ctx.end_line_off = ctx.end_line_off; */

  return result;
}

function skip_token_scanless(base_ip: number, ip: number, ctx: ParserContext) {
  ctx.tok_byte_len = ctx.byte_len;
  return skip_token(base_ip, ip, ctx);
}

function peek_skip_token(base_ip: number, ip: number, ctx: ParserContext) {
  __skip_token_core__(base_ip, ip, ctx);

  return {
    action: null,
    next_state: new ParseState(ip + 1),
  };
}

function peek_skip_token_scanless(base_ip: number, ip: number, ctx: ParserContext) {
  ctx.tok_byte_len = ctx.byte_len;
  return peek_skip_token(base_ip, ip, ctx);
}

function peek_reset(ip: number, ctx: ParserContext) {
  let offset = ctx.anchor_ptr;
  ctx.sym_ptr = offset;
  ctx.input_ptr = offset;
  ctx.tok_id = 0;
  ctx.recovery_tok_id = 0;
  ctx.tok_byte_len = 0;
  ctx.byte_len = 0;

  /*   ctx.chkp_line_num = ctx.end_line_num;
    ctx.chkp_line_num = ctx.start_line_off;
    ctx.end_line_off = ctx.start_line_off;
    ctx.end_line_num = ctx.end_line_num; */

  return {
    action: null,
    next_state: new ParseState(ip + 1),
  }
}


function assign_token(ip: number, ctx: ParserContext, bc: Uint8Array) {
  const reader = new DataView(bc.buffer, ip);

  ctx.tok_id = reader.getUint32(1, true);
  ctx.tok_byte_len = (ctx.input_ptr - ctx.sym_ptr);
  /*   ctx.chkp_line_num = ctx.end_line_num;
    ctx.chkp_line_off = ctx.end_line_off;
   */
  return {
    action: null,
    next_state: new ParseState(ip + 5),
  }
}


/**
 * 
 * @param {number} ip 
 * @param {ParserContext} ctx 
 * @param {Uint8Array} bc 
 * @returns 
 */
function reduce(ip: number, ctx: ParserContext, bc: Uint8Array) {
  const reader = new DataView(bc.buffer, ip);

  const nonterminal_id = reader.getUint32(1, true);
  const rule_id = reader.getUint32(5, true);
  const symbol_count = reader.getUint16(9, true);

  ctx.nonterm = nonterminal_id;

  return {
    action: new ParseAction(ParseActionType.Reduce, { nonterminal_id, rule_id, symbol_count }),
    next_state: new ParseState(ip + 11),
  }
}

/**
 * 
 * @param {number} ip 
 * @param {ParserContext} ctx 
 * @returns 
 */
function pop_state(ip: number, ctx: ParserContext) {

  ctx.pop_state();

  return {
    action: null,
    next_state: new ParseState(ip + 1),
  }
}

/**
 * 
 * @param {number} ip 
 * @param {ParserContext} ctx 
 * @param {Uint8Array} bc 
 * @returns 
 */
function push_state(ip: number, ctx: ParserContext, bc: Uint8Array) {
  const reader = new DataView(bc.buffer, ip);
  const address = reader.getUint32(2, true);

  ctx.push_state(new ParseState(address));

  return {
    action: null,
    next_state: new ParseState(ip + 6),
  }
}

/**
 * 
 * @param {number} ip 
 * @param {ParserContext} ctx 
 * @param {Uint8Array} bc 
 * @returns 
 */
function goto(ip: number, ctx: ParserContext, bc: Uint8Array) {
  const reader = new DataView(bc.buffer, ip);

  const address = reader.getUint32(2, true);

  return {
    action: null,
    next_state: new ParseState(address),
  }
}

function read_codepoint(i: number, ctx: ParserContext, input: string, bc: Uint8Array) {
  let val = get_input_value(MatchInputType.Codepoint, 0, ctx, input, bc);

  if (val == 0) {
    return {
      action: new ParseAction(ParseActionType.Fail),
      next_state: null
    };
  } else {
    return {
      action: null,
      next_state: new ParseState(i + 1)
    };
  }
}

function hash_branch(
  ip: number, ctx: ParserContext, bc: Uint8Array, input: string
) {
  let { input_type, scan_block_instruction, table_meta: modulo_base, table_start, default_block } = get_table_header(ip, bc);

  let hash_mask = (1 << modulo_base) - 1;
  let input_val = get_input_value(input_type, scan_block_instruction, ctx, input, bc);

  while (true) {
    let hash_index = (input_val & hash_mask);

    while (true) {
      const reader = new DataView(bc.buffer, table_start + hash_index * 4);
      let cell = reader.getUint32(0, true);
      let value = cell & 0x7FF;
      let off = (cell >> 11) & 0x7FF;
      let next = ((cell >> 22) & 0x3FF) - 512;

      if (value == input_val) {
        return {
          action: null,
          next_state: new ParseState(ip + off)
        };
      } else if (next != 0) {
        hash_index += next;
      } else {
        return {
          action: null,
          next_state: new ParseState(default_block)
        };
      }
    }
  }
}


function vector_branch(
  ip: number, ctx: ParserContext, bc: Uint8Array, input: string
) {
  let { input_type, scan_block_instruction, table_meta: value_offset, table_start, default_block, table_length } = get_table_header(ip, bc);


  let input_val = get_input_value(input_type, scan_block_instruction, ctx, input, bc);

  while (true) {
    let value_index = (input_val - value_offset);
    if (value_index < table_length) {

      let address_offset = new DataView(bc.buffer, table_start + value_index * 4).getFloat32(0, true);
      return {
        action: null,
        next_state: new ParseState(ip + address_offset)
      };
    } else {
      return {
        action: null,
        next_state: new ParseState(default_block)
      };
    }
  }

}


function get_table_header(ip: number, bc: Uint8Array) {
  const reader = new DataView(bc.buffer, ip);
  let input_type = reader.getUint8(1);
  let default_delta = reader.getUint32(2, true);
  let scan_address = reader.getUint32(6, true);
  let table_length = reader.getUint32(10, true);
  let table_meta = reader.getUint32(14, true);
  let table_start = ip + 18;

  return {
    input_type,
    table_length,
    table_meta,
    table_start,
    parse_block_address: table_start + (table_length * 4),
    scan_block_instruction: scan_address > 0 ?
      scan_address
      :
      0
    ,
    default_block: ip + default_delta,
  };
}


function get_input_value(input_type: MatchInputType, scanner_address: number, ctx: ParserContext, input: string, bc: Uint8Array) {
  switch (input_type) {
    case MatchInputType.NonTerminal: {
      return ctx.nonterm;
    }

    case MatchInputType.EndOfFile: {
      return +(ctx.input_ptr >= input.length);
    }

    case MatchInputType.Token: {
      if (ctx.recovery_tok_id > 0) {
        ctx.tok_id = ctx.recovery_tok_id;
        ctx.tok_byte_len = 0;
        ctx.byte_len = 0;
      } else {
        token_scan(scanner_address, ctx, input, bc);
      }
      return ctx.tok_id;
    }

    case MatchInputType.Byte: {
      let byte = input[ctx.input_ptr]?.codePointAt(0) || 0;
      if (byte > 0) {
        ctx.byte_len = 1;
      } else {
        ctx.byte_len = 0;
      }
      return byte
    }
    case MatchInputType.ByteScanless: {
      let byte = input[ctx.input_ptr]?.codePointAt(0) || 0;
      if (byte > 0) {
        ctx.tok_byte_len = 1;
      } else {
        ctx.tok_byte_len = 0;
      }
      return byte
    }
    default: {
      let cp = input[ctx.input_ptr]?.codePointAt(0) || 0;

      switch (input_type) {
        case MatchInputType.ClassScanless: {
          ctx.tok_byte_len = 1;
          if (cp > 0) {
            return get_token_class_from_codepoint(cp)
          } else {
            return 0
          }
        }
        case MatchInputType.Class: {
          ctx.byte_len = 1;
          if (cp > 0) {
            return get_token_class_from_codepoint(cp)
          } else {
            return 0
          }
        }
        case MatchInputType.CodepointScanless: {
          return cp;
        }
        case MatchInputType.Codepoint: {
          ctx.byte_len = 1;
          return cp;
        }

        default: return 0;
      }
    }
  }
}


/**
 * @param {ParserContext} ctx - An active parser context
 * @param {string} input - String containing input data.
 * @param {Uint8Array} bc - Parser bytecode
 * 
 * @returns {ParseAction}
 */
export function next(ctx: ParserContext, input: string, bc: Uint8Array): ParseAction | null {

  if (ctx.finished) {
    return null;
  }

  let state = ctx.pop_state();

  outer: while (true) {
    if (!state) {
      throw "Unexpected parse state"
    } else if (state.address < 1) {
      return new ParseAction(ParseActionType.Fail);
    } else {
      const { action, next_state } = dispatch(state, ctx, input, bc, false);

      switch (action?.type) {

        case ParseActionType.Pass: {
          state = ctx.pop_state();
          break
        }
        case ParseActionType.Fail: {

          ctx.finished = true;

          return new ParseAction(ParseActionType.Error, {
            last_nonterminal: ctx.nonterm
          });
        }
        default: {
          if (next_state) {
            ctx.push_state(next_state);
          }
          return action;
        }
      }
    }
  }
}

function token_scan(
  scan_address: number, ctx: ParserContext, input: string, bc: Uint8Array
) {
  ctx.tok_id = 0;
  ctx.input_ptr = ctx.sym_ptr;

  let stack = [new ParseState(0)];

  let state: any = new ParseState(scan_address);

  outer: while (true) {
    if (!state || state.address < 1) {
      break;
    } else {
      const { action, next_state } = dispatch(state, ctx, input, bc, true);

      switch (action?.type) {
        case ParseActionType.Pass: {
          break outer;
        }
        case ParseActionType.Fail: {
          break outer;
        }
        default: {
          if (next_state) {
            ctx.push_state(next_state);
          }
        }
      }

      state = stack.pop();
    }
  }

  ctx.input_ptr = ctx.sym_ptr;
}


enum MatchInputType {
  /// Matches the last reduced nonterminal id
  NonTerminal = 0,
  /// Matches the token id set from a scanner call
  Token = 1,
  /// Matches the class of a character in the input
  Class = 2,
  /// Matches a utf8 codepoint byte sequence in the input
  Codepoint = 3,
  /// Matches a byte in the input
  Byte = 4,
  /// Matches the virtual $eof token in the input stream
  EndOfFile = 5,
  /// Matches anything
  Default = 6,
  /// Matches a byte in the input
  ByteScanless = 7,
  /// Matches a utf8 codepoint byte sequence in the input
  CodepointScanless = 8,
  /// Matches the class of a character in the input
  ClassScanless = 9,
  /// Matches a distinct sequence of bytes.
  ByteSequence = 10,
  /// Matches the top node on the output stack.
  CSTNode = 11,
}



const UNI_ID_START_INDICES = [
  170, 181, 186, 748, 750, 895, 902, 908, 1369, 1749, 1791, 1808, 1969, 2042, 2074, 2084, 2088, 2365, 2384, 2482, 2493, 2510,
  2556, 2654, 2749, 2768, 2809, 2877, 2929, 2947, 2972, 3024, 3133, 3200, 3261, 3294, 3389, 3406, 3517, 3716, 3749, 3773, 3782,
  3840, 4159, 4193, 4238, 4295, 4301, 4696, 4800, 6103, 6108, 6314, 6823, 7418, 8025, 8027, 8029, 8126, 8305, 8319, 8450, 8455,
  8469, 8484, 8486, 8488, 8526, 11559, 11565, 11631, 11823, 13312, 19893, 19968, 40943, 43259, 43471, 43642, 43697, 43712, 43714,
  44032, 55203, 64285, 64318, 67592, 67644, 68096, 69415, 69956, 70006, 70106, 70108, 70280, 70461, 70480, 70751, 70855, 71236,
  71352, 71935, 72161, 72163, 72192, 72250, 72272, 72349, 72768, 73030, 73112, 94032, 94179, 94208, 100343, 119970, 119995,
  120134, 123214, 125259, 126500, 126503, 126521, 126523, 126530, 126535, 126537, 126539, 126548, 126551, 126553, 126555, 126557,
  126559, 126564, 126590, 131072, 173782, 173824, 177972, 177984, 178205, 178208, 183969, 183984, 191456,
];

const UNI_ID_START_RANGES = [
  65, 90, 97, 122, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 880, 884, 886, 887, 890, 893, 904, 906, 910, 929, 931, 1013,
  1015, 1153, 1162, 1327, 1329, 1366, 1376, 1416, 1488, 1514, 1519, 1522, 1568, 1610, 1646, 1647, 1649, 1747, 1765, 1766, 1774,
  1775, 1786, 1788, 1810, 1839, 1869, 1957, 1994, 2026, 2036, 2037, 2048, 2069, 2112, 2136, 2144, 2154, 2208, 2228, 2230, 2237,
  2308, 2361, 2392, 2401, 2417, 2432, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2486, 2489, 2524, 2525, 2527, 2529, 2544,
  2545, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2649, 2652, 2674, 2676, 2693, 2701,
  2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2784, 2785, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866,
  2867, 2869, 2873, 2908, 2909, 2911, 2913, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2974, 2975, 2979, 2980, 2984, 2986,
  2990, 3001, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3129, 3160, 3162, 3168, 3169, 3205, 3212, 3214, 3216, 3218, 3240, 3242,
  3251, 3253, 3257, 3296, 3297, 3313, 3314, 3333, 3340, 3342, 3344, 3346, 3386, 3412, 3414, 3423, 3425, 3450, 3455, 3461, 3478,
  3482, 3505, 3507, 3515, 3520, 3526, 3585, 3632, 3634, 3635, 3648, 3654, 3713, 3714, 3718, 3722, 3724, 3747, 3751, 3760, 3762,
  3763, 3776, 3780, 3804, 3807, 3904, 3911, 3913, 3948, 3976, 3980, 4096, 4138, 4176, 4181, 4186, 4189, 4197, 4198, 4206, 4208,
  4213, 4225, 4256, 4293, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786,
  4789, 4792, 4798, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4992, 5007, 5024, 5109, 5112, 5117, 5121, 5740,
  5743, 5759, 5761, 5786, 5792, 5866, 5870, 5880, 5888, 5900, 5902, 5905, 5920, 5937, 5952, 5969, 5984, 5996, 5998, 6000, 6016,
  6067, 6176, 6264, 6272, 6276, 6279, 6312, 6320, 6389, 6400, 6430, 6480, 6509, 6512, 6516, 6528, 6571, 6576, 6601, 6656, 6678,
  6688, 6740, 6917, 6963, 6981, 6987, 7043, 7072, 7086, 7087, 7098, 7141, 7168, 7203, 7245, 7247, 7258, 7293, 7296, 7304, 7312,
  7354, 7357, 7359, 7401, 7404, 7406, 7411, 7413, 7414, 7424, 7615, 7680, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023,
  8031, 8061, 8064, 8116, 8118, 8124, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8336,
  8348, 8458, 8467, 8473, 8477, 8490, 8493, 8495, 8505, 8508, 8511, 8517, 8521, 8544, 8584, 11264, 11310, 11312, 11358, 11360,
  11492, 11499, 11502, 11506, 11507, 11520, 11557, 11568, 11623, 11648, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704,
  11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 12293, 12295, 12321, 12329, 12337, 12341, 12344, 12348, 12353,
  12438, 12445, 12447, 12449, 12538, 12540, 12543, 12549, 12591, 12593, 12686, 12704, 12730, 12784, 12799, 40960, 42124, 42192,
  42237, 42240, 42508, 42512, 42527, 42538, 42539, 42560, 42606, 42623, 42653, 42656, 42735, 42775, 42783, 42786, 42888, 42891,
  42943, 42946, 42950, 42999, 43009, 43011, 43013, 43015, 43018, 43020, 43042, 43072, 43123, 43138, 43187, 43250, 43255, 43261,
  43262, 43274, 43301, 43312, 43334, 43360, 43388, 43396, 43442, 43488, 43492, 43494, 43503, 43514, 43518, 43520, 43560, 43584,
  43586, 43588, 43595, 43616, 43638, 43646, 43695, 43701, 43702, 43705, 43709, 43739, 43741, 43744, 43754, 43762, 43764, 43777,
  43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43824, 43866, 43868, 43879, 43888, 44002, 55216, 55238, 55243,
  55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64287, 64296, 64298, 64310, 64312, 64316, 64320, 64321, 64323,
  64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65136, 65140, 65142, 65276, 65313, 65338, 65345,
  65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500, 65536, 65547, 65549, 65574, 65576, 65594, 65596,
  65597, 65599, 65613, 65616, 65629, 65664, 65786, 65856, 65908, 66176, 66204, 66208, 66256, 66304, 66335, 66349, 66378, 66384,
  66421, 66432, 66461, 66464, 66499, 66504, 66511, 66513, 66517, 66560, 66717, 66736, 66771, 66776, 66811, 66816, 66855, 66864,
  66915, 67072, 67382, 67392, 67413, 67424, 67431, 67584, 67589, 67594, 67637, 67639, 67640, 67647, 67669, 67680, 67702, 67712,
  67742, 67808, 67826, 67828, 67829, 67840, 67861, 67872, 67897, 67968, 68023, 68030, 68031, 68112, 68115, 68117, 68119, 68121,
  68149, 68192, 68220, 68224, 68252, 68288, 68295, 68297, 68324, 68352, 68405, 68416, 68437, 68448, 68466, 68480, 68497, 68608,
  68680, 68736, 68786, 68800, 68850, 68864, 68899, 69376, 69404, 69424, 69445, 69600, 69622, 69635, 69687, 69763, 69807, 69840,
  69864, 69891, 69926, 69968, 70002, 70019, 70066, 70081, 70084, 70144, 70161, 70163, 70187, 70272, 70278, 70282, 70285, 70287,
  70301, 70303, 70312, 70320, 70366, 70405, 70412, 70415, 70416, 70419, 70440, 70442, 70448, 70450, 70451, 70453, 70457, 70493,
  70497, 70656, 70708, 70727, 70730, 70784, 70831, 70852, 70853, 71040, 71086, 71128, 71131, 71168, 71215, 71296, 71338, 71424,
  71450, 71680, 71723, 71840, 71903, 72096, 72103, 72106, 72144, 72203, 72242, 72284, 72329, 72384, 72440, 72704, 72712, 72714,
  72750, 72818, 72847, 72960, 72966, 72968, 72969, 72971, 73008, 73056, 73061, 73063, 73064, 73066, 73097, 73440, 73458, 73728,
  74649, 74752, 74862, 74880, 75075, 77824, 78894, 82944, 83526, 92160, 92728, 92736, 92766, 92880, 92909, 92928, 92975, 92992,
  92995, 93027, 93047, 93053, 93071, 93760, 93823, 93952, 94026, 94099, 94111, 94176, 94177, 100352, 101106, 110592, 110878,
  110928, 110930, 110948, 110951, 110960, 111355, 113664, 113770, 113776, 113788, 113792, 113800, 113808, 113817, 119808, 119892,
  119894, 119964, 119966, 119967, 119973, 119974, 119977, 119980, 119982, 119993, 119997, 120003, 120005, 120069, 120071, 120074,
  120077, 120084, 120086, 120092, 120094, 120121, 120123, 120126, 120128, 120132, 120138, 120144, 120146, 120485, 120488, 120512,
  120514, 120538, 120540, 120570, 120572, 120596, 120598, 120628, 120630, 120654, 120656, 120686, 120688, 120712, 120714, 120744,
  120746, 120770, 120772, 120779, 123136, 123180, 123191, 123197, 123584, 123627, 124928, 125124, 125184, 125251, 126464, 126467,
  126469, 126495, 126497, 126498, 126505, 126514, 126516, 126519, 126541, 126543, 126545, 126546, 126561, 126562, 126567, 126570,
  126572, 126578, 126580, 126583, 126585, 126588, 126592, 126601, 126603, 126619, 126625, 126627, 126629, 126633, 126635, 126651,
];

const UNI_ID_CONT_DISCRETE = [
  95, 1471, 1479, 1648, 1809, 2045, 2492, 2519, 2558, 2620, 2641, 2677, 2748, 2876, 2946, 3031, 3260, 3415, 3530, 3542, 3633,
  3761, 3893, 3895, 3897, 4038, 6109, 6313, 7405, 7412, 8276, 8417, 11647, 42607, 43010, 43014, 43019, 43493, 43587, 43696,
  43713, 64286, 65343, 66045, 66272, 68159, 70003, 70206, 70487, 70750, 72164, 72263, 73018, 73031, 94031, 121461, 121476,
];

const UNI_ID_CONT_RANGES = [
  48, 57, 768, 879, 1155, 1159, 1425, 1469, 1473, 1474, 1476, 1477, 1552, 1562, 1611, 1641, 1750, 1756, 1759, 1764, 1767, 1768,
  1770, 1773, 1776, 1785, 1840, 1866, 1958, 1968, 1984, 1993, 2027, 2035, 2070, 2073, 2075, 2083, 2085, 2087, 2089, 2093, 2137,
  2139, 2259, 2273, 2275, 2307, 2362, 2364, 2366, 2383, 2385, 2391, 2402, 2403, 2406, 2415, 2433, 2435, 2494, 2500, 2503, 2504,
  2507, 2509, 2530, 2531, 2534, 2543, 2561, 2563, 2622, 2626, 2631, 2632, 2635, 2637, 2662, 2673, 2689, 2691, 2750, 2757, 2759,
  2761, 2763, 2765, 2786, 2787, 2790, 2799, 2810, 2815, 2817, 2819, 2878, 2884, 2887, 2888, 2891, 2893, 2902, 2903, 2914, 2915,
  2918, 2927, 3006, 3010, 3014, 3016, 3018, 3021, 3046, 3055, 3072, 3076, 3134, 3140, 3142, 3144, 3146, 3149, 3157, 3158, 3170,
  3171, 3174, 3183, 3201, 3203, 3262, 3268, 3270, 3272, 3274, 3277, 3285, 3286, 3298, 3299, 3302, 3311, 3328, 3331, 3387, 3388,
  3390, 3396, 3398, 3400, 3402, 3405, 3426, 3427, 3430, 3439, 3458, 3459, 3535, 3540, 3544, 3551, 3558, 3567, 3570, 3571, 3636,
  3642, 3655, 3662, 3664, 3673, 3764, 3772, 3784, 3789, 3792, 3801, 3864, 3865, 3872, 3881, 3902, 3903, 3953, 3972, 3974, 3975,
  3981, 3991, 3993, 4028, 4139, 4158, 4160, 4169, 4182, 4185, 4190, 4192, 4194, 4196, 4199, 4205, 4209, 4212, 4226, 4237, 4239,
  4253, 4957, 4959, 5906, 5908, 5938, 5940, 5970, 5971, 6002, 6003, 6068, 6099, 6112, 6121, 6155, 6157, 6160, 6169, 6277, 6278,
  6432, 6443, 6448, 6459, 6470, 6479, 6608, 6617, 6679, 6683, 6741, 6750, 6752, 6780, 6783, 6793, 6800, 6809, 6832, 6845, 6912,
  6916, 6964, 6980, 6992, 7001, 7019, 7027, 7040, 7042, 7073, 7085, 7088, 7097, 7142, 7155, 7204, 7223, 7232, 7241, 7248, 7257,
  7376, 7378, 7380, 7400, 7415, 7417, 7616, 7673, 7675, 7679, 8255, 8256, 8400, 8412, 8421, 8432, 11503, 11505, 11744, 11775,
  12330, 12335, 12441, 12442, 42528, 42537, 42612, 42621, 42654, 42655, 42736, 42737, 43043, 43047, 43136, 43137, 43188, 43205,
  43216, 43225, 43232, 43249, 43263, 43273, 43302, 43309, 43335, 43347, 43392, 43395, 43443, 43456, 43472, 43481, 43504, 43513,
  43561, 43574, 43596, 43597, 43600, 43609, 43643, 43645, 43698, 43700, 43703, 43704, 43710, 43711, 43755, 43759, 43765, 43766,
  44003, 44010, 44012, 44013, 44016, 44025, 65024, 65039, 65056, 65071, 65075, 65076, 65101, 65103, 65296, 65305, 66422, 66426,
  66720, 66729, 68097, 68099, 68101, 68102, 68108, 68111, 68152, 68154, 68325, 68326, 68900, 68903, 68912, 68921, 69446, 69456,
  69632, 69634, 69688, 69702, 69734, 69743, 69759, 69762, 69808, 69818, 69872, 69881, 69888, 69890, 69927, 69940, 69942, 69951,
  69957, 69958, 70016, 70018, 70067, 70080, 70089, 70092, 70096, 70105, 70188, 70199, 70367, 70378, 70384, 70393, 70400, 70403,
  70459, 70460, 70462, 70468, 70471, 70472, 70475, 70477, 70498, 70499, 70502, 70508, 70512, 70516, 70709, 70726, 70736, 70745,
  70832, 70851, 70864, 70873, 71087, 71093, 71096, 71104, 71132, 71133, 71216, 71232, 71248, 71257, 71339, 71351, 71360, 71369,
  71453, 71467, 71472, 71481, 71724, 71738, 71904, 71913, 72145, 72151, 72154, 72160, 72193, 72202, 72243, 72249, 72251, 72254,
  72273, 72283, 72330, 72345, 72751, 72758, 72760, 72767, 72784, 72793, 72850, 72871, 72873, 72886, 73009, 73014, 73020, 73021,
  73023, 73029, 73040, 73049, 73098, 73102, 73104, 73105, 73107, 73111, 73120, 73129, 73459, 73462, 92768, 92777, 92912, 92916,
  92976, 92982, 93008, 93017, 94033, 94087, 94095, 94098, 113821, 113822, 119141, 119145, 119149, 119154, 119163, 119170, 119173,
  119179, 119210, 119213, 119362, 119364, 120782, 120831, 121344, 121398, 121403, 121452, 121499, 121503, 121505, 121519, 122880,
  122886, 122888, 122904, 122907, 122913, 122915, 122916, 122918, 122922, 123184, 123190, 123200, 123209, 123628, 123641, 125136,
  125142, 125252, 125258, 125264, 125273,
];


// Build bytecode table

const CodePointClass = {
  EndOfInput: 1,
  Symbol: 2,
  Identifier: 3,
  Number: 4,
  NewLine: 5,
  Space: 6,
  HorizontalTab: 7,
  UnicodeIdStart: 32,
  UnicodeIdCont: 64,
}


function aii(table: Uint8Array, value: number, indices: number[], indice_len: number): Uint8Array {
  let i = 0;

  while (i < indice_len) {
    let indice = indices[i];

    table[indice] |= value;

    i += 1;
  }

  return table
}


function air(table: Uint8Array, value: number, indices: number[], indice_len: number): Uint8Array {
  let i = 0;

  let a: number[] = [];

  while (i < indice_len) {
    let r1 = indices[i];

    let r2 = indices[i + 1];

    let size = r2 + 1 - r1;

    let j = 0;

    while (j < size) {
      a[j] = r1 + j;

      j += 1;
    }

    table = aii(table, value, a, size);

    i += 2;
  }

  return table
}


function char_lu_table_init(): Uint8Array {
  // Unicode character class definitions

  let jump_table = new Uint8Array(382976);

  jump_table = air(jump_table, CodePointClass.Identifier, UNI_ID_START_RANGES, UNI_ID_START_RANGES.length);

  jump_table = aii(jump_table, CodePointClass.Identifier, UNI_ID_START_INDICES, UNI_ID_START_INDICES.length);

  // 4. SPACE
  let t = [32, 0xA0, 0x2002, 0x2003, 0x2004, 0x3000];

  jump_table = aii(jump_table, CodePointClass.Space, t, 6);

  // 4. TAB
  let b = [9];

  jump_table = aii(jump_table, CodePointClass.HorizontalTab, b, 1);

  // 8. CARRIAGE RETURN
  let c = [13];

  jump_table = aii(jump_table, CodePointClass.NewLine, c, 1);

  // 8. LINE FEED
  let d = [10];

  jump_table = aii(jump_table, CodePointClass.NewLine, d, 1);

  // 16. Number
  let e = [48, 57];

  jump_table = air(jump_table, CodePointClass.Number, e, 2);

  // Add Unicode Identifier Classes
  let f = [65, 90, 97, 122];

  jump_table = air(jump_table, CodePointClass.UnicodeIdStart, f, 4);

  let i = 0;

  while (i < jump_table.length) {
    if (jump_table[i] == 0) {
      jump_table[i] = CodePointClass.Symbol;
    }

    i += 1;
  }

  jump_table = air(jump_table, CodePointClass.UnicodeIdStart, UNI_ID_START_RANGES, UNI_ID_START_RANGES.length);

  jump_table = aii(jump_table, CodePointClass.UnicodeIdStart, UNI_ID_START_INDICES, UNI_ID_START_INDICES.length);

  jump_table = air(jump_table, CodePointClass.UnicodeIdCont, UNI_ID_CONT_RANGES, UNI_ID_CONT_RANGES.length);

  jump_table = aii(jump_table, CodePointClass.UnicodeIdCont, UNI_ID_CONT_DISCRETE, UNI_ID_CONT_DISCRETE.length);

  return jump_table
}

const CHAR_LU_TABLE = char_lu_table_init();

function get_token_class_from_codepoint(codepoint: number): number {
  return (CHAR_LU_TABLE[codepoint] & 0x1F)
}