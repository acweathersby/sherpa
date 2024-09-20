import { ParseActionType, ParserContext, next } from "./bytecode_kernel";

class Token {
  start: number;
  end: number;
  str: string;
  id: number;

  constructor(start: number, end: number, id: number, str: string) {
    this.start = start;
    this.end = end;
    this.str = str;
    this.id = id;
  }

  toString(): string {
    return this.str.slice(this.start, this.end);
  }
}

export function ast_parser(
  bc: Uint8Array,
  input: string,
  entry: [string, number],
  rules: ((symbols: any[], tokens: any[], nterm_tok: any) => any)[]
) {

  let ctx = new ParserContext();
  ctx.set_entrypoint(entry);

  let symbols: any[] = [];
  let tokens: Token[] = [];

  outer: while (true) {
    let action = next(ctx, input, bc);
    if (!action) break;
    switch (action.type) {

      case ParseActionType.Error: {
        throw action.payload;
      }

      case ParseActionType.Shift: {
        let {
          byte_offset,
          byte_length,
          token_id,
        } = action.payload;


        let token = new Token(byte_offset, byte_offset + byte_length, token_id, input);

        symbols.push(token);
        tokens.push(token);

        break;
      }

      case ParseActionType.Skip: {
        break;
      }

      case ParseActionType.Reduce: {

        let { rule_id, symbol_count } = action.payload;

        console.log({ rule_id })

        let rule_handler = rules[rule_id];

        let rule_symbols = symbols.slice(symbols.length - symbol_count);
        let rule_tokens = tokens.slice(tokens.length - symbol_count)

        symbols.length -= symbol_count;
        tokens.length -= symbol_count;

        let rule_token = new Token(rule_tokens[0].start, rule_tokens[rule_tokens.length - 1].end, 0, input);

        tokens.push(rule_token);
        symbols.push(rule_handler(rule_symbols, rule_tokens, rule_token));

        break;
      }

      case ParseActionType.Accept: {
        break outer;
      }
    }
  };

  return symbols[0];
}