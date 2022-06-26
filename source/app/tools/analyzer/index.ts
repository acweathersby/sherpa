
import { assign_peek, ByteWriter, complete, scanner_state_mask, StateIterator, state_index_mask, Token } from "@hctoolkit/runtime";
import { FileHandle } from "fs/promises";
import { FunctionMaps } from "./ast.js";
import { Bytecode, Entrypoint, ExpectedTokenLookup, ReduceNames, TokenLookup } from "./parser_data.js";

const input = `<a -href www.google.com>`;
assign_peek((f: number, iter: StateIterator) => {
    const index = f & state_index_mask;
    if (f & scanner_state_mask)
        return;

    let expected = "";
    let token = "";

    const off = iter.tokens[1].byte_offset;
    const len = iter.tokens[1].byte_length;
    const offb = iter.tokens[0].byte_offset;
    const lenb = off - offb;

    if (ExpectedTokenLookup.has(index)) {
        expected = <string>ExpectedTokenLookup.get(index)?.map(v => TokenLookup.get(v)).join(" ");
    }
    if (iter.reader.offset_at_end(off)) {
        token = "<EOF>";
    } else if (f & scanner_state_mask) {
        token = String.fromCodePoint(iter.reader.byte() ?? 26);
    } else {
        token = TokenLookup.get(iter.tokens[1].type || 0) ?? "";
    }

    const slice = input.slice(off - 5, off) + "|>" + input.slice(off, off + len) + "<|" + input.slice(off + len);
    const sliceB = input.slice(offb - 5, offb) + "|>" + input.slice(offb, offb + lenb) + "<|" + input.slice(offb + lenb);

    console.log(`\n state ${((f & 0xFFFF) * 4).toString(16)} - [${token}] tokpos:${off} bufpos:${iter.reader.cursor} \n \n${sliceB.replace(/\n/, "\\n")}\n${slice.replace(/\n/, "\\n")} \n expected: [ ${expected} ]`);
    console.log(`\n PID [ ${iter.production_id} ]`);
    console.log("");
});

const { result, err } = complete<Token>(input, Entrypoint.element_block, Bytecode, FunctionMaps, ReduceNames);

if (err) {

    const index = err.last_state & state_index_mask;

    let expected = "";
    let token = new Token(input, err.tk_length || 1, err.tk_offset, 0);

    if (ExpectedTokenLookup.has(index)) {
        expected = <string>ExpectedTokenLookup.get(index)?.map(v => TokenLookup.get(v)).join(" ");
    }
    console.log({
        err,
        expected,
        index: (index * 4).toString(16),

    });

    token.throw(`Expected [${expected}], encountered [${err.tk_offset >= input.length ? "<eof>" :
        token

        }]`);
}

console.dir({ result: result }, { depth: null });

class FileWriter extends ByteWriter {
    handle: FileHandle;
    constructor(handle: FileHandle) {

        super();

        this.handle = handle;
    }

    purge() {
        this.handle.write(new Uint8Array(this.get_chunk()));
        this.cursor = 0;
    }
}
