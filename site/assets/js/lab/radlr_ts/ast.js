

/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.1-beta2
/// - **SOURCE**: /home/work/projects/lib_radlr/grammars/v2_0_0/grammar.sg
///
/// #### WARNING:
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### License:
/// Copyright (c) 2020-2024 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the 'Software'), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE

class Token {
  trim(start, end) {
    return new Token();
  }
};




export class Rule { constructor(sym, name, rule_id,) { this.sym = sym; this.name = name; this.rule_id = rule_id; } }

export class RuleId { constructor(index,) { this.index = index; } }

export class SymSet { constructor(col, syms,) { this.col = col; this.syms = syms; } }


function rule_0(nodes, tokens, nterm_tok) { var out0 = nodes[0]; return out0; }

function rule_1(nodes, tokens, nterm_tok) { var out_1_0 = nodes[0]; var out0 = [out_1_0]; return out0; }

function rule_2(nodes, tokens, nterm_tok) { var out_r0 = nodes[1]; var out_l0 = nodes[0]; var out0 = out_l0; out0.push(out_r0); return out0; }

function rule_3(nodes, tokens, nterm_tok) {
  var sym0 = nodes[2];


  var name1 = nodes[0];

  if (!(typeof name1 == "string")) throw ("Node is not a String!");
  var name0 = name1;


  var rule_id0 = nodes[1];


  return new Rule(sym0, name0, rule_id0);
}

function rule_4(nodes, tokens, nterm_tok) {
  var sym0 = nodes[1];


  var name1 = nodes[0];

  if (!(typeof name1 == "string")) throw ("Node is not a String!");
  var name0 = name1;


  return new Rule(sym0, name0, undefined);
}

function rule_5(nodes, tokens, nterm_tok) { var out_1_0 = nodes[0]; var out0 = [out_1_0]; return out0; }

function rule_6(nodes, tokens, nterm_tok) { var out_r0 = nodes[1]; var out_l0 = nodes[0]; var out0 = out_l0; out0.push(out_r0); return out0; }

function rule_7(nodes, tokens, nterm_tok) { var col0 = nodes[2]; var syms0 = nodes[1]; return new SymSet(col0, syms0); }

function rule_8(nodes, tokens, nterm_tok) { var syms0 = nodes[1]; return new SymSet(undefined, syms0); }

function rule_9(nodes, tokens, nterm_tok) { var out1 = tokens[0]; var out0 = parseFloat(out1.toString()); return out0; }

function rule_10(nodes, tokens, nterm_tok) { var out_1_0 = nodes[0]; var out0 = [out_1_0]; return out0; }

function rule_11(nodes, tokens, nterm_tok) { var out_r0 = nodes[2]; var out_l0 = nodes[0]; var out0 = out_l0; out0.push(out_r0); return out0; }

function rule_12(nodes, tokens, nterm_tok) { var out1 = tokens[0]; var out0 = out1.toString(); return out0; }

function rule_13(nodes, tokens, nterm_tok) { var index1 = tokens[1]; var index0 = parseFloat(index1.toString()); return new RuleId(index0); }

function rule_14(nodes, tokens, nterm_tok) { var out1 = tokens[0]; var out0 = out1.toString(); return out0; }



export const reduce_rules = [
  rule_0,
  rule_1,
  rule_2,
  rule_3,
  rule_4,
  rule_5,
  rule_6,
  rule_7,
  rule_8,
  rule_9,
  rule_10,
  rule_11,
  rule_12,
  rule_13,
  rule_14,
];