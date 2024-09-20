/**
 * Primary interface for the radlr parser system.
 */

import { defineLanguageFacet, Language, LanguageSupport } from "@codemirror/language";
import { Input, NodeSet, Parser, PartialParse, Tree, TreeFragment, NodeType } from "@lezer/common";
import * as radlr from "js/radlr/radlr_wasm.js";
import { tags, Tag, styleTags, tagHighlighter } from '@lezer/highlight';
import { syntaxHighlighting, HighlightStyle, defaultHighlightStyle } from '@codemirror/language';
import { Diagnostic, setDiagnostics } from "@codemirror/lint";
import { Facet } from "@codemirror/state";
import { ViewPlugin, EditorView, ViewUpdate } from "@codemirror/view";
import { EventType, GrammarContext } from "./grammar_context";
import { set_grammar } from "../common/session_storage";

class RadlrParser extends Parser {

  names: string[];

  types: { [name: string]: number; } = {};

  nodeSet: NodeSet;

  ctx: GrammarContext;

  input: string = ""

  stack: any

  constructor(ctx: GrammarContext) {
    super();

    this.ctx = ctx;

    let names = radlr.get_nonterminal_names();
    names.push("token");
    this.nodeSet = new NodeSet(names.map((name: string, id: number) => {
      return NodeType.define({ id, top: name == "radlr::grammar", name: name.replace("::", "-") });
    })).extend(styleTags({
      "nonterminal_symbol!": tags.definitionKeyword,
      "terminal!": tags.string,
      "nonterminal/...": tags.definitionOperator,
      "class!": tags.className,
    }));

    this.names = radlr.get_nonterminal_names();
  }


  createParse(input: Input | any | string, fragments: TreeFragment[], ranges: { from: number, to: number; }[]): PartialParse {

    let input_string = input.read(ranges[0].from, ranges[0].to);

    if (input_string != this.input) {
      this.input = input_string;
      this.stack = radlr.get_codemirror_parse_tree(input_string);
      this.stack[this.stack.length - 4] = 4;
      this.ctx.addGrammar(input_string, "/");
    }

    let len = this.input.length;

    return {
      advance: () => {
        return Tree.build({
          topID: 0,
          buffer: this.stack,
          nodeSet: this.nodeSet

        });
      },
      stopAt(pos: number) {
        console.log("end", pos);
      },
      parsedPos: len,
      stoppedAt: len

    };
  }
}

/// Responsible for building the ParserDB for a given grammar or producing semantic errors. 
/// Used in conjunction with the RadlrParser to produce a viable ParserBase.
function RadlrLinter(ctx: GrammarContext): any[] {

  let linter = ViewPlugin.fromClass(class {

    view: EditorView

    ctx: GrammarContext

    timeout: number

    timeout_handle: number = -1

    force_refresh: boolean = false;

    need_linting: boolean = true

    _lint: any

    constructor(view: EditorView) {
      this.view = view;
      this.ctx = ctx;
      this.timeout = 200;
      this._lint = this.lint.bind(this);
      this.ctx.addListener(EventType.NewErrors, () => {
        this.need_linting = true;
        this.lint();
      });
      this.ctx.addListener(EventType.DBCreated, () => {
        this.need_linting = true;
        this.lint();
      });
    }

    lint() {
      if (!this.need_linting)
        return;

      this.timeout_handle = -1;
      this.need_linting = false;

      let { view } = this;

      set_grammar(view.state.doc.toString());

      let messages: Diagnostic[] = [];

      convertPosErrorsToDiagnostics(ctx.parse_errors, "grammar", messages);

      convertPosErrorsToDiagnostics(ctx.parser_errors, "parser", messages);

      convertPosErrorsToDiagnostics(ctx.db_errors, "semantic-evaluator", messages);

      this.view.dispatch(setDiagnostics(this.view.state, messages))

    }

    update(update: ViewUpdate) {
      let config = update.state.facet(linterFacet);
      let start = update.startState.facet(linterFacet);

      if (update.docChanged || config != start || this.need_linting) {
        this.need_linting = true;
        if (this.timeout_handle) {
          clearTimeout(this.timeout_handle);
        }
        if (this.force_refresh) {
          this.lint()
          this.force_refresh = false;
          this.timeout_handle = -1;
        } else {
          this.timeout_handle = setTimeout(this._lint, this.timeout)
        }
      }
    }

    destroy() {
      console.log("destroy");
    }
  });

  let linterFacet = Facet.define({
    combine() {
      return {
        sources: [() => { }],
        needsRefresh: true,
      }
    },
    enables: linter
  })

  return [linterFacet.of({})]
}



function convertPosErrorsToDiagnostics(e: radlr.PositionedErrors | radlr.JSRadlrSourceError[], source: string, messages: Diagnostic[] = []): Diagnostic[] {
  for (let i = 0; i < e.length; i++) {
    let error = (Array.isArray(e)) ? e[i] : e.get_error_at(i);;
    if (error) {
      messages.push({
        from: error.start_offset,
        to: error.end_offset,
        severity: "error",
        source,
        message: error.message,
      });
    }
  }

  return messages;
}

export function radlrLang(ctx: GrammarContext) {
  let lang_sys = new RadlrParser(ctx);
  return new LanguageSupport(
    new Language(defineLanguageFacet({ commentTokens: { block: { open: "/*", close: "*/" } } }),
      lang_sys, [
      RadlrLinter(ctx),
      syntaxHighlighting(
        HighlightStyle.define([
          {
            tag: tags.definitionOperator,
            class: "syn-prod"
          },
          {
            tag: tags.definitionKeyword,
            class: "syn-prod-id"
          },
          {
            tag: tags.string,
            class: "syn-term"
          },
          {
            tag: tags.className,
            class: "syn-class"
          }
        ]))
    ],
      "radlr"), [
  ]);
}

