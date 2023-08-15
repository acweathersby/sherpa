/**
 * Primary interface for the sherpa parser system.
 */

import { defineLanguageFacet, Language, LanguageSupport } from "@codemirror/language";
import { Input, NodeSet, Parser, PartialParse, Tree, TreeFragment, NodeType } from "@lezer/common";
import * as sherpa from "js/sherpa/sherpa_wasm.js";
import { tags, Tag, styleTags, tagHighlighter } from '@lezer/highlight';
import { syntaxHighlighting, HighlightStyle, defaultHighlightStyle } from '@codemirror/language';
import { linter, Diagnostic } from "@codemirror/lint";
import { GrammarContext } from "./grammar_context";

class SherpaParser extends Parser {

    names: string[];

    types: { [name: string]: number; } = {};

    nodeSet: NodeSet;

    ctx: GrammarContext;

    constructor(ctx: GrammarContext) {
        super();

        this.ctx = ctx;

        let names = sherpa.get_production_names();
        names.push("token");
        this.nodeSet = new NodeSet(names.map((name: string, id: number) => {
            return NodeType.define({ id, top: name == "sherpa::grammar", name: name.replace("::", "-") });
        })).extend(styleTags({
            "sherpa_symbol-production_symbol!": tags.definitionKeyword,
            "sherpa_symbol-terminal!": tags.string,
            "sherpa-production/...": tags.definitionOperator,
            "sherpa_symbol-class!": tags.className,
        }));

        this.names = sherpa.get_production_names();
    }


    createParse(input: Input | any | string, fragments: TreeFragment[], ranges: { from: number, to: number; }[]): PartialParse {

        let input_string = input.read(ranges[0].from, ranges[0].to);

        let parser = sherpa.JSGrammarParser.new(input_string);
        let stack = sherpa.get_codemirror_parse_tree(input_string);

        this.ctx.addGrammar(input_string, "/");

        stack[stack.length - 4] = 4;

        let nodeSet = this.nodeSet;
        let len = input_string.length;

        return {
            advance() {
                return Tree.build({
                    topID: 0,
                    buffer: stack,
                    nodeSet

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
/// Used in conjunction with the SherpaParser to produce a viable ParserBase.
function SherpaLinter(ctx: GrammarContext) {
    return linter((view) => {
        console.log("Linting!")

        let messages: Diagnostic[] = [];

        let parser_errors = ctx.parse_errors;

        if (parser_errors.length > 0) {
            convertPosErrorsToDiagnostics(parser_errors, "parser", messages);
        }

        if (!ctx.createDB("/")) {
            convertPosErrorsToDiagnostics(ctx.db_errors, "semantic-evaluator", messages);
        } else {

            let db = ctx.db;

            if (!db)
                return messages;

            // Now that we have a db we can kick off jobs to further process
            // the data on to 
            console.log("DB Created")

            try {
                // Build the soup.
                let output = document.getElementById("ast-output");
                if (output) {
                    output.innerText = sherpa.create_rust_ast_output(db);
                }
            } catch (e) {
                if (e instanceof sherpa.PositionedErrors) {
                    convertPosErrorsToDiagnostics(e, "ast-compiler", messages);
                    e.free();
                } else {
                    console.log(e)
                }
            }
        }

        return messages;
    }, {
        delay: 130,
    })
}

function convertPosErrorsToDiagnostics(e: sherpa.PositionedErrors | sherpa.JSSherpaSourceError[], source: string, messages: Diagnostic[] = []): Diagnostic[] {
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

export function sherpaLang(ctx: GrammarContext) {
    let lang_sys = new SherpaParser(ctx);
    return new LanguageSupport(
        new Language(defineLanguageFacet({ commentTokens: { block: { open: "/*", close: "*/" } } }),
            lang_sys, [
            SherpaLinter(ctx),
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
            "sherpa"), [
    ]);
}

