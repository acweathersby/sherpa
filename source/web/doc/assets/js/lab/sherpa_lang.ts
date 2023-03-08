import { defineLanguageFacet, Language, LanguageSupport } from "@codemirror/language";
import { Input, NodeSet, Parser, PartialParse, Tree, TreeFragment, NodeType } from "@lezer/common";
import init_sherpa, { compile_grammar, JournalWrap, JSGrammarParser, get_codemirror_parse_tree, get_production_names } from "js/sherpa/sherpa_wasm.js";
import { tags, Tag, styleTags, tagHighlighter } from '@lezer/highlight';
import { syntaxHighlighting, HighlightStyle, defaultHighlightStyle } from '@codemirror/language';

class SherpaParser extends Parser {

    names: string[];
    types: { [name: string]: number; } = {};

    nodeSet: NodeSet;

    constructor() {
        super();
        let names = get_production_names();
        names.push("token");
        this.nodeSet = new NodeSet(names.map((name: string, id: number) => {
            return NodeType.define({ id, top: name == "sherpa::grammar", name: name.replace("::", "-") });
        })).extend(styleTags({
            "sherpa_symbol-production_symbol!": tags.definitionKeyword,
            "sherpa_symbol-terminal!": tags.string,
            "sherpa-production/...": tags.definitionOperator,
            "sherpa_symbol-class!": tags.className,
        }));

        this.names = get_production_names();
    }

    createParse(input: Input | any | string, fragments: TreeFragment[], ranges: { from: number, to: number; }[]): PartialParse {

        let input_string = input.read(ranges[0].from, ranges[0].to);
        let parser = JSGrammarParser.new(input_string);
        let stack = get_codemirror_parse_tree(input_string);

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

export function sherpaLang() {
    return new LanguageSupport(
        new Language(defineLanguageFacet({ commentTokens: { block: { open: "/*", close: "*/" } } }),
            new SherpaParser(), [
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
