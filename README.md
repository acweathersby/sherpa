# Sherpa

The recursive parser compiler. 

Recurse is 
- LL Compatible
- LR Compatible
- Peg Compatible
- Hybrid Ready

Sherpa is a general purpose parser compiler that provides a rich set of tools and features to easily create, use, and anaylize powerful parsers. Using an algorithm agnostic intermediate representation (IR) to define parsing methods, Sherpa is able to create parsers from a verietty of gammars, including context free LL and LR grammars and PEG grammars, as well as a limited set of context sensitive grammars. The Sherpa Grammar Syntax allows for quick and easy writing of programming languages, and provides direct access to the IR to create fully customizable parsers to suite any need. 

The primary goals of the Sherpa project are to privide useful tools for

- Language design and anaylsis
- Syntax highlighting
- Linting and formatting
- AST and CST construction and traversal

# Docs 

Checkout out the online documentation [here](https://acweathersby.github.io/sherpa/docs).

API documentation can be found here. 

# Contribute

> # *TODO*

# Similar Projects

## Recursive Ascent and Descent Compilers

## Earley Parser

- [Marpa](https://jeffreykegler.github.io/Marpa-web-site/)
- [Santiago](https://github.com/kamadorueda/santiago)

## LR/LL Parser Compilers
- [Antlr](https://github.com/antlr/antlr4) (Java (+ others)) - An incremental 
- [Bison](https://www.gnu.org/software/bison/) (C/C++ (+ others)) -  a general-purpose parser generator 
- [LALRPOP](https://github.com/lalrpop/lalrpop) (rust) - LR(1) parser generator for Rust
- [tree-sitter](https://github.com/tree-sitter/tree-sitter) (C & JavaScript (+ others)) - An incremental parsing system for programming tools, supporting error recovery 

## Parsing Expression Grammar (PEG) Compilers

- [Pest](https://github.com/pest-parser/pest) (rust) - The Elegant Parser
- [rust-peg](https://github.com/kevinmehall/rust-peg) (rust) - The Elegant Parser

## Abstract and Concrete Syntax Trees

- [Rowan](https://github.com/rust-analyzer/rowan) (rust) - lossless syntax trees


> `(+ others)` - These projects support programming language targets other than their host language.
