# Radlr

The Swiss Army Knife of parser compilers.

Radlr is a general purpose parser compiler toolchain that provides a rich set of tools and features to easily create powerful parsers, upon which advanced language services, compilers, and other tooling can be built. Radlr uses a novel multi-paradigm parser compiler pipeline that can generate parsers that replicate the behavior of LL, LR, recursive descent, and recursive ascent algorithms, with unlimited lookahead, and the option to make general through [context splitting]().

Radlr provides a rich tooling system that allows for the creation of dedicated formatters, fuzzers, code hinters, and syntax highlighters with minimal effort. 

The primary goals of the Radlr project are to provide useful tools for

- Language design and anaylsis
- Syntax highlighting
- Linting and formatting
- AST and CST construction and traversal

## Want To Know More?

### Docs 

Details on all this and more can be found in  the 

Checkout out the online documentation [here](https://acweathersby.github.io/radlr/docs).

API documentation can be found here. 

### Explore the Lab

### Compiler

### Error Recovery

### CST and AST based language analysis

## Contribute

*TODO*

## License

Radlr is released with a split license structure. The main toolchain is licensed under the [GNU General Public License v3](./LICENSE.md), wherease generated and runtime code is released under Apache Licenses. This allows users to use Radlr parsers however they see fit, while protecting the integrity of the core project. 

## Similar Projects

| | Type | Site |  |
|:-:|:-:||
||||

### Recursive Ascent and Descent Compilers

### Earley Parser

- [Marpa](https://jeffreykegler.github.io/Marpa-web-site/)
- [Santiago](https://github.com/kamadorueda/santiago)

### LR/LL Parser Compilers
- [Antlr](https://github.com/antlr/antlr4) (Java (+ others)) - An incremental 
- [Bison](https://www.gnu.org/software/bison/) (C/C++ (+ others)) -  a general-purpose parser generator 
- [LALRPOP](https://github.com/lalrpop/lalrpop) (rust) - LR(1) parser generator for Rust
- [tree-sitter](https://github.com/tree-sitter/tree-sitter) (C & JavaScript (+ others)) - An incremental parsing system for programming tools, supporting error recovery 

### Parsing Expression Grammar (PEG) Compilers

- [Pest](https://github.com/pest-parser/pest) (rust) - The Elegant Parser
- [rust-peg](https://github.com/kevinmehall/rust-peg) (rust) - The Elegant Parser

### Abstract and Concrete Syntax Trees

- [Rowan](https://github.com/rust-analyzer/rowan) (rust) - lossless syntax trees


> `(+ others)` - These projects support programming language targets other than their host language.
