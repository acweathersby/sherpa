# Radlr

A parser compiler Swiss-army knife.

Radlr is a general purpose parser compiler toolchain that provides a rich set of features to easily create powerful parsers, upon which advanced language services, compilers, and other tooling can be built. Radlr uses a novel multi-paradigm parser compiler pipeline that can generate parsers that replicate the behavior of LL, LR, recursive descent, and recursive ascent algorithms, with unlimited lookahead, and the option to make general through parser context fork.

### Primary Features

- Language design and anaylsis
- Syntax highlighting
- Linting and formatting
- AST and CST construction and traversal

### Docs 

Checkout out the online documentation [here](https://rum-craft.github.io/radlr/docs).

## License

Radlr is released with a split license structure. The main toolchain is licensed under the [GNU General Public License v3](./LICENSE.md), wherease generated and runtime code is released under Apache Licenses. This allows users to use Radlr parsers however they see fit, while protecting the integrity of the core project. 
