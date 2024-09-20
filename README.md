# Radlr

A flexible parser compiler for experimentation and rapid syntax development. 

- Syntax highlighting
- Code Formatting
- Predictive and recovering syntax analysis
- Automatic generation of AST definitions
- Simple shift/reduce parser interface

- Support for ambiguous grammars (though not recommended)
- Context sensitive lexing (scanning)
- Parsing algorithm detection
- Arbitrary lookahead


## Whats in a name?

As can be inferred, RADLR is a **r**ecursive **d**escent and **a**scent parser compiler; the LR is just to make it match common naming conventions (though it is capable of producing parsers that are equivalent to LR algorithms)

# State of the code

My interest have moved away from development of Radlr and towards using it io create other languages and tools. At this point it is still incomplete, and if or when I do restart active development of Radlr, it will be to reimplement it in a new language (preferably one in which it is itself implemented in terms of Radlr; yeah, turtles all the way down).