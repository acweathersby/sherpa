# Sherpa

Sherpa is new type of parse generator that is 

What sherpa is:

- No Lexer
Sherpa dispenses with the notion of a separate lexer process by allowing arbitrary matching on three low level classes of data: bytes, UTF8 codepoints, UTF character classes. 

- Algorithm Agnostic
- Low Level


Sherpa is parse algorithm agnostic. At its core is a parser programming language that is design simplify the creation
of parsers that can handle a large number of grammatical constructs. It is able to describe (G)LL, (G)LR, Recursive Descent,
Recursive Ascent, Pratt, and Peg parser, to name few common ones. Not only that, the language design allows arbitrary mixing
of these algorithms. 

Sherpa also provides a general AST  AST specification language that allows both the language specification and its AST structures. 

It provides a flexible way to defining parsers through language grammar, AST specifications, and IR code definitions. It can target LLVM, allowing it to be used in languages other Rust (eventually including Python, C++, and Js). 

## Usage 

> Sherpa compiler requires an installation of the LLVM library to build native 
> parsers. Please review the [LLVM parsers guide](./doc/llvm_parser_guide.md) to make sure you're able to build
> these type of parsers. 

Currently Sherpa is only available as a direct download of the Github Repo . It can be included in existing cargo projects by adding the following to the Cargo.toml file:

```toml

[build-dependencies]
sherpa = { git="https://github.com/acweathersby/sherpa", branch="release" }

[dependencies]
sherpa_runtime = { git="https://github.com/acweathersby/sherpa", branch="release" }

```
> `v1.*.*+` releases of Sherpa will be made available through crates.io

### CLI

#### Basic usage

```bash
sherpa build --rust --ast -o ./parser/ ./source_grammar.sg 
```

Checkout the [CLI README](./source/app/cli/README.md) for command line based operation of Sherpa. 

## Learn More

- [Using Sherpa](doc/introductory_tutorial.md)

- [What is a parser generator](doc/parser_generator.md)

- [Creating AST source code with AScript](doc/ascript.md)

## License

The source code of Sherpa is licensed under the [GPL-v3](./LICENSE.md) license. 

All artifacts produced by the Sherpa compiler are licensed under the [MIT](./ARTIFACT_LICENSE.md) license.
