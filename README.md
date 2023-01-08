# Sherpa

Sherpa is a parser generator written in Rust and LLVM that generates stack based hybrid parsers. 

It also provides an AST specification language that allows both the language specification and its AST structures. 

It provides a flexible way to defining parsers through GRAMMAR, AST, and IR code definitions. It can target LLVM, allowing it to be used in languages other Rust, including C/C++, Typescript/Javascript,  Python, and 

## Usage 

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
