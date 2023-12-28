# Radlr CLI

## How to get it

#### Github Releases:
```

```

## Usage

---

### `radlr build` 

Build a parser from one or more grammar files. Each grammar source is compiled into a separate parser source file. 

#### Example

```sh
$ radlr build ./the-best-language-ever.sp
```

#### Command Format

```
radlr build [Arguments]? <.sg Source File>+
```

#### Arguments

- `--type | -t <type>`
    Either `bytecode` or `llvm`
- `--o <Path>` 
    Path to an output
- `--ast`
    Output AST generation code alongside the parser code
- `--lang | -l <Language>`
    Output language for parser - [(r)ust] | [(js) | javascript]

##### LLVM Specific Arguments

These arguments are active when `--type` is set to `llvm`

- `--clang-path <Path | Alias> ` 
    Alias name or path to the `clang` executable
- `--llvm-ar <Path | Alias>`
    Alias or path to the `llvm-ar` executable
- `--lto`
    enables link-time-optimization [LTO] for LLVM generated object files
- `--ir-text`
    output a text-based version of the LLVM ir of the parser. This is purely
    for reference purposes, and is not used in the compilation of the parser object
    file.

---
### `radlr disassemble`

Produce a *"disassembly"* file of the parser bytecode of a grammar.

#### Command Format

```
radlr disassemble [Arguments]? <.sg Source File>+
```
-- Args 

 # License 

 The core Radlr source code is licensed under [GNU-GPLv3](../../../LICENSE.md). 

 The runtime source code is licensed under [GNU-LGPLv3](../../../LICENSE.md). 

 All generated code is MIT licensed.