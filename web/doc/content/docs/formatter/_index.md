---
title: "Tutorial"
description: "Radlr Documentation"
draft: false
weight: 1
---

# The Recurse Formatter

Recurse uses a specialized formatting language to construct outputs in a desired language, such as AST construction code and binary parser frontends. This language is called @@, or At-At, due to it's excessive use of the @ symbol to define formatting commands. Here is an example of @@: 
```at-at
#two { two }

#nums @n:num {
  match @n {
    4 { four }
  }
}

#t @val:num { 
   match @val { 1 { @" one " } 2 { #two() } 3 { @[1 + 2] } 1+3 { 4 } { zero } }
}

#t(1) @1 #t(2) @2 #t(3) @3 #t(4) @4 #t(1000)
```

> The formal definition of At-At can be found [here](). 

# @@ Reference

- Function 


 - Function Argument

<var_name> : <type> 

-- Function Argument Type

  `obj | num | int | flt | str`



Functions can execute any code type that is available in the top level scrip.





The language a functional style programming language. It is not quite Turing complete, but it is sufficiently powerful enough to handle the majority of formatting task required by Recurse.

## API

# Ascript Formatting API

## DB

- `structs` - Stores all struct definitions, of type `AscriptStruct`
- `rules` - Stores an ordered list of rule, which map each parser non-terminal in a parser database to a single semantic action that runs upon the reduction of a set of symbols to that non-terminal.

## AscriptStruct

Represents a single node object in an AST graph, with a particular set of properties derived from the symbols of one or more parser rules. An AscriptStruct may also contain properties that are not derived by parsed symbols, but instead are directly assigned when a particular rule generates the AscriptStruct object.

 - `name` - The name of the struct
 - `props` - A mapping of property names and value types objects 


## AscriptProp

- `name` - The name of the property
- `type` - The type of object the property
- `optional` - Whether or not the property might not be assigned when instantiated. This may occur if the property is mapped to symbol that is not present in some derivations of rule-production.


## AscriptRule

Fundamentally, all symbols in a rule are comprised of either a token representing a character sequence, or a list or vector of tokens which result from list productions `A+` or `A(+delimiter?)`. When the AST type of non-terminal is calculated, it's defualt value is the value of the last symbol in its rules. This is often a termina, but in the case the symbol is a non-terminal, then the type of that non-terminal symbol becomes the type of the host non-terminal

An AscriptRule reduces the symmbols of a grammar rule into a single entity, usually a struct / class instance / object, that then represents the type of the non-terminal the rule produces. There a several rule types that can be generated

### StructRule
### ExpressionRule
### ListInitialRule
### ListContinueRule
### LastSymbolRule
### InvalidRule


## AscriptType

## GraphNode

GraphNodes represent expressions that operate on the symbols encountered in a rule, transforming and combining them to facilitate their usage in the target language.

### Add 
- `right` -
- `left` -
`

### Sub

### Mul 

### Div

### Map

### Vec

### Str

### Bool

### Num

### Tok

### Sym

### TokSym

### TokRule

### Undefined

An `Undefined` GraphNode represent an error in evaluating the AST definition on a rule. Rules that have an `Undefined` 