---
title: "Sherpa | Docs"
description: "Sherpa Tutorials"
draft: false
docsroot: true
---

# Tutorials

## Your First Grammar

So you got some text data:

```
This is some data that needs to be parsed
```

# The Grammar

This tutorial will take you through process of creating a parser for the toy language `sherpie`, which 
looks something like this:

```
print = msg:str {   
    sys::io::say(msg)
}
print "Hello, how would you like some kandy? (yes/no)"
print ( 
    [ 
        yes:"Here's your candy" 
        no:"Well I don't have some anyway" 
    ][sys::io::get()] 
)
```

First things first we need to create a grammar. A grammar formally defines a set of specific actions that direct the parser on what to recognize. In a more formal definition the grammar defines the set of strings that can be created by language of that grammar. You might already be familier with regular expressions, or regex, where a distinct set of symbols is used to direct the regex engine in recognizing or rejecting an input. 

> `/[\w]+/` - The regex that matches any wordlike sequence of characters

In much the same way, a Sherpa grammar contains symbols that direct the parser in matching inputs.

> `<> start > ( c:id | c:num )(+)` - Equivalant to the above regex

Sherpa has a distinct set of symbols that can be used to match a variety of characters and character sequeneces. More information on them can be found [here](about:blank). Additionally, a review of regex can make you more familiar of the.  Unlike regex, sherpa grammars can match more than just regular expressions. 

> Inputs such as `{ { {  } } }` cannot be reliably parsed using regular expressions, but are no problem for more powerfull parser like Sherpa.

For our purpose, we'll want to define different parts of an input as having special meanings, such as matching identifiers, brackets, functions, and statements. We can define specific sets of symbols within rules, and give those rules a specific name. For instance to define our identifiers we can add 

```
<> identifier > c:id ( c:id | c:num | '-' | '_' )(*)
```

This will match string like `name`, ``