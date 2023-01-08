# Sherpa Parser Generator

Generally, the majority of parser compilers, such as ANTLR, Yacc, & Bison, build parsers that confirm to a single algorithmic parsing principle. For instance, Yacc is a [LALR(1)](https://en.wikipedia.org/wiki/LALR_parser) parser, which means that it is capable of understanding grammars that are LALR(0) and LALR(1), and produces parsers that use a table based algorithm to transition between LALR states. ANTLR4 is a [ALL(*)](https://www.antlr.org/papers/allstar-techreport.pdf) parser, which, though incredibly powerful, is still limited to grammars that do not contain indirect left recursion.
// TODO - Write the above in terms of types of PARSERS instead of types of ALGORITHMS.

Sherpa dispenses with the notion of a specific type of parser, and instead is able to apply different parsing techniques to different parts of a grammar to maximize flexibility in the description of parsable languages. In practice, it builds Recursive Descent parsers when the  grammar is LL, Recursive Descent/Ascent (RAD) parsers when needed, RAD + LR(&ast;) on occasions, RAD(CRUMB) + LR(&ast;) if configured to do so, and can even mix in some Packrat style parsing if one prefers. In addition, Sherpas intermediate representation intermediate parser representation is intentionally designed to be trivial written by hand, allowing more parsing behavior to be defined that falls outside the bounds of any particular algorithm or technique.

# Algorithms

Sherpa employs numerous algorithms to achieve parsing. Some are well known, others are custom designed. 

- Earley
- Breadcrumb
- LR(*) / Recursive Ascent

## Recursive Descent

- All parsers start in this mode. 
- Allows for trivial entry into any production

## Recursive Ascent / LR(*)

- A basic form that only targets rules with non-terminals in the home position
- Expanded when necessary to handle general LR states

## Earley

- Used to statically resolve ambiguities. Combined with breadcrumb to create parse forests if 
  needed. 
