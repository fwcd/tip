# Tip
A small, Haskell-like purely functional programming language.

This repo contains the compiler frontend that parses a source file and emits a (typed) AST in Haskell's show notation.

Note that the project is highly experimental and mostly an attempt to gain a better understanding of how type checking and type inference works in the context of lambda calculus.

## Example

```
let const = \x -> \y -> x in (const "Hello!")
```

## Running
`stack run -- [input file] [output file]`

For example:

`stack run -- examples/HelloWorld.tip output/HelloWorld`
