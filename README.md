# Tip
A small, Haskell-like, purely functional programming language.

This repo contains the compiler frontend that parses a source file and emits a (typed) AST in Haskell's show notation.

Note that the project is highly experimental and mostly an attempt to gain a better understanding of how type checking and type inference works in the context of lambda calculus.

The type system is a basic Hindley-Milner system that is heavily inspired by Christoph Hegemann's "Type Inference From Scratch" talk.

## Example

```
let const = \x -> \y -> x in (const "Hello!")
```

The frontend then yields the following, fully-typed expression:

```
(let const = (\x -> (\y -> x :: _a) :: (_b -> _a)) :: (_a -> (_b -> _a)) in (const :: (_a -> (_b -> _a)) "Hello!" :: String) :: (_b -> String)) :: (_b -> String)
```

## Running
`stack run -- [input file] [output file]`

For example:

`stack run -- examples/HelloWorld.tip output/HelloWorld`
