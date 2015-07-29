## YATP - Yet Another TeX Parser

This is an experiment in writing a TeX parser in Haskell. The overall goal is
to understand better how TeX works interally, and to be able to write tests to
test the [KaTeX](https://khan.github.io/KaTeX/) library against.

To that end, I'm not sure whether this will try to reach processing of boxes,
or will merely stop at the parsing of TeX syntax. Also, the code will
eventually be used with Haste to compile to JavaScript so testing against KaTeX
can be done.

### Running

Currently, the executable this produces does nothing. You can build it with
`cabal build`, or, more helpfully, start a GHCi session to tinker around with
the Lexer using `cabal repl`.

### Testing

There are currently some unit tests written using HUnit to test the lexer.
These tests can be built and run using `cabal run tex-parser-tests`.
