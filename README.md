# go-in-ocaml

A very small subset of Go (like) language interpreter written in OCaml.

## How to use

1. run `make`
2. run `./go`.
3. run `Main.exec "test/sample.imp";;`
   and so on...

## Modules

- main.ml
  - The very main program.
  - Run the `run` function to test.
- eval.ml
  - The evaluator.
- syntax.ml
  - The definition of the syntax.
- lexer.mll
  - A file to pass to the ocamllex.
- parser.mly
  - A file to pass to the ocamlyacc.
