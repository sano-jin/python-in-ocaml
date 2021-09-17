# go-in-ocaml

A Go (like) language interpreter written in OCaml.

## How to use

run `./run example/sample1.go`


## Modules

- main.ml
  - The very main program.
  - Run the `run` function to test.
- eval.ml
  - The evaluator.
  - Run `eval_command` with a `command` and an initial environment (usualy an empty list).
- syntax.ml
  - The definition of the syntax.
- lexer.mll
  - A file to pass to the ocamllex.
- parser.mly
  - A file to pass to the ocamlyacc.
