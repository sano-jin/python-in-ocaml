# imp-in-ocaml

An IMP (very simple imperative language) interpreter written in OCaml.

## How to use

1. run `make`
2. run `./imp`.
3. run `Main.run "X := 4; Y := 1; Z := 0; while (Z < X) { Y := 2 * Y; Z := Z + 1 }" ;;`
   and so on...

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
