# python-in-ocaml
_Experimental_

A very small subset of Python written in OCaml.

## Getting Started
### Prerequisites
- [opam](https://opam.ocaml.org/)

### Installation
```bash
git clone https://github.com/sano-jin/python-in-ocaml
cd python-in-ocaml
opam install .
dune build
```

## Usage

run `./run example/sample1.py`


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
