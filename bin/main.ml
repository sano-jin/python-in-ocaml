(* main.ml *)

open Eval
open Util
open Syntax
open Object

let base_env =
  [ ref [ ("object", object_class_obj_ref); ("None", ref VoidVal) ] ]

let read_and_parse filename =
  Parsing.parse_with_error filename @@ read_file filename

let system = read_and_parse "/Users/sano/work/python-in-ocaml/lib/system.py"

let () =
  ignore @@ eval_stmt [] base_env @@ Seq (system, read_and_parse Sys.argv.(1))
