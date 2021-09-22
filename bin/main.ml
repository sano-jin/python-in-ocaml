(* main.ml *)

open Eval
open Util
open Syntax
open Object

let () =
  ignore
  @@ eval_stmt
       ([], [ ref [ ("pass", ref VoidVal); ("object", object_class_obj_ref) ] ])
  @@ Parsing.parse_with_error
  @@ read_file Sys.argv.(1)
