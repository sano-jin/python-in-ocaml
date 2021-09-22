(* main.ml *)

open Eval
open Util
open Syntax

let () =
  ignore
  @@ eval_stmt
       ( [],
         [
           ref
             [
               ("pass", ref VoidVal);
               ("object", ref @@ ObjectVal (ref object_variables));
             ];
         ] )
  @@ Parsing.parse_with_error
  @@ read_file Sys.argv.(1)
