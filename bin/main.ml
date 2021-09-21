(* main.ml *)

open Eval
open Util
open Syntax

let preprocess_newline_in_exp paren_n = function
  | '(' -> (succ paren_n, '(')
  | ')' -> (pred paren_n, ')')
  | '\n' -> (paren_n, if paren_n > 0 then ' ' else '\n')
  | c -> (paren_n, c)

let preprocess_newlines_in_exp =
  implode <. snd <. List.fold_left_map preprocess_newline_in_exp 0 <. explode

let () =
  ignore
  @@ eval_stmt ([], [ ref [ ("pass", ref VoidVal) ] ])
  @@ Parsing.parse_with_error
  @@ read_file Sys.argv.(1)
