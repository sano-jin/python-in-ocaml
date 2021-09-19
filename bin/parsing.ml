(** Parser の top-level *)

open Lexing
open Parser

(** [DEDENTS] を無数の [DEDENT] に展開しながら出力する tokenizer *)
let token =
  let dedents_n = ref 0 in
  fun lexbuf ->
    if !dedents_n > 0 then (
      decr dedents_n;
      DEDENT)
    else
      (* dedents_n = 0 *)
      match Lexer.token lexbuf with
      | Parser.DEDENTS n ->
          dedents_n := pred n;
          DEDENT
      | x -> x

(** parse : string -> stmt *)
let parse_with_error str =
  let lexbuf = Lexing.from_string @@ "\n" ^ str in
  try Parser.main token lexbuf with
  | Lexer.SyntaxError msg ->
      prerr_endline msg;
      exit (-1)
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, position %d.\n"
        (pred pos.pos_lnum)
        (pos.pos_cnum - pos.pos_bol + 1);
      exit (-1)
