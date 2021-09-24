(** Parser の top-level *)

open Lexing
open Parser
open Util

(** [TOKENS] を無数の [token] に展開しながら出力する tokenizer *)
let token =
  let tokens = ref [] in
  let rec helper lexbuf =
    match !tokens with
    | [] -> (
        match Lexer.token lexbuf with
        | TOKENS ts ->
            tokens := ts;
            helper lexbuf
        | x -> x)
    | TOKENS _ :: _ -> failwith "nesting TOKENS token is not allowed"
    | h :: t ->
        tokens := t;
        h
  in
  helper

(** parse : string -> stmt *)
let parse_with_error filename str =
  let lexbuf = Lexing.from_string @@ "\n" ^ str ^ "\n" in
  try Parser.main token lexbuf with
  | Lexer.SyntaxError msg ->
      prerr_endline @@ msg ^ " in " ^ filename;
      exit (-1)
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, position %d in %s\n"
        (pred pos.pos_lnum)
        (pos.pos_cnum - pos.pos_bol + 1)
        filename;
      exit (-1)

let read_and_parse filename = parse_with_error filename @@ read_file filename
