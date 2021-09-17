open Lexing

(* parse : string -> stmt *)
let parse_with_error str =
  let lexbuf = Lexing.from_string str in
  try Parser.main Lexer.token lexbuf with
  | Lexer.SyntaxError msg ->
      prerr_endline msg;
      exit (-1)
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, position %d.\n" pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1);
      exit (-1)
