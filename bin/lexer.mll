(* Lexer *)

{
  open Parser
  exception SyntaxError of string

  open Lexing_aux

}

let space = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let newline = '\r' | '\n' | "\r\n"

(* 改行後のスペースを indent で読んだ後に呼ばれる Lexer *)
rule token = parse
  (* Number *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }
  
  (* Operators *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '<'       { LT }
  | '>'       { GT }
  | ':'       { COL }
  | '.'       { DOT }
  | ','       { COMMA }
  | '='       { EQ }

  (* Parentheses *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  
  (* reserved names *)
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "while"    { WHILE }
  | "pass"     { PASS }
  | "if"       { IF }
  | "lambda"   { LAMBDA }
  | "def"      { DEF }
  | "class"    { CLASS }
  | "nonlocal" { NONLOCAL }
  | "return"   { RETURN }

  (* variable *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* end of file *)
  | eof       { EOF }

  (* spaces *)
  | space+    { token lexbuf }

  (* new line. call the [indent] tokenizer *)
  | newline  { Lexing.new_line lexbuf; indent lexbuf }

  (* comments *)
  | '#' [^ '\n']*  { token lexbuf }

  (* string *)
  | ''' [^ '\'']* '\''
    { let str = Lexing.lexeme lexbuf in
      STRING (String.sub str 1 @@ String.length str - 2)
    }

  | _
    {
      let message = Printf.sprintf
        "Unknown token '%s' near line %d (near characters %d-%d)"
        (Lexing.lexeme lexbuf)
        (pred lexbuf.lex_curr_p.pos_lnum)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      raise @@ SyntaxError message
    }


(* 改行があった場合に直後に呼ばれる Lexer *)
and indent = parse
  (* blank line *)
  | space* newline { Lexing.new_line lexbuf; indent lexbuf }

  (* blank line with a comment *)
  | space* '#' [^ '\n']* newline { Lexing.new_line lexbuf; indent lexbuf }

  (* indent (assuming that the next comming token is not just a space/newline/comment) *)
  | space*
    { let indent_level =
        let pos = lexbuf.lex_curr_p in
        (* the number of characters from the beginning of the line*)
        pos.pos_cnum - pos.pos_bol
      in
      emit_indent indent_level
     }


