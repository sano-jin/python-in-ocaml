(* Lexer *)

{
  open Parser
  exception SyntaxError of string
}

let space = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let newline = '\r' | '\n' | "\r\n"


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
  | ';'       { SEMICOL }
  | ','       { COMMA }
  | ":="      { ASSIGN }
  | '='       { EQ }

  (* Parentheses *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '{'       { LCBRA }
  | '}'       { RCBRA }
  
  (* reserved names *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "while"   { WHILE }
  | "func"    { FUNC }
  | "let"     { LET }
  | "rec"     { REC }
  | "return"  { RETURN }

  (* variable *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* end of file *)
  | eof       { EOF }

  (* spaces *)
  | space+    { token lexbuf }

  | newline  { Lexing.new_line lexbuf; token lexbuf }

  (* comments *)
  | '#' [^ '\n']*  { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "Unknown token '%s' near line %d (near characters %d-%d)"
        (Lexing.lexeme lexbuf)
        lexbuf.lex_curr_p.pos_lnum
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      raise @@ SyntaxError message
    }
