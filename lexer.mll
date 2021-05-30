(* Lexer *)

{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

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
  | "return"  { RETURN }

  (* variable *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* end of file *)
  | eof       { EOF }

  (* spaces *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
