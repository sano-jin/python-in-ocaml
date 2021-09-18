(* Lexer *)

{
  open Parser
  exception SyntaxError of string
  let paren_depth = ref 0
  let indents = ref [0]
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
  | ':'       { COL }
  | ','       { COMMA }
  | '='       { EQ }
  | "=="      { EQEQ }

  (* Parentheses *)
  | '('       { incr paren_depth; LPAREN }
  | ')'       { decr paren_depth; RPAREN }
  
  (* reserved names *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "while"   { WHILE }
  | "for"     { FOR }
  | "def"     { DEF }
  | "return"  { RETURN }

  (* variable *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* end of file *)
  | eof       { EOF }

  (* indent *)
  | newline space*
    { Lexing.new_line lexbuf;
      if !paren_depth > 0 then token lexbuf (* Skip if sandwiched between parentheses *)
      else
        let white_space_n = pred lexbuf.lex_buffer_len in
        
      else INDENT (pred lexbuf.lex_buffer_len) (* indent *)
    }

  (* spaces *)
  | space+    { token lexbuf }


  (* comments *)
  | '#' [^ '\n']*  { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "Unknown token '%s' near line %d (near characters %d-%d)"
        (Lexing.lexeme lexbuf)
        (pred lexbuf.lex_curr_p.pos_lnum )
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      raise @@ SyntaxError message
    }
