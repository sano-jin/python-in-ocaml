(* Parser *)
     
%{
  open Syntax
%}

%token <string> VAR	(* x, y, abc, ... *)
%token <int> INT	(* 0, 1, 2, ...  *)

(* operators *)
%token PLUS		(* '+' *)
%token MINUS		(* '-' *)
%token ASTERISK		(* '*' *)
%token LT		(* '<' *)
%token COL		(* ':' *)
%token COMMA		(* ',' *)
%token EQ		(* '=' *)
%token DELIMITER	(* '\n' *)
		       
(* Parentheses *)
%token LPAREN		(* '(' *)
%token RPAREN		(* ')' *)

(* Indentation *)
%token INDENT     
%token DEDENT     
%token BAD_DEDENT     
%token <token list> TOKENS	(* Zero or more TOKENs (NESTING THIS IS NOT ALLOWED) *)


(* reserved names *)
%token TRUE		(* "true"   *)
%token FALSE		(* "false"  *)
%token WHILE		(* "while"  *)
%token LAMBDA		(* "lambda" *)
%token DEF		(* "def"    *)
%token RETURN		(* "return" *)

(* End of file *)
%token EOF 

(* Operator associativity *)
%nonassoc LT
%left PLUS
%left ASTERISK



%start main
%type <Syntax.stmt> main

%%

(* Main part must end with EOF (End Of File) *)
main:
  | DELIMITER block EOF { $2 }
  | INDENT block DEDENT DELIMITER EOF { $2 }
;

(* tuple *)
tup_inner:
  | exp { [$1] }
  | exp COMMA tup_inner { $1::$3 }
;
	

(* vars inner *)
vars_inner:
  | VAR { [$1] }
  | VAR COMMA vars_inner { $1::$3 }
;
	
(* vars *)
vars:
  | LPAREN vars_inner RPAREN { $2 }
  | LPAREN RPAREN { [] } 
;
	

(* argument *)
arg_exp:
  (* (e1, ..., en) *)
  | LPAREN tup_inner RPAREN { $2 }  
  | LPAREN RPAREN { [] } 
;
  
(* expression *)
exp:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }

  (* Unary minus -i *)
  | MINUS INT
    { IntLit (- $2) }
  
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
  
  (* e1 + e2 *)
  | exp PLUS exp
    { Plus ($1, $3) }
  
  (* e1 * e2 *)
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  (* e1 < e2 *)
  | exp LT exp
    { Lt ($1, $3) }    

  (* lambda x1, ..., xn COL { block } *)
  | LAMBDA vars_inner COL INDENT block DEDENT
     { Lambda ($2, $5) }

  | LAMBDA vars_inner COL 
     { Lambda ($2, Skip) }

  (* application *)
  (* f (e1, ..., en) *)
  | exp arg_exp { App ($1, $2) }

  (* Parentheses *)
  | LPAREN exp RPAREN
    { $2 }
;

(* statement *)
stmt:
  (* f (e1, ..., en) ; *)
  | exp { Exp $1 } 

  (* Return *)
  | RETURN exp
    { Return $2 }
  
  (* Assignment *)
  | exp EQ exp
    { Assign ($1, $3) }

  (* def f (x1, ..., xn): { block } *)
  | DEF VAR vars COL INDENT block DEDENT
    { Assign (Var $2, RecFunc ($2, $3, $6)) }

  (* def f (x1, ..., xn): <nothing> *)
  | DEF VAR vars COL
    { Assign (Var $2, RecFunc ($2, $3, Skip)) }

  (* while exp block *)
  | WHILE exp COL INDENT block DEDENT
   { While ($2, $5) }

  (* while nothing *)
  | WHILE exp COL 
   { While ($2, Skip) }
;
    
(* block *)
block:       
  (* stmt1 stmt2 ... *)
  | stmt DELIMITER block
    { Seq ($1, $3) }
    
  | stmt DELIMITER { $1 }
;

