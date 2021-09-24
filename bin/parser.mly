(* Parser *)
     
%{
  open Syntax
%}

%token <string> VAR	(* x, y, abc, ... *)
%token <string> STRING	(* 'str', ... *)
%token <int> INT	(* 0, 1, 2, ...  *)

(* operators *)
%token PLUS		(* '+' *)
%token MINUS		(* '-' *)
%token ASTERISK		(* '*' *)
%token LT		(* '<' *)
%token GT		(* '>' *)
%token EQEQ		(* "=="  *)
%token NEQ		(* "!="  *)
%token COL		(* ':' *)
%token DOT		(* '.' *)
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
%token PASS		(* "pass"  *)
%token IF		(* "if"  *)
%token ELIF		(* "elif"  *)
%token ELSE		(* "else"  *)
%token IS		(* "is"  *)
%token NOT		(* "not"  *)
%token ISNOT		(* "is not"  *)
%token LAMBDA		(* "lambda" *)
%token DEF		(* "def"    *)
%token CLASS		(* "class"  *)
%token NONLOCAL		(* "nonlocal"    *)
%token RETURN		(* "return" *)
%token TRY		(* "try" *)
%token EXCEPT		(* "except" *)
%token AS		(* "as" *)
%token RAISE		(* "raise" *)
%token BREAK		(* "break" *)
%token CONTINUE		(* "continue" *)
%token IMPORT		(* "import" *)
%token FROM		(* "from" *)

(* End of file *)
%token EOF 

(* Operator associativity *)
%nonassoc COL 
%nonassoc LT GT IS ISNOT EQEQ NEQ
%left PLUS
%left ASTERISK
%left DOT
%nonassoc LPAREN


%start main
%type <Syntax.stmt> main

%%

(* Main part must end with EOF (End Of File) *)
main:
  | DELIMITER block EOF { $2 }
  | INDENT block DEDENT DELIMITER EOF { $2 }
  | DELIMITER imports block EOF { Seq($2, $3) }
  | INDENT imports block DEDENT DELIMITER EOF { Seq($2, $3) }
  | BAD_DEDENT { failwith "bad dedent"} 
  | TOKENS { failwith "tokens should be exploded"}
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
  

(* unary expression *)
unary_exp:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }
  
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
  
  | STRING
    { StringLit $1 }
  
  (* Parentheses *)
  | LPAREN exp RPAREN
    { $2 }
;


(* expression *)
exp:
  | unary_exp
    { $1 }
    
  (* Unary minus -i *)
  | MINUS INT
    { IntLit (- $2) }
  
  (* Unary not  *)
  | NOT unary_exp
    { Not $2 }
  
  (* e1 + e2 *)
  | exp PLUS exp
    { Plus ($1, $3) }
  
  (* e1 * e2 *)
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  (* e1 < e2 *)
  | exp LT exp
    { Lt ($1, $3) }    
  
  (* e1 > e2 *)
  | exp GT exp
    { Gt ($1, $3) }    

  (* e1 == e2 *)
  | exp EQEQ exp
    { Eq ($1, $3) }
  
  (* e1 != e2 *)
  | exp NEQ exp
    { Neq ($1, $3) }
  
  (* e1 is e2 *)
  | exp IS exp
    { Is ($1, $3) }
  
  (* e1 is not e2 *)
  | exp ISNOT exp
    { IsNot ($1, $3) }
  
  (* lambda x1, ..., xn : { block } *)
  | LAMBDA vars_inner COL exp
     { Lambda ($2, Return $4) }

  (* application *)
  (* f (e1, ..., en) *)
  | exp arg_exp { App ($1, $2) }

  (* dot notation *)
  (* exp.var *)
  | exp DOT VAR { Access ($1, $3) }
;

(* statement *)
stmt:
  (* f (e1, ..., en) ; *)
  | exp { Exp $1 } 

  (* Return *)
  | RETURN exp
    { Return $2 }
  
  (* Pass *)
  | PASS
    { Skip }
  
  (* Assignment *)
  | exp EQ exp
    { Assign ($1, $3) }

  (* def f (x1, ..., xn): { block } *)
  | DEF VAR vars COL INDENT block DEDENT
    { Assign (Var $2, Lambda ($3, Seq ($6, Return (Var "None")))) }

  (* class MyClass: { block } *)
  | CLASS VAR COL INDENT block DEDENT
    { Assign (Var $2, Class ($2, [], $5)) }

  (* class MyClass (...): { block } *)
  | CLASS VAR vars COL INDENT block DEDENT
    { Assign (Var $2, Class ($2, $3, $6)) }

  (* while exp block *)
  | WHILE exp COL INDENT block DEDENT
   { While ($2, $5) }

  | NONLOCAL VAR { NonLocal $2 }

  | RAISE exp { Raise $2 }

  | BREAK { Break }
  | CONTINUE { Continue }
;

    
(* block *)
block:       
  (* stmt1 stmt2 ... *)
  | stmt DELIMITER block
    { Seq ($1, $3) }
    
  | stmt DELIMITER { $1 }

  | if_elifs_else block { Seq($1, $2) }
  | if_elifs_else { $1 }

  | try_exceptions block { Seq ($1, $2) }
  | try_exceptions { $1 }
;


except:
  | EXCEPT exp COL INDENT block DEDENT { ($2, "_", $5) }
  | EXCEPT exp AS VAR COL INDENT block DEDENT { ($2, $4, $7) }

exceptions:
  | except DELIMITER exceptions  { $1 :: $3 }
  | except DELIMITER { [ $1 ] }
;

try_exceptions:
  | TRY COL INDENT block DEDENT DELIMITER exceptions { Try ($4, $7) }
;


if_elifs_else:
  (* if exp block *)
  | IF exp COL INDENT block DEDENT DELIMITER elifs
   { If ($2, $5, $8) }

  (* if exp block *)
  | IF exp COL INDENT block DEDENT DELIMITER else_
   { If ($2, $5, $8) }

  (* if exp block *)
  | IF exp COL INDENT block DEDENT DELIMITER
   { If ($2, $5, Skip) }
;


elifs:
  (* elif exp block *)
  | ELIF exp COL INDENT block DEDENT DELIMITER
   { If ($2, $5, Skip) }

  (* elif exp block *)
  | ELIF exp COL INDENT block DEDENT DELIMITER else_
   { If ($2, $5, $8) }

  (* elif exp block *)
  | ELIF exp COL INDENT block DEDENT DELIMITER elifs
   { If ($2, $5, $8) }
;

else_:
  (* else: block *)
  | ELSE COL INDENT block DEDENT DELIMITER
   { $4 }
;


imports:
  (* import1 import2 ... *)
  | import DELIMITER imports
    { Seq ($1, $3) }
    
  | import DELIMITER { $1 }
;

import:
 (** import <module_name> *)
 | IMPORT VAR
  { Assign(Var $2, Open $2) }

 (** from <var> import <module_name> *)
 | FROM VAR IMPORT importing_values
  { let module_name = "<" ^ $2 ^ ">" in
    let binding_of (var1, var2) =
     Assign(Var var1, Access(Var module_name, var2))
    in
    let binds = seq_of_list @@ List.map binding_of $4 in
    Seq(Assign(Var module_name, Open $2), binds)
  }
;

importing_values:
 | VAR COMMA importing_values { ($1, $1) :: $3 }
 | VAR { [ ($1, $1) ] }

 | VAR AS VAR COMMA importing_values { ($3, $1) :: $5 }
 | VAR AS VAR { [ ($3, $1) ] }
;


