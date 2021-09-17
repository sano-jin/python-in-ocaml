// Parser
     
%{
  open Syntax
%}

%token <string> VAR  // "x", "y", "abc", ...
%token <int> INT     // "0", "1", "2", ...
%token <int> INDENT  // indent

// operators
%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token LT       // '<'
%token SEMICOL  // ';'
%token COMMA    // ','
%token ASSIGN   // ":="
%token EQ       // "="

// Parentheses
%token LPAREN   // '('
%token RPAREN   // ')'
%token LCBRA    // '{'
%token RCBRA    // '}'

// reserved names
%token TRUE     // "true"
%token FALSE    // "false"
%token WHILE    // "while"
%token FUNC     // "func"
%token LET      // "let"
%token REC      // "rec"
%token RETURN   // "return"
%token PRINT    // "print"

// End of file
%token EOF 

// Operator associativity
%left COMMA
%nonassoc LET
%left SEMICOL
%nonassoc RETURN	  
%nonassoc ASSIGN WHILE 
%nonassoc LT
%left PLUS
%left ASTERISK
%nonassoc UNARY
%nonassoc FUNC
%nonassoc VAR
%nonassoc INT TRUE FALSE LPAREN LCBRA

%start main
%type <Syntax.stmt> main

%%

// Main part must end with EOF (End Of File)
main:
  | block EOF
    { $1 }
;

// tuple
tup_inner:
  | exp { [$1] }
  | exp COMMA tup_inner { $1::$3 }
;
	

// vars inner
vars_inner:
  | VAR { [$1] }
  | VAR COMMA vars_inner { $1::$3 }
;
	
// vars
vars:
  | LPAREN vars_inner RPAREN { $2 }
;
	
// body of a function
body:
  | LCBRA block RCBRA { $2 }
  | LCBRA RCBRA       { Skip }
;		     

// application
app:
  // f (e1, ..., en)
  | exp arg_exp { App ($1, $2) }
;

// argument
arg_exp:
  // (e1, ..., en)
  | LPAREN tup_inner RPAREN { $2 }  
  | LPAREN RPAREN { [] } 
;
  
// expression
exp:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }

  // Unary minus -i
  | MINUS INT %prec UNARY
    { IntLit (- $2) }
  
  // Parentheses
  | LPAREN exp RPAREN
    { $2 }

  // e1 + e2
  | exp PLUS exp
    { Plus ($1, $3) }
  
  // e1 * e2
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  | TRUE
    { BoolLit true }
    
  | FALSE
    { BoolLit false }
  
  // e1 < e2
  | exp LT exp
    { Lt ($1, $3) }    

  // func (x1, ..., xn) { block }
  | FUNC vars body
     { Func ($2, $3) }

  // application
  | app { $1 }
;

// statement
stmt:
  // f (e1, ..., en)
  | app { Exp $1 } 

  // Return
  | RETURN exp
    { Return $2 }
  
  // func f (x1, ..., xn) { block } block
  | FUNC VAR vars body block
    { Let ($2, RecFunc ($2, $3, $4), $5) }

  // while exp :
  | WHILE exp COL
   { While ($2, []) }

  // print e
  | PRINT exp
   { Print $2 }    
;
    
// lines
lines:       
  // stmt1 \n stmt2 ...
  | INDENT stmt lines
    { ($1, $2) :: $3 }
    
  | INDENT stmt { ($1, $2) }
;

