// Parser
     
%{
  open Syntax
%}

%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

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
%token RETURN   // "return"

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
%nonassoc INT TRUE FALSE LPAREN

%start main
%type <Syntax.exp> main

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

// body of a function
body:
  | LCBRA block RCBRA { $2 }
  | LCBRA RCBRA     { Skip }
		     

// expression
exp:
  | VAR
    { Var $1 }
    
  | INT
    { IntLit $1 }

  // Unary minus -i
  | MINUS INT %prec UNARY
    { IntLit (- $2) }
  
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

  // Parentheses
  | LPAREN exp RPAREN
    { $2 }

  // f (e1, ..., en)
  | VAR LPAREN tup_inner RPAREN { App ($1, $3) }  
;

// statement
statement:
  // expression     
  | exp { $1 }

  // Return
  | RETURN exp
    { Return $2 }
  
  // Assignment
  | VAR ASSIGN exp
    { Assign ($1, $3) }
  
// block
block:       
  | statement { $1 }

  // Bind.	
  | LET VAR EQ exp SEMICOL block
    { Let ($2, $4, $6) }
  
  // e1; e2
  | exp SEMICOL block
    { Seq ($1, $3) }
    
  // e;
  | block SEMICOL
  { $1 }
	  
  // while exp block
  | WHILE exp block
   { While ($2, $3) }
  
  // func f (x1, ..., xn) { block }
  | FUNC VAR LPAREN tup_inner RPAREN body { Func ($2, $4, $6) }
    
  // Block
  | LCBRA block RCBRA
    { $2 }

  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;

