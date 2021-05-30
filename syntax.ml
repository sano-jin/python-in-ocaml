(* syntax.ml *)

(* expression *)
type exp = 
  | Var of string              (* variable e.g. x *)
  | IntLit of int              (* integer literal e.g. 17 *)
  | BoolLit of bool	       (* boolean literal e.g. true, false *)
  | Plus of exp * exp          (* e + e *)
  | Times of exp * exp         (* e * e *)
  | Lt of exp * exp            (* e < e *)
  | Func of exp list * stmt           (* func (x, y) {return x + y} *)
  | App of exp * exp list   (* f(x1, ..., xn) *)
			
and stmt =
  | Exp of exp
  | Assign of string * exp            (* assignment e.g. x := 1 + 2 * y *)
  | Seq of stmt * stmt                (* sequence e.g. x := 2; y := x + 1 *)
  | While of exp * stmt               (* loop e.g. while (1 < x) { x := x + 1 } *)
  | Let of string * exp * stmt        (* let binding. e.g. `let x = 3; ...` *)
  | LetRec of string * exp * stmt     (* let binding. e.g. `let rec x = 3; ...` *)
  | Skip                              (* skip *)
  | Return of exp                     (* return e *)
		
(* value *)			 
type value =
  | VoidVal
  | IntVal of int
  | BoolVal of bool
  | FuncVal of string list * exp * env (* closure *)
 and env = (string * int) list (* environment e.g. [("x", 1); ("y", 2)]*)
