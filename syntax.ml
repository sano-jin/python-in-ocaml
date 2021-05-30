(* syntax.ml *)

(* expression *)
type exp = 
  | Var of string              (* variable e.g. x *)
  | IntLit of int              (* integer literal e.g. 17 *)
  | BoolLit of bool	       (* boolean literal e.g. true, false *)
  | Plus of exp * exp          (* e + e *)
  | Times of exp * exp         (* e * e *)
  | Lt of exp * exp            (* e < e *)
  | App of string * exp list   (* f(x1, ..., xn) *)
  | Assign of string * exp     (* assignment e.g. x := 1 + 2 * y *)
  | Seq of exp * exp           (* sequence e.g. x := 2; y := x + 1 *)
  | While of exp * exp         (* loop e.g. while (1 < x) { x := x + 1 } *)
  | Func of string * exp list * exp     (* func f(x, y) {return x + y} *)
  | Let of string * exp * exp  (* let binding. e.g. `let x = 3; ...` *)
  | Skip                       (* skip *)
  | Return of exp              (* return e *)
			    
(* value *)			 
type value =
  | VoidVal
  | IntVal of int
  | BoolVal of bool
  | FuncVal of string list * exp * env (* closure *)
 and env = (string * int) list (* environment e.g. [("x", 1); ("y", 2)]*)
