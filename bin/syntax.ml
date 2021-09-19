(** syntax.ml *)

(** expression *)
type exp =
  | Var of string  (** variable e.g. x *)
  | IntLit of int  (** integer literal e.g. 17 *)
  | BoolLit of bool  (** boolean literal e.g. true, false *)
  | Plus of exp * exp  (** e + e *)
  | Times of exp * exp  (** e * e *)
  | Lt of exp * exp  (** e < e *)
  | Lambda of string list * stmt  (** lambda x, y : {return x + y} *)
  | RecFunc of string * string list * stmt  (** def f (x, y): {return x + y} *)
  | App of exp * exp list  (** f(x1, ..., xn) *)

and stmt =
  | Exp of exp
  | Assign of exp * exp  (** assignment e.g. x := 1 + 2 * y *)
  | Seq of stmt * stmt  (** sequence e.g. x := 2; y := x + 1 *)
  | While of exp * stmt  (** loop e.g. while (1 < x) { x := x + 1 } *)
  | Skip  (** skip *)
  | NonLocal of string
  | Return of exp  (** return e *)

(** value *)
type value =
  | VoidVal
  | IntVal of int
  | BoolVal of bool
  | LambdaVal of string list * stmt * env  (** closure *)
  | RecFuncVal of string * string list * stmt * env  (** closure *)

and env = (string * value ref) list ref list
(** environment e.g. [("x", 1); ("y", 2)]*)

let string_of_value = function
  | VoidVal -> "void"
  | IntVal i -> string_of_int i
  | BoolVal true -> "true"
  | BoolVal false -> "false"
  | LambdaVal _ -> "lambda ..."
  | RecFuncVal _ -> "RecFunc ..."
