(** syntax.ml *)

(** expression *)
type exp =
  | Var of string  (** variable e.g. x *)
  | IntLit of int  (** integer literal e.g. 17 *)
  | BoolLit of bool  (** boolean literal e.g. true, false *)
  | Plus of exp * exp  (** e + e *)
  | Times of exp * exp  (** e * e *)
  | Lt of exp * exp  (** e < e *)
  | Eq of exp * exp  (** e == e *)
  | Lambda of string list * stmt list  (** lambda x, y: ... *)
  | App of exp * exp list  (** f (x1, ..., xn) *)

and stmt =
  | Exp of exp
  | Bind of string * exp  (** bind e.g. x = 1 + 2 *)
  | While of exp * stmt list  (** loop e.g. while 1 < x: ... *)
  | If of exp * stmt list  (** if e.g. if 1 < x: ... *)
  | Else of exp * stmt list  (** else e.g. else: ... *)
  | Elif of exp * stmt list  (** elif e.g. elif 0 < x: ... *)
  | Def of string * string list * stmt list  (** def f (x, y): ... *)
  | Return of exp  (** return e *)

(** value *)
type value =
  | VoidVal
  | IntVal of int
  | BoolVal of bool
  | LambdaVal of string list * stmt * env  (** closure *)
  | DefVal of string * string list * stmt * env  (** recursive closure *)

and env = (string * value ref) list
(** environment e.g. [("x", 1); ("y", 2)]*)

let string_of_value = function
  | VoidVal -> "void"
  | IntVal i -> string_of_int i
  | BoolVal true -> "true"
  | BoolVal false -> "false"
  | FuncVal _ -> "Func ..."
  | RecFuncVal _ -> "RecFunc ..."
