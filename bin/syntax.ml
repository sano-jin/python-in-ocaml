(** syntax.ml *)

open Util

(** expression *)
type exp =
  | Var of string  (** variable e.g. x *)
  | IntLit of int  (** integer literal e.g. 17 *)
  | BoolLit of bool  (** boolean literal e.g. true, false *)
  | StringLit of string  (** string literal e.g. 'dog' *)
  | Plus of exp * exp  (** e + e *)
  | Times of exp * exp  (** e * e *)
  | Lt of exp * exp  (** e < e *)
  | Gt of exp * exp  (** e > e *)
  | Eq of exp * exp  (** e = e *)
  | Neq of exp * exp  (** e != e *)
  | Not of exp  (** not e *)
  | Is of exp * exp  (** e is e *)
  | IsNot of exp * exp  (** e is not e *)
  | Lambda of string list * stmt  (** lambda x, y : {return x + y} *)
  | App of exp * exp list  (** f (x1, ..., xn) *)
  | Access of exp * string  (** exp.exp *)
  | Class of string * string list * stmt  (** class  *)
  | Open of string  (** open a file and return the clas object of the module *)

and stmt =
  | Exp of exp
  | Assign of exp * exp  (** assignment e.g. x := 1 + 2 * y *)
  | Seq of stmt * stmt  (** sequence e.g. x := 2; y := x + 1 *)
  | While of exp * stmt  (** loop e.g. while (1 < x) { x := x + 1 } *)
  | If of exp * stmt * stmt  (** branch e.g. if (1 < x) { x := x + 1 } *)
  | Skip  (** skip *)
  | NonLocal of string  (** nonlocal e.g. nonlocal y *)
  | Return of exp  (** return e *)
  | Try of stmt * (exp * string * stmt) list
      (** try e.g. try: ... except ... as ... : ... *)
  | Raise of exp  (**  raise e.g. raise Exception() *)
  | Break
  | Continue

(** value *)
type value =
  | VoidVal
  | IntVal of int
  | BoolVal of bool
  | StringVal of string
  | LambdaVal of string list * stmt * env  (** closure *)
  | ObjectVal of (string * value ref) list ref

and env = (string * value ref) list ref list
(** environment e.g. [("x", 1); ("y", 2)]*)

let rec string_of_value = function
  | VoidVal -> "void"
  | IntVal i -> string_of_int i
  | BoolVal true -> "true"
  | BoolVal false -> "false"
  | StringVal str -> str
  | LambdaVal (vars, _, _) -> "lambda (" ^ String.concat ", " vars ^ "): ..."
  | ObjectVal _ as obj -> snd @@ string_of_object [] obj

and string_of_object printed = function
  | ObjectVal variables_ref as self ->
      if List.memq self printed then (printed, "<~")
      else
        let vars, variable_refs = List.split !variables_ref in
        let variables = List.map ( ! ) variable_refs in
        let printeds, strs =
          List.fold_left_map string_of_object (self :: printed) variables
        in
        let strs = List.combine vars strs in
        let strs = List.map (fun (x, y) -> x ^ " : " ^ y) strs in
        (printeds, "[" ^ String.concat ", " strs ^ "]")
  | other -> (printed, string_of_value other)

type ('a, 'b, 'c) result_with =
  | ProceedWith of 'a
  | ReturnWith of 'b
  | ContinueWith of 'a
  | BreakWith of 'a
  | ExceptionWith of 'c

let string_of_list string_of_elem list =
  "[" ^ String.concat "; " (List.map string_of_elem list) ^ "]"

let string_of_envs envs =
  let string_of_binding (var, value_ref) =
    "(" ^ var ^ ", " ^ string_of_value !value_ref ^ ")"
  in
  let string_of_env = string_of_list string_of_binding <. ( ! ) in
  string_of_list string_of_env envs

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"

let extract_bool = function
  | BoolVal b -> b
  | _ -> failwith @@ "type error. expected bool"
