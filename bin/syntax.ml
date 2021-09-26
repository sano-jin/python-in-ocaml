(** syntax.ml *)

open Util
open Util.ListExtra

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
  | IntValOf of exp  (** convert to int *)
  | BoolValOf of exp  (** convert to bool *)
  | StrValOf of exp  (** convert to str *)

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
  | SystemFunVal of string
  | TupleVal of value list
  | ClassObjVal of class_obj_val
  | InstObjVal of {
      __class__ : class_obj_val;
      inst_vars : (string * value ref) list ref;
    }

and class_obj_val = {
  __name__ : string;
  __bases__ : class_obj_val list;
  __mro__ : class_obj_val list;
  class_vars : (string * value ref) list ref;
}

and env = (string * value ref) list ref list
(** environment e.g. [("x", 1); ("y", 2)]*)

let rec string_of_value = function
  | VoidVal -> "None"
  | IntVal i -> string_of_int i
  | BoolVal true -> "True"
  | BoolVal false -> "False"
  | StringVal str -> str
  | SystemFunVal name -> name
  | LambdaVal (vars, _, _) -> "\\" ^ String.concat " " vars ^ "."
  | (TupleVal _ | ClassObjVal _ | InstObjVal _) as obj ->
      snd @@ string_of_object [] obj

and string_of_class_variables printed obj_variables_ref =
  let vars, variable_refs = List.split !obj_variables_ref in
  let variables = List.map ( ! ) variable_refs in
  let printed, var_strs =
    List.fold_left_map string_of_object printed variables
  in
  let strs = List.map2 (fun x y -> x ^ " : " ^ y) vars var_strs in
  (printed, String.concat ", " strs)

and string_of_class_obj printed class_obj_val =
  let printed, vars_str =
    string_of_class_variables printed class_obj_val.class_vars
  in
  let class_name_of class_obj = class_obj.__name__ in
  ( printed,
    Printf.sprintf "<class '%s' | __bases__: %s, __mro__: %s, %s>"
      class_obj_val.__name__
      (ListExtra.string_of_list class_name_of class_obj_val.__bases__)
      (ListExtra.string_of_list class_name_of class_obj_val.__mro__)
      vars_str )

and string_of_object printed = function
  | ClassObjVal class_obj_val as self ->
      if List.memq self printed then (printed, "<cycle>")
      else string_of_class_obj (self :: printed) class_obj_val
  | InstObjVal inst_obj_val as self ->
      if List.memq self printed then (printed, "<cycle>")
      else
        let printed, vars_str =
          string_of_class_variables (self :: printed) inst_obj_val.inst_vars
        in
        ( printed,
          Printf.sprintf "<%s | %s>" inst_obj_val.__class__.__name__ vars_str )
  | TupleVal values as self ->
      if List.memq self printed then (printed, "<cycle>")
      else
        let printed, strs =
          List.fold_left_map string_of_object printed values
        in
        let last_comma = if List.length strs = 1 then "," else "" in
        (printed, "(" ^ String.concat ", " strs ^ last_comma ^ ")")
  | value -> (value :: printed, string_of_value value)

type ('a, 'b, 'c) result_with =
  | ProceedWith of 'a
  | ReturnWith of 'b
  | ContinueWith of 'a
  | BreakWith of 'a
  | ExceptionWith of 'c

let string_of_envs envs =
  let string_of_binding (var, value_ref) =
    "(" ^ var ^ ", " ^ string_of_value !value_ref ^ ")"
  in
  let string_of_env = string_of_list string_of_binding <. ( ! ) in
  string_of_list string_of_env envs

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be int"

let extract_bool = function
  | BoolVal b -> b
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be bool"

let extract_string = function
  | StringVal str -> str
  | value ->
      failwith @@ "TypeError: " ^ string_of_value value
      ^ " is expected to be string"

let seq_of_list = List.fold_left (fun acc stmt -> Seq (acc, stmt)) Skip

let intVal = function Ok i -> Ok (IntVal i) | Error _ as err -> err

let boolVal = function Ok i -> Ok (BoolVal i) | Error _ as err -> err

let stringVal = function Ok i -> Ok (StringVal i) | Error _ as err -> err
