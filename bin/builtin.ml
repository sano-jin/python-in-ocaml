(** Built-in types and functions *)

open Util.ResultExtra
open Syntax
open Util

let intVal = function Ok i -> Ok (IntVal i) | Error _ as err -> err

let boolVal = function Ok i -> Ok (BoolVal i) | Error _ as err -> err

let stringVal = function Ok i -> Ok (StringVal i) | Error _ as err -> err

let eval_obj eval obj prop args =
  let binding_of i arg = ("arg_" ^ string_of_int i, ref arg) in
  let bindings = List.mapi binding_of args in
  let vars = List.map ((fun var -> Var var) <. fst) bindings in
  eval (ref @@ (("obj", ref obj) :: bindings))
  @@ App (Access (Var "obj", prop), vars)

let __int__ eval = function
  | IntVal i -> Ok i
  | BoolVal true -> Ok 1
  | BoolVal false -> Ok 0
  | VoidVal -> Ok 0
  | StringVal str -> Ok (int_of_string str)
  | ObjectVal _ as obj -> extract_int <$> eval_obj eval obj "__int__" []
  | _ -> failwith "NotImplemented"

let __bool__ eval = function
  | IntVal i -> Ok (i <> 0)
  | BoolVal b -> Ok b
  | VoidVal -> Ok false
  | StringVal str -> Ok (bool_of_string str)
  | ObjectVal _ as obj -> extract_bool <$> eval_obj eval obj "__bool__" []
  | _ -> failwith "NotImplemented"

let __str__ eval = function
  | IntVal i -> Ok (string_of_int i)
  | BoolVal b -> Ok (string_of_bool b)
  | VoidVal -> Ok "Void"
  | StringVal str -> Ok str
  | ObjectVal _ as obj -> extract_string <$> eval_obj eval obj "__str__" []
  | _ -> failwith "NotImplemented"

let __add__ eval = function
  | IntVal i, obj -> intVal (( + ) i <$> __int__ eval obj)
  | StringVal s, obj -> stringVal (( ^ ) s <$> __str__ eval obj)
  | BoolVal true, obj -> intVal (( + ) 1 <$> __int__ eval obj)
  | BoolVal false, obj -> intVal (( + ) 0 <$> __int__ eval obj)
  | (ObjectVal _ as obj1), obj2 -> eval_obj eval obj1 "__add__" [ obj2 ]
  | _ -> failwith "NotImplemented"

let __mul__ eval = function
  | IntVal x, obj ->
      let+ y = __int__ eval obj in
      IntVal (x * y)
  | StringVal s, obj ->
      let+ n = __int__ eval obj in
      StringVal (String.concat "" @@ ListExtra.repeat n s)
  | BoolVal true, obj -> Ok obj
  | BoolVal false, IntVal _ -> Ok (IntVal 0)
  | BoolVal false, StringVal _ -> Ok (StringVal "")
  | BoolVal false, BoolVal _ -> Ok (BoolVal false)
  | BoolVal false, (ObjectVal _ as obj2) -> eval_obj eval obj2 "__mul__" []
  | (ObjectVal _ as obj1), obj2 -> eval_obj eval obj1 "__mul__" [ obj2 ]
  | _ -> failwith "NotImplemented"

let __not__ eval = function
  | IntVal i -> Ok (IntVal (-i))
  | BoolVal b -> Ok (BoolVal (not b))
  | VoidVal -> Ok (BoolVal true)
  | StringVal "" -> Ok (BoolVal true)
  | StringVal _ -> Ok (StringVal "")
  | ObjectVal _ as obj -> eval_obj eval obj "__not__" []
  | _ -> failwith "NotImplemented"

let __lt__ eval = function
  | IntVal i, obj -> boolVal (( < ) i <$> __int__ eval obj)
  | StringVal s, obj -> boolVal (( < ) s <$> __str__ eval obj)
  | BoolVal b, obj -> boolVal (( < ) b <$> __bool__ eval obj)
  | (ObjectVal _ as obj1), obj2 -> eval_obj eval obj1 "__lt__" [ obj2 ]
  | _ -> failwith "NotImplemented"

let __gt__ eval = function
  | IntVal i, obj -> boolVal (( > ) i <$> __int__ eval obj)
  | StringVal s, obj -> boolVal (( > ) s <$> __str__ eval obj)
  | BoolVal b, obj -> boolVal (( > ) b <$> __bool__ eval obj)
  | (ObjectVal _ as obj1), obj2 -> eval_obj eval obj1 "__gt__" [ obj2 ]
  | _ -> failwith "NotImplemented"

let __eq__ eval = function
  | IntVal i, obj -> boolVal (( = ) i <$> __int__ eval obj)
  | StringVal s, obj -> boolVal (( = ) s <$> __str__ eval obj)
  | BoolVal b, obj -> boolVal (( = ) b <$> __bool__ eval obj)
  | (ObjectVal _ as obj1), obj2 -> eval_obj eval obj1 "__eq__" [ obj2 ]
  | _ -> failwith "NotImplemented"

let __neq__ eval = function
  | (IntVal _ as x), y | (StringVal _ as x), y | (BoolVal _ as x), y ->
      __eq__ eval (x, y) >>= __not__ eval
  | (ObjectVal _ as obj1), obj2 -> eval_obj eval obj1 "__neq__" [ obj2 ]
  | _ -> failwith "NotImplemented"
