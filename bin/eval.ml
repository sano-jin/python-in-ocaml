(* eval.ml *)

open Syntax
open Util

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"

let extract_object_variables_ref = function
  | ObjectVal obj_variables_ref -> obj_variables_ref
  | value -> failwith @@ string_of_value value ^ " is expected to be an object"

let extract_class_variables_ref obj =
  (extract_object_variables_ref <. ( ! ) <. List.assoc "__class__") obj

let extract_mro_class_objs_ref =
  extract_object_variables_ref <. ( ! ) <. List.assoc "__mro__" <. ( ! )
  <. extract_object_variables_ref

let mro_of_class base_classes =
  let base_classes_of =
    ( ! ) <. extract_object_variables_ref <. ( ! ) <. List.assoc "__bases__"
    <. ( ! ) <. extract_object_variables_ref <. ( ! ) <. snd
  in
  let rec helper base_classes =
    match List.map base_classes_of base_classes with
    | [] -> []
    | base_base_classes ->
        base_classes :: (helper @@ List.concat base_base_classes)
  in
  remove_dup (fun (_, class_obj_ref1) (_, class_obj_ref2) ->
      !class_obj_ref1 == !class_obj_ref2)
  @@ List.concat @@ helper base_classes

let object_variables =
  [
    ("__init__", ref @@ LambdaVal ([ "_" ], Skip, []));
    ("__mro__", ref @@ ObjectVal (ref []));
    ("__bases__", ref @@ ObjectVal (ref []));
  ]

let object_fields = [ ("__class__", ref @@ ObjectVal (ref object_variables)) ]

let class_of_lambda_ref = function
  | LambdaVal (_, _, env :: _) -> List.assoc "__class__" !env
  | _ -> failwith @@ "Cannot extract class object fromm non-lambda"

let app_instance instance_obj = function
  | LambdaVal (var :: vars, body, env :: envs) ->
      LambdaVal (vars, body, ref ((var, ref instance_obj) :: !env) :: envs)
  | other -> other

let extract_class_variable_opt class_fields prop =
  let base_classes =
    extract_object_variables_ref @@ ( ! ) @@ List.assoc "__mro__" class_fields
  in
  let extract_base_class_variable_opt =
    List.assoc_opt prop <. ( ! ) <. extract_object_variables_ref <. ( ! ) <. snd
  in
  one_of extract_base_class_variable_opt !base_classes

let extract_variable_opt obj prop =
  let obj_fields = !(extract_object_variables_ref obj) in
  let is_instance = List.mem_assoc "__class__" obj_fields in
  if is_instance then
    match List.assoc_opt prop obj_fields with
    | Some prop -> Some !prop
    | None ->
        app_instance obj <. ( ! )
        <$> extract_class_variable_opt
              !(extract_class_variables_ref obj_fields)
              prop
  else ( ! ) <$> extract_class_variable_opt obj_fields prop

let seq_of_list = List.fold_left (fun acc stmt -> Seq (acc, stmt)) Skip

(** The evaluator *)
let rec eval_exp envs exp =
  let eval_binop f e1 e2 = f (eval_exp envs e1) (eval_exp envs e2) in
  let eval_binop_int f =
    eval_binop (fun v1 v2 -> f (extract_int v1) (extract_int v2))
  in
  match exp with
  | Var var -> (
      match one_of (List.assoc_opt var <. ( ! )) envs with
      | Some v -> !v
      | None -> failwith @@ "unbound variable " ^ var)
  | IntLit num -> IntVal num
  | BoolLit bool -> BoolVal bool
  | StringLit str -> StringVal str
  | Plus (e1, e2) -> IntVal (eval_binop_int ( + ) e1 e2)
  | Times (e1, e2) -> IntVal (eval_binop_int ( * ) e1 e2)
  | Lt (e1, e2) -> BoolVal (eval_binop_int ( < ) e1 e2)
  | Gt (e1, e2) -> BoolVal (eval_binop_int ( > ) e1 e2)
  | Lambda (args, body) -> LambdaVal (args, body, envs)
  | App (f, args) -> (
      match (f, List.map (eval_exp envs) args) with
      | Var "print", argVals ->
          print_endline @@ String.concat " " @@ List.map string_of_value argVals;
          VoidVal
      | _, argVals -> (
          let beta_conv argVals = function
            | LambdaVal (vars, body, envs') ->
                let argVals = List.map ref argVals in
                let new_env = ref @@ List.combine vars argVals in
                Either.fold ~left:(const VoidVal) ~right:id
                @@ eval_stmt ([], new_env :: envs') body
            | other_val ->
                failwith @@ string_of_value other_val
                ^ " is expected to be a function type"
          in
          match eval_exp envs f with
          | LambdaVal _ as f -> beta_conv argVals f
          | ObjectVal class_fields_ref as class_obj ->
              let init =
                !(Option.get
                 @@ extract_class_variable_opt !class_fields_ref "__init__")
              in
              let instance_obj =
                ObjectVal (ref [ ("__class__", ref class_obj) ])
              in
              ignore @@ beta_conv (instance_obj :: argVals) init;
              instance_obj
          | other_val ->
              failwith @@ string_of_value other_val
              ^ " is expected to be a class object or a function type"))
  | Access (obj, prop) -> (
      match eval_exp envs obj with
      | ObjectVal _ as obj -> (
          match extract_variable_opt obj prop with
          | Some prop -> prop
          | None ->
              failwith @@ "No such field " ^ prop ^ " in object "
              ^ string_of_value obj)
      | _ -> failwith @@ "Cannot access to a non-object with a dot notation")
  | Class (name, vars, body) -> (
      let vars = if vars = [] then [ "object" ] else vars in
      let base_classes = List.map (eval_exp envs <. fun var -> Var var) vars in
      let bases = List.combine vars @@ List.map ref base_classes in
      let env = ref [] in
      let this_class_obj = ObjectVal env in
      let mro = (name, ref this_class_obj) :: mro_of_class bases in
      env :=
        ("__name__", ref @@ StringVal name)
        :: ("__init__", ref @@ LambdaVal ([ "_" ], Skip, []))
        :: ("__mro__", ref @@ ObjectVal (ref mro))
        :: ("__bases__", ref @@ ObjectVal (ref bases))
        :: !env;
      match eval_stmt ([], env :: envs) body with
      | Either.Right _ -> failwith @@ "cannot return in class definition"
      | Either.Left _ -> this_class_obj)

and eval_stmt (nonlocals, envs) stmt =
  let proceed = Either.Left nonlocals in
  let assign envs var v =
    let assignable_envs =
      match envs with
      | [] -> failwith "there should be at least the global environment"
      | [ _ ] -> envs
      | env :: _ -> if List.mem var nonlocals then dropLast1 envs else [ env ]
    in
    (match one_of (List.assoc_opt var <. ( ! )) assignable_envs with
    | None ->
        let env = List.hd envs in
        env := (var, ref v) :: !env
    | Some old_ref -> old_ref := v);
    proceed
  in
  match stmt with
  | Assign (Var var, e) -> assign envs var @@ eval_exp envs e
  | Assign (Access (obj, prop), exp) -> (
      let obj = eval_exp envs obj in
      let value = eval_exp envs exp in
      match obj with
      | ObjectVal dict ->
          (match List.assoc_opt prop !dict with
          | Some prop -> prop := value
          | None -> dict := (prop, ref value) :: !dict);
          proceed
      | _ -> failwith @@ "Cannot access to a non-object with a dot notation")
  | Assign (_, _) -> failwith @@ "cannot assign to operator"
  | NonLocal var -> Either.Left (var :: nonlocals)
  | Exp e ->
      ignore @@ eval_exp envs e;
      proceed
  | Seq (s1, s2) -> (
      match eval_stmt (nonlocals, envs) s1 with
      | Either.Left nonlocals -> eval_stmt (nonlocals, envs) s2
      | otherwise -> otherwise)
  | While (cond, stmt) -> (
      match eval_exp envs cond with
      | BoolVal true ->
          eval_stmt (nonlocals, envs) @@ Seq (stmt, While (cond, stmt))
      | BoolVal false -> proceed
      | _ -> failwith @@ "expected boolean value")
  | If (cond, stmt) -> (
      match eval_exp envs cond with
      | BoolVal true -> eval_stmt (nonlocals, envs) stmt
      | BoolVal false -> proceed
      | _ -> failwith @@ "expected boolean value")
  | Skip -> proceed
  | Return e -> Either.Right (eval_exp envs e)
