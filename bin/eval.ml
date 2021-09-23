(* eval.ml *)

open Syntax
open Util
open Object

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"

(** The evaluator *)
let rec eval_exp envs exp =
  let intVal = Result.map @@ fun i -> IntVal i in
  let boolVal = Result.map @@ fun b -> BoolVal b in
  let ( let* ) = Result.bind in
  let eval_binop f e1 e2 =
    let* v1 = eval_exp envs e1 in
    let* v2 = eval_exp envs e2 in
    Ok (f v1 v2)
  in
  let eval_binop_int f =
    eval_binop (fun v1 v2 -> f (extract_int v1) (extract_int v2))
  in
  match exp with
  | Var var -> (
      match OptionExtra.one_of (List.assoc_opt var <. ( ! )) envs with
      | Some v -> Ok !v
      | None -> failwith @@ "unbound variable " ^ var)
  | IntLit num -> Ok (IntVal num)
  | BoolLit bool -> Ok (BoolVal bool)
  | StringLit str -> Ok (StringVal str)
  | Plus (e1, e2) -> intVal @@ eval_binop_int ( + ) e1 e2
  | Times (e1, e2) -> intVal @@ eval_binop_int ( * ) e1 e2
  | Lt (e1, e2) -> boolVal @@ eval_binop_int ( < ) e1 e2
  | Gt (e1, e2) -> boolVal @@ eval_binop_int ( > ) e1 e2
  | Lambda (args, body) -> Ok (LambdaVal (args, body, envs))
  | App (f, args) -> (
      let* argVals = ResultExtra.map_results (eval_exp envs) args in
      match (f, argVals) with
      | Var "print", argVals ->
          print_endline @@ String.concat " " @@ List.map string_of_value argVals;
          eval_exp envs @@ Var "None"
      | _, argVals -> (
          let beta_conv argVals = function
            | LambdaVal (vars, body, envs') -> (
                let argVals = List.map ref argVals in
                let new_env = ref @@ List.combine vars argVals in
                match eval_stmt [] (new_env :: envs') body with
                | ReturnWith value -> Ok value
                | ExceptionWith excp -> Error excp
                | _ -> failwith "function ended without return")
            | other_val ->
                failwith @@ string_of_value other_val
                ^ " is expected to be a function type"
          in
          let* f = eval_exp envs f in
          match f with
          | LambdaVal _ as f -> beta_conv argVals f
          | ObjectVal class_fields_ref as class_obj ->
              let init =
                Option.get
                @@ extract_class_variable_opt !class_fields_ref "__init__"
              in
              let instance_obj =
                ObjectVal (ref [ ("__class__", ref class_obj) ])
              in
              ignore @@ beta_conv (instance_obj :: argVals) init;
              Ok instance_obj
          | other_val ->
              failwith @@ string_of_value other_val
              ^ " is expected to be a class object or a function type"))
  | Access (obj, prop) -> (
      let* obj = eval_exp envs obj in
      match obj with
      | ObjectVal _ as obj -> (
          match extract_variable_opt obj prop with
          | Some prop -> Ok prop
          | None ->
              failwith @@ "No such field " ^ prop ^ " in object "
              ^ string_of_value obj)
      | _ -> failwith @@ "Cannot access to a non-object with a dot notation")
  | Class (name, vars, body) -> (
      let vars = if vars = [] then [ "object" ] else vars in
      let* base_classes =
        ResultExtra.map_results (eval_exp envs <. fun var -> Var var) vars
      in
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
      match eval_stmt [] (env :: envs) body with
      | ProceedWith _ -> Ok this_class_obj
      | _ ->
          failwith
          @@ "cannot return/continue/break/throw exception in class definition")

and eval_stmt nonlocals envs stmt =
  let proceed = ProceedWith nonlocals in
  let ( let* ) x f =
    match x with Error err -> ExceptionWith err | Ok x -> f x
  in
  let assign envs var v =
    let assignable_envs =
      match envs with
      | [] -> failwith "there should be at least the global environment"
      | [ _ ] -> envs
      | env :: _ ->
          if List.mem var nonlocals then ListExtra.dropLast1 envs else [ env ]
    in
    (match OptionExtra.one_of (List.assoc_opt var <. ( ! )) assignable_envs with
    | None ->
        let env = List.hd envs in
        env := (var, ref v) :: !env
    | Some old_ref -> old_ref := v);
    proceed
  in
  match stmt with
  | Assign (Var var, e) ->
      let* v = eval_exp envs e in
      assign envs var v
  | Assign (Access (obj, prop), exp) -> (
      let* obj = eval_exp envs obj in
      let* value = eval_exp envs exp in
      match obj with
      | ObjectVal dict ->
          (match List.assoc_opt prop !dict with
          | Some prop -> prop := value
          | None -> dict := (prop, ref value) :: !dict);
          proceed
      | _ -> failwith @@ "Cannot access to a non-object with a dot notation")
  | Assign (_, _) -> failwith @@ "cannot assign to operator"
  | NonLocal var -> ProceedWith (var :: nonlocals)
  | Exp e ->
      let* _ = eval_exp envs e in
      proceed
  | Seq (s1, s2) -> (
      match eval_stmt nonlocals envs s1 with
      | ProceedWith nonlocals -> eval_stmt nonlocals envs s2
      | otherwise -> otherwise)
  | While (cond, stmt) -> (
      let* cond_value = eval_exp envs cond in
      match cond_value with
      | BoolVal true ->
          eval_stmt nonlocals envs @@ Seq (stmt, While (cond, stmt))
      | BoolVal false -> proceed
      | _ -> failwith @@ "expected boolean value")
  | If (cond, stmt) -> (
      let* cond_value = eval_exp envs cond in
      match cond_value with
      | BoolVal true -> eval_stmt nonlocals envs stmt
      | BoolVal false -> proceed
      | _ -> failwith @@ "expected boolean value")
  | Skip -> proceed
  | Return e ->
      let* return_val = eval_exp envs e in
      ReturnWith return_val
