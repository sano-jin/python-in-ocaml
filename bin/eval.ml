(* eval.ml *)

open Syntax
open Util
open Object
open Parsing

(** The evaluator *)
let rec eval_exp envs exp =
  let open ResultExtra in
  let eval_helper env = eval_exp (env :: envs) in
  let eval_binop f e1 e2 =
    let* v1 = eval_exp envs e1 in
    let+ v2 = eval_exp envs e2 in
    f v1 v2
  in
  let eval_builtin_binop f =
    Result.join <.. eval_binop @@ curry @@ f eval_helper
  in
  let eval_builtin_uniop f e =
    let* v = eval_exp envs e in
    f eval_helper v
  in
  match exp with
  | Var var -> Env.lookup envs var
  | IntLit num -> Ok (IntVal num)
  | BoolLit bool -> Ok (BoolVal bool)
  | StringLit str -> Ok (StringVal str)
  | Not e1 -> eval_builtin_uniop Builtin.__not__ e1
  | Plus (e1, e2) -> eval_builtin_binop Builtin.__add__ e1 e2
  | Times (e1, e2) -> eval_builtin_binop Builtin.__mul__ e1 e2
  | Lt (e1, e2) -> eval_builtin_binop Builtin.__lt__ e1 e2
  | Gt (e1, e2) -> eval_builtin_binop Builtin.__gt__ e1 e2
  | Eq (e1, e2) -> eval_builtin_binop Builtin.__eq__ e1 e2
  | Neq (e1, e2) -> eval_builtin_binop Builtin.__neq__ e1 e2
  | Is (e1, e2) -> boolVal @@ eval_binop ( == ) e1 e2
  | IsNot (e1, e2) -> boolVal @@ eval_binop ( != ) e1 e2
  | IntValOf e -> intVal @@ eval_builtin_uniop Builtin.__int__ e
  | BoolValOf e -> boolVal @@ eval_builtin_uniop Builtin.__bool__ e
  | StrValOf e -> stringVal @@ eval_builtin_uniop Builtin.__str__ e
  | Lambda (args, body) -> Ok (LambdaVal (args, body, envs))
  | App (f, args) -> (
      let* argVals = ResultExtra.map_results (eval_exp envs) args in
      let* f = eval_exp envs f in
      let beta_conv argVals = function
        | LambdaVal (vars, body, envs') -> (
            let new_env = List.combine vars @@ List.map ref argVals in
            match eval_stmt [] (ref new_env :: envs') body with
            | ReturnWith value -> Ok value
            | ExceptionWith excp -> Error excp
            | _ -> failwith "function ended without return")
        | other_val ->
            failwith @@ string_of_value other_val
            ^ " is expected to be a function type"
      in
      match f with
      | LambdaVal _ as f -> beta_conv argVals f
      | SystemFunVal "print" ->
          print_endline @@ String.concat " " @@ Result.get_ok
          @@ ResultExtra.map_results (Builtin.__str__ eval_helper) argVals;
          Ok VoidVal
      | SystemFunVal "repl" ->
          Ok (StringVal (String.concat " " @@ List.map string_of_value argVals))
      | ClassObjVal class_obj_val ->
          let init =
            Result.get_ok @@ extract_class_variable_opt class_obj_val "__init__"
          in
          let instance_obj =
            InstObjVal { __class__ = class_obj_val; inst_vars = ref [] }
          in
          ignore @@ beta_conv (instance_obj :: argVals) init;
          Ok instance_obj
      | other_val ->
          failwith @@ string_of_value other_val
          ^ " is expected to be a class object or a function type")
  | Access (obj, prop) ->
      let* obj = eval_exp envs obj in
      extract_variable_opt (eval_exp envs) prop obj
  | Class (name, vars, body) -> (
      let vars = if vars = [] then [ "object" ] else vars in
      let* bases =
        ResultExtra.map_results (Env.lookup envs >=> extract_class_obj_val) vars
      in
      let env, this_class_obj = init_class_obj name bases [] envs in
      match eval_stmt [] (env :: envs) body with
      | ProceedWith _ -> Ok (ClassObjVal this_class_obj)
      | _ ->
          failwith
          @@ "cannot return/continue/break/throw exception in class definition")
  | Open filename ->
      let filepath = Filename.dirname Sys.argv.(1) ^ "/" ^ filename ^ ".py" in
      read_and_run filepath filename

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
  | Assign (Access (obj, prop), exp) ->
      let* obj = eval_exp envs obj in
      let* value = eval_exp envs exp in
      let dict =
        match obj with
        | InstObjVal inst_obj_val -> inst_obj_val.inst_vars
        | ClassObjVal class_obj_val -> class_obj_val.class_vars
        | _ -> failwith @@ "Cannot access to a non-object with a dot notation"
      in
      (match List.assoc_opt prop !dict with
      | Some prop -> prop := value
      | None -> dict := (prop, ref value) :: !dict);
      proceed
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
      | BoolVal true -> (
          match eval_stmt nonlocals envs stmt with
          | BreakWith _ -> proceed
          | ContinueWith _ | ProceedWith _ ->
              eval_stmt nonlocals envs @@ While (cond, stmt)
          | other -> other)
      | BoolVal false -> proceed
      | _ -> failwith @@ "expected boolean value")
  | If (cond, s1, s2) -> (
      let* cond_value = eval_exp envs cond in
      match cond_value with
      | BoolVal true -> eval_stmt nonlocals envs s1
      | BoolVal false -> eval_stmt nonlocals envs s2
      | _ -> failwith @@ "expected boolean value")
  | Skip -> proceed
  | Return e ->
      let* return_val = eval_exp envs e in
      ReturnWith return_val
  | Try (stmt, excepts) -> (
      match eval_stmt nonlocals envs stmt with
      | ExceptionWith except_obj as exception_with ->
          let rec handle_exceptions = function
            | [] -> exception_with
            | (except_exp, var, body) :: t -> (
                match eval_exp envs except_exp with
                | Ok except_value ->
                    if Result.get_ok @@ is_subclass_of except_obj except_value
                    then
                      eval_stmt nonlocals
                        (ref [ (var, ref except_obj) ] :: envs)
                        body
                    else handle_exceptions t
                | Error err -> ExceptionWith err)
          in
          handle_exceptions excepts
      | other -> other)
  | Raise exp ->
      let* v = eval_exp envs exp in
      ExceptionWith v
  | Break -> BreakWith nonlocals
  | Continue -> ContinueWith nonlocals

and read_and_run filepath module_name =
  let _, this_module_obj =
    init_class_obj module_name [] [ ("object", ref Env.object_class_obj) ] []
  in
  let base_env =
    ("__name__", ref @@ StringVal module_name)
    :: (Env.init_bindings @ !(this_module_obj.class_vars))
  in
  match
    eval_stmt [] [ ref base_env ]
    @@ Seq (Env.system_stmt, read_and_parse filepath)
  with
  | ProceedWith _ | ReturnWith _ -> Ok (ClassObjVal this_module_obj)
  | ExceptionWith err -> Error err
  | BreakWith _ | ContinueWith _ -> failwith "file ended with break/continue"
