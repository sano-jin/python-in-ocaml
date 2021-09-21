(* eval.ml *)

open Syntax
open Util

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"

let extract_object_variables_ref value_ref =
  match !value_ref with
  | ObjectVal obj_variables_ref -> obj_variables_ref
  | value -> failwith @@ string_of_value value ^ " is expected to be an object"

let extract_class_variables_ref =
  extract_object_variables_ref <. List.assoc "__class__" <. ( ! )

let extract_mro_class_objs_ref =
  extract_object_variables_ref <. List.assoc "__mro__" <. ( ! )
  <. extract_object_variables_ref

let extract_class_variable_opt obj_fields_ref prop =
  List.assoc_opt prop !(extract_class_variables_ref obj_fields_ref)

let object_variables =
  [
    ("__init__", ref @@ LambdaVal ([ "<self>" ], Skip, []));
    ("__mro__", ref @@ ObjectVal (ref []));
  ]

let object_fields = [ ("__class__", ref @@ ObjectVal (ref object_variables)) ]

let class_of_lambda_ref = function
  | LambdaVal (_, _, env :: _) -> List.assoc "__class__" !env
  | _ -> failwith @@ "Cannot extract class object fromm non-lambda"

let extract_variable_opt obj_fields_ref prop =
  match List.assoc_opt prop !obj_fields_ref with
  | Some prop -> Some prop
  | None -> (
      let class_fields = extract_class_variables_ref obj_fields_ref in
      match List.assoc_opt prop !class_fields with
      | Some prop -> Some prop
      | None ->
          prerr_endline ">>> extract_variable_opt ";
          let base_classes =
            extract_object_variables_ref @@ List.assoc "__mro__" !class_fields
          in
          let extract_base_class_variable_opt =
            List.assoc_opt prop <. ( ! ) <. extract_object_variables_ref <. snd
          in
          one_of extract_base_class_variable_opt !base_classes)

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
      | Var "object", [] -> ObjectVal (ref object_fields)
      | Var "copy", [ ObjectVal dict ] ->
          ObjectVal (ref @@ List.map (second (ref <. ( ! ))) !dict)
      | Var "print", argVals ->
          print_endline @@ String.concat " " @@ List.map string_of_value argVals;
          VoidVal
      | _, argVals -> (
          match eval_exp envs f with
          | LambdaVal (vars, body, envs') ->
              prerr_endline @@ ">>> vars = [" ^ String.concat ", " vars ^ "]"
              ^ ", vals = ["
              ^ String.concat ", " (List.map string_of_value argVals)
              ^ "]";
              let argVals = List.map ref argVals in
              let new_env = ref @@ List.combine vars argVals in
              Either.fold ~left:(const VoidVal) ~right:id
              @@ eval_stmt ([], new_env :: envs') body
          | other_val ->
              failwith @@ string_of_value other_val
              ^ " is expected to be a  function type"))
  | Access (obj, prop) -> (
      match eval_exp envs obj with
      | ObjectVal dict -> (
          match List.assoc_opt prop !dict with
          | Some prop_ref -> !prop_ref
          | None -> !(Option.get @@ extract_class_variable_opt dict prop))
      | LambdaVal (_, _, variables_ref :: _) ->
          !(Option.get @@ extract_class_variable_opt variables_ref prop)
      | _ -> failwith @@ "Cannot access to a non-object with a dot notation")
  | Class (name, vars, body) -> (
      prerr_endline @@ "--------- class " ^ name ^ " ------------------";
      let env = ref [] in
      let vars = vars @ [ "object" ] in
      let class_obj_of_var = function
        | "object" -> [ ("object", ref @@ ObjectVal (ref object_variables)) ]
        | class_obj_name ->
            let class_obj_ref =
              class_of_lambda_ref
                !(Option.get
                 @@ one_of (List.assoc_opt class_obj_name <. ( ! )) envs)
            in
            (class_obj_name, class_obj_ref)
            :: !(extract_mro_class_objs_ref class_obj_ref)
      in
      let mro =
        remove_dup (fun (name1, _) (name2, _) -> name1 = name2)
        @@ List.concat_map class_obj_of_var vars
      in
      prerr_endline @@ ">>> " ^ String.concat ", " @@ List.map fst mro;
      let super_obj_name, super_obj_ref = List.hd mro in
      let init_vars_of_variables_ref variables_ref =
        match !(List.assoc "__init__" !variables_ref) with
        | LambdaVal (self :: vars, _, _) -> (self, vars)
        | _ ->
            failwith
              "__init__ should be function type with zero or more arguments"
      in
      let super_init_vars =
        snd @@ init_vars_of_variables_ref
        @@ extract_object_variables_ref super_obj_ref
      in
      prerr_endline @@ ">>> super_init_vars ["
      ^ String.concat ", " super_init_vars
      ^ "]";
      prerr_endline "hoge";
      env :=
        [
          ("__name__", ref @@ StringVal name);
          ("__init__", ref @@ LambdaVal ([], Skip, env :: envs));
          ("__mro__", ref @@ ObjectVal (ref mro));
        ];
      match eval_stmt ([], env :: envs) body with
      | Either.Right _ -> failwith @@ "cannot return in class definition"
      | Either.Left _ ->
          let super_init self_var =
            Lambda
              ( super_init_vars,
                seq_of_list
                  [
                    Exp
                      (App
                         ( Var "print",
                           [
                             StringLit
                               ("    .....    ------- enter super ["
                               ^ String.concat ", " super_init_vars
                               ^ "] ...................");
                           ] ));
                    Exp
                      (App
                         ( Var "print",
                           [
                             StringLit
                               ("    .....    ------- super is "
                              ^ super_obj_name);
                           ] ));
                    Exp
                      (App
                         ( Var "print",
                           [
                             StringLit "    .....    ------- super.__init is ";
                             Access (Var super_obj_name, "__init__");
                           ] ));
                    Exp
                      (App
                         ( Access (Var super_obj_name, "__init__"),
                           Var self_var
                           :: List.map (fun var -> Var var) super_init_vars ));
                    Exp
                      (App
                         ( Var "print",
                           [
                             StringLit
                               ("    .....    ------- exit super ["
                               ^ String.concat ", " super_init_vars
                               ^ "] ...................");
                           ] ));
                  ] )
          in
          prerr_endline @@ ">>> super_init_vars ["
          ^ String.concat ", " super_init_vars
          ^ "]";
          let super_stmt self_var =
            seq_of_list
              [
                Exp
                  (App
                     ( Var "print",
                       [
                         StringLit ("---> my super class is " ^ super_obj_name);
                       ] ));
                Exp
                  (App
                     ( Var "print",
                       [
                         StringLit
                           ("---> super_init_vars ["
                           ^ String.concat ", " super_init_vars
                           ^ "]");
                       ] ));
                Assign (Var "<proxy_obj>", App (Var "object", []));
                Assign
                  (Access (Var "<proxy_obj>", "__init__"), super_init self_var);
                Assign (Var "super", Lambda ([], Return (Var "<proxy_obj>")));
              ]
          in
          (let init_ref = List.assoc "__init__" !env in
           match !init_ref with
           | LambdaVal ((self :: _ as vars), stmt, env) ->
               init_ref := LambdaVal (vars, Seq (super_stmt self, stmt), env)
           | _ -> ());
          let classify_methods (var, value) =
            match !value with
            | LambdaVal (self_var :: vars, body, _) ->
                Either.Left
                  (var, Lambda ([ self_var ], Return (Lambda (vars, body))))
            | _ -> Either.Right (var, value)
          in
          let methods, _ = List.partition_map classify_methods !env in
          let method_binding_stmt_of (var, lambda) =
            Assign (Access (Var "<self>", var), App (lambda, [ Var "<self>" ]))
          in
          let init_self, init_vars = init_vars_of_variables_ref env in
          prerr_endline @@ ">>> init_vars ["
          ^ String.concat ", " (init_self :: init_vars)
          ^ "]";
          prerr_endline "hoge";
          let stmts =
            [
              Assign (Var "<self>", App (Var "object", []));
              Assign (Access (Var "<self>", "__class__"), Var "__class__");
            ]
            @ List.map method_binding_stmt_of methods
            @ [
                Exp
                  (App
                     ( Access (Var "<self>", "__init__"),
                       List.map (fun var -> Var var) init_vars ));
                Return (Var "<self>");
              ]
          in

          prerr_endline @@ "--------- exit class " ^ name
          ^ " ------------------";
          LambdaVal
            ( init_vars,
              seq_of_list stmts,
              ref [ ("__class__", ref (ObjectVal env)) ] :: ref !env :: envs ))

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
      | LambdaVal (_, _, variables_ref :: _) ->
          (let class_variables_ref =
             extract_class_variables_ref variables_ref
           in
           match List.assoc_opt prop !class_variables_ref with
           | Some prop -> prop := value
           | None ->
               class_variables_ref := (prop, ref value) :: !class_variables_ref);
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
