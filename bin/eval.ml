(* eval.ml *)

open Syntax
open Util

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"

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
  | Plus (e1, e2) -> IntVal (eval_binop_int ( + ) e1 e2)
  | Times (e1, e2) -> IntVal (eval_binop_int ( * ) e1 e2)
  | Lt (e1, e2) -> BoolVal (eval_binop_int ( < ) e1 e2)
  | Lambda (args, body) -> LambdaVal (args, body, envs)
  | App (f, args) -> (
      let argVals = List.map (eval_exp envs) args in
      if f = Var "print" then (
        print_endline @@ String.concat ", " @@ List.map string_of_value argVals;
        VoidVal)
      else
        let argVals = List.map ref argVals in
        let eval_app envs body =
          Either.fold ~left:(const VoidVal) ~right:id
          @@ eval_stmt ([], envs) body
        in
        match eval_exp envs f with
        | LambdaVal (vars, body, envs') ->
            eval_app ((ref @@ List.combine vars argVals) :: envs') body
        | _ -> failwith @@ "expected function type")

and eval_stmt (nonlocals, envs) stmt =
  let proceed = Either.Left nonlocals in
  let assign var v envs =
    (match one_of (List.assoc_opt var <. ( ! )) envs with
    | None ->
        let env = List.hd envs in
        env := (var, ref v) :: !env
    | Some old_ref -> old_ref := v);
    proceed
  in
  match stmt with
  | Assign (Var var, e) -> (
      let v = eval_exp envs e in
      match envs with
      | [] -> failwith "there should be at least the global environment"
      | [ _ ] -> assign var v envs
      | env :: _ ->
          if List.mem var nonlocals then assign var v @@ dropLast1 envs
          else assign var v [ env ])
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
  | Skip -> proceed
  | Return e -> Either.Right (eval_exp envs e)
