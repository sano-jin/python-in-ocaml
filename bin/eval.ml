(* eval.ml *)

open Syntax

(** some helper functions *)
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"

(** The evaluator *)
let rec eval_exp env exp =
  let eval_binop f e1 e2 = f (eval_exp env e1) (eval_exp env e2) in
  let eval_binop_int f =
    eval_binop (fun v1 v2 -> f (extract_int v1) (extract_int v2))
  in
  let maybe default = function Some v -> v | None -> default in
  match exp with
  | Var var -> !(List.assoc var env)
  | IntLit num -> IntVal num
  | BoolLit bool -> BoolVal bool
  | Plus (e1, e2) -> IntVal (eval_binop_int ( + ) e1 e2)
  | Times (e1, e2) -> IntVal (eval_binop_int ( * ) e1 e2)
  | Lt (e1, e2) -> BoolVal (eval_binop_int ( < ) e1 e2)
  | Lambda (args, body) -> LambdaVal (args, body, env)
  | RecFunc (f, args, body) -> RecFuncVal (f, args, body, env)
  | App (f, args) -> (
      let argVals = List.map (eval_exp env) args in
      if f = Var "print" then (
        print_endline @@ String.concat ", " @@ List.map string_of_value argVals;
        VoidVal)
      else
        let argVals = List.map ref argVals in
        match eval_exp env f with
        | LambdaVal (vars, body, env') ->
            maybe VoidVal @@ fst
            @@ eval_stmt (List.combine vars argVals @ env') body
        | RecFuncVal (f, vars, body, env') ->
            maybe VoidVal @@ fst
            @@ eval_stmt
                 ((f, ref @@ RecFuncVal (f, vars, body, env'))
                  :: List.combine vars argVals
                 @ env')
                 body
        | _ -> failwith @@ "expected function type")

and eval_stmt env stmt =
  let mzero _ = None in
  match stmt with
  | Exp e -> (mzero @@ eval_exp env e, env)
  | Assign (Var var, e) -> (
      let v = eval_exp env e in
      match List.assoc_opt var env with
      | None -> (None, (var, ref @@ eval_exp env e) :: env)
      | Some old_ref ->
          old_ref := v;
          (None, env))
  | Assign (_, _) -> failwith @@ "cannot assign to operator"
  | Seq (s1, s2) -> (
      match eval_stmt env s1 with
      | None, env -> eval_stmt env s2
      | some, env -> (some, env))
  | While (cond, stmt) -> (
      match eval_exp env cond with
      | BoolVal true -> eval_stmt env @@ Seq (stmt, While (cond, stmt))
      | BoolVal false -> (None, env)
      | _ -> failwith @@ "expected boolean value")
  | Skip -> (None, env)
  | Return e -> (Some (eval_exp env e), env)
