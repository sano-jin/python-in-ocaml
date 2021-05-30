(* eval.ml *)

open Syntax ;;

(* Some helper functions for dealing the environment *)

(* List.assoc *)
let rec lookup var env =
  match env with
  | [] -> failwith @@ "unbound variable '" ^ var ^ "'"
  | (var2, val2)::t ->
     if var = var2 then val2
     else lookup var t


let rec update var value env =
  match env with
  | [] -> failwith @@ "unbound variable '" ^ var ^ "'"
  | (var2, val2) as h::t ->
     if var = var2 then (var, value)::t
     else h::update var value t


(* some helper functions *)		    

(* mapM for a state monad *)		    
let rec mapM f list state = 
  match list with
  | [] -> ([], state)
  | h::t ->
     let (h', state') = f h state in
     let (t', state'') = mapM f t state' in
     (h'::t', state'')
		    

let second f (a, b) = (a, f b)
let rec drop n list =
  if n <= 0 then list
  else match list with
       | [] -> failwith @@ "not enough length"
       | _::t -> drop (n - 1) t
		      

(* The evaluator *)
		      
		      
(* Eval binop *)
let eval_binop f e1 e2 env =
  let (v1, env') = eval_exp e1 env in
  let (v2, env'') = eval_exp e2 env' in
  (f v1 v2, env'')
  
let extract_int = function
  | IntVal i -> i
  | failwith @@ "type error"
    
let eval_binop_int f e1 e2 =
  f (extract_int e1) (extract_int e2)
  
(* eval_exp : exp -> env -> int *)
let rec eval_exp exp env =
  let return v = (v, env) in 
  match exp with
  | Var var ->
     return @@ lookup var env
  | IntLit num -> return @@ IntVal num
  | BoolLit bool -> return @@ BoolVal bool
  | Plus (e1, e2) ->
     eval_binop_int (+) e1 e2 env
  | Times (e1, e2) ->
     eval_binop_int ( * ) e1 e2 env
  | Lt (e1, e2) ->
     eval_binop_int ( < ) e1 e2 env
  | Func args body ->
     return @@ Closure args body env
  | App (f, args) ->
     let (argVals, env') = foldM eval_exp args env in
     match eval_exp f env with
     | Func vars body env' ->
	second (drop @@ List.length vars)
	@@ eval_stmt body
	@@ List.combine vars argVals @ env'
     | RecFuncVal f vars body env' ->
	second (drop @@ List.length vars + 1)
	@@ eval_stmt body
	@@ (f, RecFuncVal f vars body env')::List.combine vars argVals @ env'
	
     
     
  | App of exp * exp list   (* f(x1, ..., xn) *)
	    
(* eval_b_exp : b_exp -> env -> bol *)
let eval_b_exp b_exp env =
  match b_exp with
  | BoolLit bool -> bool
  | Lt (a_exp1, a_exp2) ->  
     let value1 = eval_a_exp a_exp1 env in
     let value2 = eval_a_exp a_exp2 env in
     value1 < value2
		     
(* eval_command : command -> env -> env *)
let rec eval_command command env =
  match command with
  | Skip -> env
  | Seq (command1, command2) ->
     let new_env = eval_command command1 env in
     eval_command command2 new_env
  | While (b_exp, command) ->
     if eval_b_exp b_exp env
     then eval_command (Seq (command, While (b_exp, command))) env
     else env
  | Assign (var, a_exp)->
     let value = eval_a_exp a_exp env in
     update var value env
	    
