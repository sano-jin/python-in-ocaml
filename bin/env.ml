(** Environment *)

open Util
open Parsing
open Syntax

let lookup envs var =
  match OptionExtra.one_of (List.assoc_opt var <. ( ! )) envs with
  | Some v -> Ok !v
  | None -> failwith @@ "unbound variable " ^ var

let system_stmt =
  let home_path = Sys.getenv "PYTHON_IN_OCAML_HOME" in
  read_and_parse @@ home_path ^ "/lib/system.py"

(** 全てのオブジェクトの基底クラス *)
let object_class_obj_ref, init_bindings =
  let variables_ref = ref [] in
  let this_obj_val = ObjectVal variables_ref in
  let init_bindings =
    [
      ("object", ref this_obj_val);
      ("print", ref @@ SystemFunVal "print");
      ("repl", ref @@ SystemFunVal "repl");
    ]
  in
  variables_ref :=
    [
      ("__init__", ref @@ LambdaVal ([ "_" ], Return (Var "None"), []));
      ("__mro__", ref @@ ObjectVal (ref [ ("object", ref this_obj_val) ]));
      ("__bases__", ref @@ ObjectVal (ref []));
      ( "__str__",
        ref
        @@ LambdaVal
             ( [ "self" ],
               Return (App (Var "repl", [ Var "self" ])),
               [ ref init_bindings ] ) );
    ];
  (ref this_obj_val, init_bindings)
