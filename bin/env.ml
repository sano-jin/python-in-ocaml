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

let repl =
  LambdaVal
    ( [ "self" ],
      Return (App (Var "repl", [ Var "self" ])),
      [ ref [ ("repl", ref @@ SystemFunVal "repl") ] ] )

(** 全てのオブジェクトの基底クラス *)
let object_class_obj =
  ClassObjVal
    (snd
       (Object.init_class_obj "object" []
          [ ("__repl__", ref @@ repl); ("__str__", ref @@ repl) ]
          []))

let init_bindings =
  [
    ("repl", ref @@ SystemFunVal "repl"); ("print", ref @@ SystemFunVal "print");
  ]
