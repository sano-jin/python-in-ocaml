(** Environment *)

open Util
open Parsing

let lookup envs var =
  match OptionExtra.one_of (List.assoc_opt var <. ( ! )) envs with
  | Some v -> Ok !v
  | None -> failwith @@ "unbound variable " ^ var

let system_stmt =
  let home_path = Sys.getenv "PYTHON_IN_OCAML_HOME" in
  read_and_parse @@ home_path ^ "/lib/system.py"
