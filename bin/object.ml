(* Object *)

open Syntax
open Util
open Util.OptionExtra

(** 全てのオブジェクトの基底クラス *)
let object_class_obj_ref =
  let variables =
    [
      ("__init__", ref @@ LambdaVal ([ "_" ], Skip, []));
      ("__mro__", ref @@ ObjectVal (ref []));
      ("__bases__", ref @@ ObjectVal (ref []));
    ]
  in
  ref @@ ObjectVal (ref variables)

(** extract_object_variables_ref *)
let dir = function
  | ObjectVal obj_variables_ref -> !obj_variables_ref
  | value -> failwith @@ string_of_value value ^ " is expected to be an object"

let dir_prop = dir <. ( ! ) <.. List.assoc

(** クラスオブジェクトの mro を取得する
@param base_classes クラスの規定クラスのリスト
*)
let mro_of_class base_classes =
  let base_classes_of = dir_prop "__bases__" <. dir <. ( ! ) <. snd in
  let rec helper base_classes =
    match List.map base_classes_of base_classes with
    | [] -> []
    | base_base_classes ->
        base_classes :: (helper @@ List.concat base_base_classes)
  in
  ListExtra.remove_dup (fun (_, class_obj_ref1) (_, class_obj_ref2) ->
      !class_obj_ref1 == !class_obj_ref2)
  @@ List.concat @@ helper base_classes

(** クラス変数を取得する *)
let extract_class_variable_opt class_fields prop =
  let base_classes = dir_prop "__mro__" class_fields in
  let extract_base_class_variable_opt =
    Option.map ( ! ) <. List.assoc_opt prop <. dir <. ( ! ) <. snd
  in
  OptionExtra.one_of extract_base_class_variable_opt base_classes

let dir_class = dir_prop "__class__"

let app_instance instance_obj = function
  | LambdaVal (var :: vars, body, env :: envs) ->
      LambdaVal (vars, body, ref ((var, ref instance_obj) :: !env) :: envs)
  | other -> other

(** オブジェクトからプロパティを取得する．
インスタンスオブジェクトでクラスのメソッドを取得した場合は，そのインスタンスオブジェクトを部分適用しておく．
e.g. [inst_obj.f(...args) ---> inst_obj.__class__.f(inst_obj, ...args)]
 *)
let extract_variable_opt obj prop =
  let obj_fields = dir obj in
  let is_instance = List.mem_assoc "__class__" obj_fields in
  if is_instance then
    match List.assoc_opt prop obj_fields with
    | Some prop -> Some !prop
    | None ->
        app_instance obj
        <$> extract_class_variable_opt (dir_class obj_fields) prop
  else extract_class_variable_opt obj_fields prop
