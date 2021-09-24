(* Object *)

open Syntax
open Util
open Util.OptionExtra

(** 全てのオブジェクトの基底クラス *)
let object_class_obj_ref =
  let variables =
    [
      ("__init__", ref @@ LambdaVal ([ "_" ], Return (Var "None"), []));
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

(** [sub_class_inst_obj] が [super_obj] またはこのサブクラスの
    インスタンスオブジェクトであることを確かめる *)
let is_subclass_of sub_class_inst_obj super_obj =
  let base_classes =
    List.map (( ! ) <. snd)
    @@ dir_prop "__mro__" @@ dir_prop "__class__" @@ dir sub_class_inst_obj
  in
  List.exists (( == ) super_obj) base_classes

(** クラスオブジェクトのフィールドの初期値とクラスオブジェクトを生成して返す．
@arg name このクラスの名前
@arg bases base class とその名前の組のリスト
@arg init_env フィールドの初期状態
@arg envs 親元の環境
*)
let init_class_obj name bases init_env envs =
  let env = ref init_env in
  let this_class_obj = ObjectVal env in
  let mro = (name, ref this_class_obj) :: mro_of_class bases in
  env :=
    ("__name__", ref @@ StringVal name)
    :: ("__init__", ref @@ LambdaVal ([ "_" ], Return (Var "None"), env :: envs))
    :: ("__mro__", ref @@ ObjectVal (ref mro))
    :: ("__bases__", ref @@ ObjectVal (ref bases))
    :: !env;
  (env, this_class_obj)
