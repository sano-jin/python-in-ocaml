(* Object *)

open Syntax
open Util
open Util.ResultExtra

(*
(** extract_object_variables_ref *)
let dir = function
  | ObjectVal obj_variables_ref -> !obj_variables_ref
  | SystemFunVal name ->
      failwith @@ "__dir__ ing built_in_function_or_method " ^ name
      ^ ". This expected to be an object"
  | value -> failwith @@ string_of_value value ^ " is expected to be an object"

let dir_prop = dir <. ( ! ) <.. List.assoc
*)

let classObjVal class_obj_record = ClassObjVal class_obj_record

(** クラスオブジェクトの mro を取得する
@param base_classes クラスの基底クラスのリスト
*)
let mro_of_class base_classes =
  let base_classes_of class_obj = class_obj.__bases__ in
  let rec helper base_classes =
    match List.map base_classes_of base_classes with
    | [] -> []
    | base_base_classes ->
        base_classes :: (helper @@ List.concat base_base_classes)
  in
  ListExtra.remove_dup ( == ) @@ List.concat @@ helper base_classes

(** クラス変数を取得する *)
let extract_class_variable_opt class_var_obj = function
  | "__name__" -> Ok (StringVal class_var_obj.__name__)
  | "__mro__" -> Ok (TupleVal (List.map classObjVal class_var_obj.__mro__))
  | "__bases__" -> Ok (TupleVal (List.map classObjVal class_var_obj.__bases__))
  | prop -> (
      let lookup_class_vars class_obj_val =
        List.assoc_opt prop !(class_obj_val.class_vars)
      in
      match OptionExtra.one_of lookup_class_vars class_var_obj.__mro__ with
      | Some prop -> Ok !prop
      | None ->
          failwith @@ "No such field " ^ prop ^ " in object " ^ snd
          @@ string_of_class_obj [] class_var_obj)

(* todo SystemFunVal だった場合はどうするか? *)
let app_instance instance_obj = function
  | LambdaVal (var :: vars, body, env :: envs) ->
      LambdaVal (vars, body, ref ((var, ref instance_obj) :: !env) :: envs)
  | other -> other

(** クラスまたはインスタンスオブジェクト [value] からそのクラスの値 [class_obj_val] を取り出す *)
let extract_class_obj_val = function
  | ClassObjVal class_val -> Ok class_val
  | InstObjVal inst_val -> Ok inst_val.__class__
  | value ->
      failwith @@ string_of_value value
      ^ " is expected to be a instance or a class object"

(** オブジェクトからプロパティを取得する．
インスタンスオブジェクトでクラスのメソッドを取得した場合は，そのインスタンスオブジェクトを部分適用しておく．
e.g. [inst_obj.f(...args) ---> inst_obj.__class__.f(inst_obj, ...args)]
@arg eval [eval_exp envs]
 *)
let extract_variable_opt eval prop =
  let access_non_obj class_name value =
    let* class_obj = eval @@ Var class_name in
    match class_obj with
    | ClassObjVal class_obj_val ->
        app_instance value <$> extract_class_variable_opt class_obj_val prop
    | value ->
        failwith @@ string_of_value value ^ " is expected to be a class object"
  in
  function
  | ClassObjVal class_var_obj -> extract_class_variable_opt class_var_obj prop
  | InstObjVal inst_var_obj as self -> (
      if prop = "__class__" then Ok (ClassObjVal inst_var_obj.__class__)
      else
        match List.assoc_opt prop !(inst_var_obj.inst_vars) with
        | Some prop -> Ok !prop
        | None ->
            app_instance self
            <$> extract_class_variable_opt inst_var_obj.__class__ prop)
  | VoidVal as self -> access_non_obj "None" self
  | IntVal _ as self -> access_non_obj "int" self
  | BoolVal _ as self -> access_non_obj "boolean" self
  | StringVal _ as self -> access_non_obj "string" self
  | LambdaVal _ as self -> access_non_obj "function" self
  | TupleVal _ as self -> access_non_obj "tuple" self
  | SystemFunVal _ as self ->
      prerr_endline @@ "accessing a property of a built_in_function_or_method";
      access_non_obj "built_in_function_or_method" self

(** [sub_obj] が [super_obj] またはこのサブクラス，
    またはそのインスタンスオブジェクトであることを確かめる *)
let is_subclass_of sub_obj super_obj =
  let* sub_val = extract_class_obj_val sub_obj in
  let+ super_val = extract_class_obj_val super_obj in
  List.exists (( == ) super_val) sub_val.__mro__

(** クラスオブジェクトのフィールドの初期値とクラスオブジェクトを生成して返す．
@arg name このクラスの名前
@arg bases base class のリスト
@arg init_env フィールドの初期状態
@arg envs 親元の環境
*)
let init_class_obj name bases init_env envs =
  let env = ref init_env in
  let rec this_obj_vars =
    {
      __name__ = name;
      __mro__ = this_obj_vars :: mro_of_class bases;
      __bases__ = bases;
      class_vars = env;
    }
  in
  env :=
    ("__init__", ref @@ LambdaVal ([ "_" ], Return (Var "None"), env :: envs))
    :: !env;
  (env, this_obj_vars)
