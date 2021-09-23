(** リスト系の追加関数
    - List.Extra モジュールなどとして定義した方が良いかも
 *)

open Combinator
(** open basic combinators *)

open Option_extra

(** リストの要素の添字番号を取得する *)
let index_of elem =
  let rec helper index = function
    | [] -> None
    | h :: t -> if h = elem then Some index else helper (succ index) t
  in
  helper 0

(**  [('a -> 'b -> 'a * ('c * 'd list)) -> 'a -> 'b list -> 'a * ('c list * 'd list)]
     2重の fold_left_map．
 *)
let fold_left_map2 f acc xs =
  let acc, ys = List.fold_left_map f acc xs in
  let zs, ws = List.split ys in
  (acc, (zs, List.concat ws))

(** 要素に Option を返す関数を適用して，初めて Some になったところでリストを分割する
    - Some になった値も返す
    - None になった部分のリストは反転して返すことに注意
      - [rev_appned] をすると元のリストから一つ要素を除いたものになる
    - [break_opt (fun x -> if x > 3 then Some x else None) [1; 2; 3; 4; 5; 6]
    ---> Some (4, ([3; 2; 1], [5; 6]))
    ]
    - Tail-recursive
    @return a list, (Some (f a, a list) | None)
 *)
let rev_break_opt f =
  let rec helper left = function
    | [] -> None
    | h :: t -> (
        match f h with
        | None -> helper (h :: left) t
        | Some s -> Some (s, (left, t)))
  in
  helper []

(** Tail-recursive List.concat with List.rev_append
    - リストのリストを反転させながら結合する
    - [rev_concat [[6; 5; 4]; [3; 2]; [1]] [7; 8]
    ---> rev_append [1] (rev_append [3; 2] (rev_append [6; 5; 4] [7; 8]))
    ---> [1; 2; 3; 4; 5; 6; 7; 8]
    ]
 *)
let rev_concat_append lists list =
  List.fold_left (flip List.rev_append) list lists

(** Tail-recursive List.concat
    - リストのリストを反転させながら結合する
    - [rev_concat [[1; 2; 3]; [4; 5]; [6]]
    ---> rev_append [6] (rev_append [4; 5] (rev_append [1; 2; 3] []))
    ---> [6; 5; 4; 3; 2; 1]
    ]
 *)
let rev_concat lists = rev_concat_append [] lists

(** [List.fold_left] with indices  *)
let fold_lefti f =
  let rec helper i acc = function
    | [] -> acc
    | h :: t -> helper (succ i) (f i acc h) t
  in
  helper 0

(** [List.fold_left_map] with indices  *)
let fold_left_mapi f acc xs =
  let rec helper i acc = function
    | [] -> (acc, [])
    | h :: t ->
        let acc, h = f i acc h in
        let acc, t = helper (succ i) acc t in
        (acc, h :: t)
  in
  helper 0 acc xs

(** リストを "回転" する
    - [roll [1; 2; 3; 4] ---> [2; 3; 4; 1]]
    - TODO: もっと効率の良い実装にする．キューを使うなど
 *)
let roll = function [] -> [] | h :: t -> t @ [ h ]

(** Monadic while.
    Tail recursive.
    @param f Option 型を返す関数
    @param x 最初の入力値
    @return f をゼロ回以上適用して None になったら，その直前の x を返す
 *)
let rec whileM f x = match f x with None -> x | Some x -> whileM f x

(** リストの末尾を除去したリストを返す *)
let rec dropLast1 = function
  | [] -> failwith "cannot drop the last element from an empty list"
  | [ _ ] -> []
  | h :: t -> h :: dropLast1 t

(** リストの要素の重複を取得する *)
let get_dup_opt l =
  let rec helper rest = function
    | [] -> None
    | h :: t -> if List.mem h rest then Some h else helper (h :: rest) t
  in
  helper [] l

(** filter_map with indices  *)
let filter_mapi f l =
  let rec helper i = function [] -> [] | h :: t -> f h :: helper (succ i) t in
  helper 0 l

(** randomize list *)
let rec shuffle = function
  | [] -> []
  | [ x ] -> [ x ]
  | list ->
      let before, after = List.partition (fun _ -> Random.bool ()) list in
      List.rev_append (shuffle after) (shuffle before)

(** Remove duplications in a list *)
let remove_dup comparer list =
  let rec helper result = function
    | [] -> result
    | h :: t ->
        if List.exists (comparer h) result then helper result t
        else helper (h :: result) t
  in
  List.rev @@ helper [] list

(** make n length list with elem as the all elements *)
let repeat n elem = List.init n @@ const elem

(** Either 型の要素のリストを左右に振り分ける *)
let partitionEithers l = List.partition_map id l

(** 以下試作であまり価値がなさそう *)

(** リストのリストをそれぞれ head と tail に分けてそれぞれリストにしたタプルを返す．
transpose のための補助関数．
[unconses [[1; 2; 3]; []; [4; 5]] = ([1; 4], [[2; 3]; [5]])]

*)
let rec unconses = function
  | [] -> ([], [])
  | [] :: ts -> unconses ts
  | (h :: t) :: ts ->
      let hs, ts = unconses ts in
      (h :: hs, t :: ts)

(** リストのリストの転置
[transpose [[1; 2; 3]; []; [4; 5]] = [[1; 4]; [2; 5]; [3]]]
*)
let rec transpose lists =
  let hs, ts = unconses lists in
  hs :: transpose ts

(** safe [List.combine] *)
let combine_opt list1 list2 =
  let rec helper = function
    | [], [] -> Some []
    | h1 :: t1, h2 :: t2 ->
        let+ t = helper (t1, t2) in
        (h1, h2) :: t
    | _ -> None
  in
  helper (list1, list2)
