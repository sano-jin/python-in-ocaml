(** ResultExtra *)

open Combinator

(** *)
let ( >>= ) = Result.bind

let ( let* ) = Result.bind

let ( <$> ) = Result.map

let ( let+ ) x f = Result.map f x

let ( <|> ) l r = if Result.is_ok l then l else r ()

(** monadic combinators for the traversible type *)

(** monadic cons *)
let ( <::> ) h t = List.cons h <$> t

(** monadic [List.fold_left] 
    - f を適用して，Ok が帰ってきたら fold を続ける．
    - もし一度でも Error が帰ってきたら，Error を返す
*)
let rec foldM f acc = function
  | [] -> Ok acc
  | h :: t -> f acc h >>= flip (foldM f) t

(** monadic [List.map] 
    - f を適用して，Ok が帰ってきたら map を続ける．
    - もし一度でも Error が帰ってきたら，Error を返す
*)
let rec map_results f = function
  | [] -> Ok []
  | h :: t ->
      let* h = f h in
      let+ t = map_results f t in
      h :: t
