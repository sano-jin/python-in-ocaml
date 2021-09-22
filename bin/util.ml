(** 基本的なコンビネータなど *)

(** some very basic combinators *)

(** *)
let flip f x y = f y x

let id x = x

let const x _ = x

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

(** tuple の操作のためのコンビネータ *)

(** *)
let first f (a, b) = (f a, b)

let second f (a, b) = (a, f b)

let both f (a, b) = (f a, f b)

let pair x y = (x, y)

let swap (x, y) = (y, x)

(** compositional functions *)

(** *)
let ( <. ) f g x = f (g x)

let ( <.. ) f g x y = f (g x y)

let ( <... ) f g x y z = f (g x y z)

(** monadic combinators for the Option type *)

(** *)
let ( >>= ) = Option.bind

let ( let* ) = Option.bind

let ( <$> ) = Option.map

let ( let+ ) x f = Option.map f x

let ( <|> ) l r = if Option.is_some l then l else r ()

(** f を適用してどれか一つでも Some を返したらそれを返して終わりにする *)
let rec one_of f = function [] -> None | h :: t -> f h <|> fun _ -> one_of f t

let maybe default = function None -> default | Some s -> s

(** monadic combinators for the traversible type *)

(** monadic cons *)
let ( <::> ) h t = List.cons h <$> t

(** monadic [fold_left] 
    - f を適用して，Some が帰ってきたら fold を続ける．
    - もし一度でも None が帰ってきたら，None を返す
*)
let rec foldM f acc = function
  | [] -> Some acc
  | h :: t -> f acc h >>= flip (foldM f) t

(* read lines from the given file *)
let read_file name =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        String.concat "\n" @@ List.rev ("" :: acc)
  in
  loop []

(** utility *)

let explode = List.of_seq <. String.to_seq

let implode = String.of_seq <. List.to_seq

(** make n length list with elem as the all elements *)
let repeat n elem = List.init n @@ const elem

let rec dropLast1 = function
  | [] -> failwith "cannot drop element from an empty list"
  | [ _ ] -> []
  | h :: t -> h :: dropLast1 t

let update_ref f r = r := f !r

let remove_dup comparer list =
  let rec helper result = function
    | [] -> result
    | h :: t ->
        if List.exists (comparer h) result then helper result t
        else helper (h :: result) t
  in
  List.rev @@ helper [] list

let rec unconses = function
  | [] -> ([], [])
  | [] :: ts -> unconses ts
  | (h :: t) :: ts ->
      let hs, ts = unconses ts in
      (h :: hs, t :: ts)

let rec zip_list lists =
  let hs, ts = unconses lists in
  hs :: zip_list ts
