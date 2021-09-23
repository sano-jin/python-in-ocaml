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
