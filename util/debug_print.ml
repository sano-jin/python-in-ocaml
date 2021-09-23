(** A module for print debugging.
    Usecases: [
    let f x = 
      (print_debug @@ "x is now " ^ string_of_int x;
      (* this is indented*)
      1 + x)
      >>> incr_indent ()
    in f 3
    ]
     *)

(** Add 4 * n white spaces to the head of the string
    - ["\t"] の方が良いかも
 *)
let indent n = ( ^ ) @@ String.make (4 * n) ' '

let debug_print_indent = ref 0

type indent_was = INDENT_WAS of int

let incr_indent () =
  let old_indent = !debug_print_indent in
  incr debug_print_indent;
  INDENT_WAS old_indent

let ( >>> ) value = function
  | INDENT_WAS old_indent ->
      debug_print_indent := old_indent;
      value

let print_debug str = prerr_endline @@ indent !debug_print_indent str

let usecase x =
  (print_debug @@ "x is now " ^ string_of_int x;
   1 + x)
  >>> incr_indent ()
