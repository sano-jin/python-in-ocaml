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

let ( <. ) f g x = f (g x)

(** utility *)

let explode = List.of_seq <. String.to_seq

let implode = String.of_seq <. List.to_seq

let flip f x y = f y x
