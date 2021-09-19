(** Lexer のための補助関数などを定義する．
*)

open Parser
open Util

(** インデントレベルのスタック *)
let indent_level_stack =
  let stack = Stack.create () in
  Stack.push 0 stack;
  stack

let is_first_line = ref true

(** インデントレベルが上がった場合に，何段階上がったかを Option 型で取得する．
      マッチするインデントレベルが存在していなかった場合は [None] を返す．
  *)
let rec emit_dedents indent_level =
  let prev = Stack.top indent_level_stack in
  if indent_level > prev then None
  else if indent_level = prev then Some 0
  else (
    ignore @@ Stack.pop indent_level_stack;
    succ <$> emit_dedents indent_level)

(** インデントされていた場合は [INDENT] を返す．
     - 前の行と同じオフセットの場合は，
       最初の行なら無視してそのまま tokenizing を続け，
       それ以外なら delimiter を挿入する（C 言語でのセミコロンに対応）．
     - インデントレベルが上がっていた場合は，
       補助関数 [emit_dedents] を用いて [DEDENTS n] を返す．
   *)
let emit_indent indent_level token lexbuf =
  let current_indent_level = Stack.top indent_level_stack in
  if indent_level > current_indent_level then (
    Stack.push indent_level indent_level_stack;
    INDENT)
  else
    match emit_dedents indent_level with
    | None -> BAD_DEDENT
    | Some 0 ->
        if !is_first_line then (
          is_first_line := false;
          token lexbuf)
        else DELIMITER
    | Some n -> DEDENTS n