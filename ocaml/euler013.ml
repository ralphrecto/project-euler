#use "bignum.ml"
#use "monadic.ml"
#use "util.ml"

open ListMonad

let char_to_int c =
  let base = int_of_char '0' in
  (int_of_char c) - base

let first n =
  let x = ref n in
  fun a ->
    if !x > 0 then (x:= !x -1; true)
    else false

let alist = Core.Std.In_channel.read_lines "./txt/euler013.txt" >>=
            fun s -> return (BatString.to_list s) >>=
            fun clist -> return (List.map char_to_int clist)

let ans = Util.take_first 10 (List.fold_left BigNum.add [0;] alist)
