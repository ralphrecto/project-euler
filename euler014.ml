#use "util.ml"

(* gives the length of the collatz sequence of n *)
let collatz' f n =
  if n = 1 then 1
  else if n mod 2 = 0 then 1 + (f (n/2))
  else 1 + (f ((3* n) + 1))

let collatz = Util.memoize collatz'

let find_big_collatz max =
  let rec inner (n, n_collatz) i =
    if i = max then n
    else
      let i_collatz = collatz i in
      if n_collatz < i_collatz then
        inner (i, i_collatz) (i + 1)
      else
        inner (n, n_collatz) (i + 1) in
  inner (1, collatz 1) 2

let () = print_int (find_big_collatz 1000000)
