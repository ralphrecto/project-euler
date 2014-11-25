#use "util.ml"

(* idea: divisors of a natural are multiplicative combinations of its prime
 * factorization. So: take power set of the set of unique prime factors *)

let foi = float_of_int

let rec subl l = 
  match l with
  | [] -> [[]]
  | hd::tl -> 
      let lessone = List.map (fun x -> List.filter (( <> ) x) l) l in
      let lessubl = List.flatten (List.map subl lessone) in
      l :: lessubl

let triangle max =
  let rec inner tnum delta =
    let numfacts = List.length (Util.unique (Util.factorize tnum)) in
    if 2.0 ** (foi numfacts) >= (foi (max - 1)) then tnum
    else inner (tnum + delta) (delta + 1) in
  inner 6 4
