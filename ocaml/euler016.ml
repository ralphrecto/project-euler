#use "bignum.ml"

let power base exponent = 
  let rec inner acc i =
    if i = exponent then acc
    else inner (BigNum.inttimes acc base) (i + 1) in
  inner [1] 0

let ans = List.fold_left ( + ) 0 (power 2 1000)
