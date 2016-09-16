
let arr_exists pred (arr: 'a option array) =
  let arr_len = Array.length arr in
  let rec inner i =
    if i = arr_len then false
    else
      match arr.(i) with
      | Some x when pred x -> true
      | _ -> inner (i+1) in
  inner 0

let sum_primes n =
  let primes = Array.make 200000 None in
  primes.(0) <- Some 2;
  let rec inner sum num_primes k =
    let is_prime x =
      not (arr_exists (fun p -> x mod p = 0) primes) in
    if k < n then
      if is_prime k then (
        primes.(num_primes) <- Some k;
        inner (sum+k) (num_primes+1) (k+1))
      else
        inner sum num_primes (k+1)
    else sum in
  inner 2 1 3
