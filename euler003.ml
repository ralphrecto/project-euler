#use "util.ml"

let factorize' f n =  
  let rec inner m =
    if n = m then [m]
    else if n mod m = 0 then
      List.rev_append (f m) (f (n/m))
    else inner (m + 1) in
  if n = 1 then [1]
  else inner 2

let factorize = Util.memoize factorize'

let () = print_int (Util.listmax (factorize 600851475143))
