let primes n =
  let check l k =
    List.for_all (fun x -> k mod x <> 0) l in
  let rec inner acc i =
    if List.length acc = n then acc
    else
      if check acc i then
        inner (acc @ [i]) (i+2)
      else
        inner acc (i+2) in
  inner [2] 3 

List.nth (primes 10001) 10000
