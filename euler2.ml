#use "util.ml"

let main2 () = 
  let almostfibo f x =
    if x = 0 then 1 else if x = 1 then 1
    else (f (x - 1)) + (f (x - 2)) in
  let fibo = Util.memoize almostfibo in
  let rec inner acc i =
    let v = fibo i in
    if v > 4000000 then acc
    else 
      if v mod 2 = 0 then
        inner (acc + v) (i + 1)
      else
        inner acc (i + 1) in
  inner 0 0
