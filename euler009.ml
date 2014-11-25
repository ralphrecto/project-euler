
let pred a b c = 
  ((a * a) + (b * b) = (c * c)) && (a < b) && (b < c)
 
let rec triplets a b c =
  if pred a b c then Some (a, b, c)
  else
    if a = 0 then
      if b = 0 then None
      else triplets a (b-1) (c+1)
    else
      if b = 0 then triplets (a-1) (c+1) 0
      else triplets a (b-1) (c+1)

let ans = match (triplets 1000 0 0) with 
  | Some (x, y, z) -> x * y * z
  | None -> failwith "didn't find :("
