
let count_to n = 
  let counter = ref 0 in
  fun () ->
    if !counter = n then true
    else counter := !counter + 1; false

let rec subsets l =
  let to_halflen = count_to ((List.length l)/2) in
  match l with
  | [] -> [[]]
