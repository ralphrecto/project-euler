#use "util.ml"

let l = [19; 18; 17; 16; 15; 14; 13; 12; 11;]

let find_divisible max rest = 
  let pred x = 
    List.for_all (fun a -> x mod a = 0) rest in
  let rec inner cand =
    if pred cand then cand else inner (cand + max) in
  inner max

let a = print_int (find_divisible 20 l); print_newline ()
