#use "util.ml"

let to100 = Util.range 1 100
let sumOfSquares = List.fold_left (fun acc x -> acc + (x * x)) 0 to100
let sum = List.fold_left ( + ) 0 to100

let () = print_int ((sum * sum) - sumOfSquares); print_newline ()
