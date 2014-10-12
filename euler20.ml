#use "bignum.ml"
 
let fact n =
    let rec inner acc m =
        if m > n then acc
        else 
            inner (BigNum.inttimes acc m) (m + 1) in
    inner [1] 1

let () = print_int (List.fold_left ( + ) 0 (fact 100)); print_newline ()
