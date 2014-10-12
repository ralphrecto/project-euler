#use "bignum.ml"
#use "util.ml"
 
let fibo' f n = 
    if n = 0 then [1]
    else if n = 1 then [1]
    else BigNum.add (f (n - 1)) (f (n - 2))

let fibo = Util.memoize fibo'

let fibo_len term_len =
    let rec inner n =
        if List.length (fibo n) >= term_len then n
        else inner (n + 1) in
    inner 0 

let () = print_int (fibo_len 1000); print_newline ()
