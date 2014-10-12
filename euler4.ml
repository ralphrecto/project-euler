#use "bignum.ml"

let is_palindrome l = l = (List.rev l)

let find_palindrome lower_bound upper_bound = 
    let one = [1;] in
    let rec inner cur n = 
        let prod = BigNum.inttimes cur n in
        if is_palindrome prod then prod
        else if n = lower_bound then 
            inner (BigNum.add cur one) upper_bound
        else 
            inner cur (n - 1) in
    inner [9; 5; 0;] 999


