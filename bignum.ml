module BigNum = struct
    type bignum = int list

    let pad_zeroes (l1: bignum) (l2: bignum): bignum * bignum =
        let pdr, pde = if List.length l1 > List.length l2 then l1, l2 else l2, l1 in
        let rec inner acc =
            if List.length acc <> List.length pdr then inner (0::acc)
            else acc in
        (inner pde, pdr)

    let add l1 l2 = 
        let foldf (d1, d2) (carry, acc_lst) =
            let n = d1 + d2 + carry in
            (n / 10, (n mod 10)::acc_lst) in
        let l1', l2' = pad_zeroes l1 l2 in
        let carry, acc = List.fold_right foldf (List.combine l1' l2') (0, []) in
        if carry <> 0 then carry::acc else acc

    let rec inttimes (l: bignum) (n: int) = 
        let rec inner acc m =
            if m = 1 then acc
            else inner (add acc l) (m - 1) in
        inner l n
end
