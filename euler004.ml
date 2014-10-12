#use "bignum.ml"

let is_palindrome l = l = (List.rev l)

let find_palindrome lbound ubound =
  let ubound_int = BigNum.to_int ubound in
  let lbound_int = BigNum.to_int lbound in
  let one_l = [1;] in
  let rec inner acc cur_l cur_n =
    if cur_l = ubound then acc
    else if cur_n > ubound_int then 
      inner acc (BigNum.add cur_l one_l) lbound_int
    else
      let cand = BigNum.inttimes cur_l cur_n in
      if is_palindrome cand then
        inner (cand::acc) cur_l (cur_n + 1)
      else
        inner acc cur_l (cur_n + 1) in
  let cands = inner [] lbound lbound_int in
  print_int (List.length cands);
  List.fold_left (fun max cand ->
    let cand_n = BigNum.to_int cand in
    if max < cand_n then cand_n else max) (-1) cands

(* this is pretty slow...takes a few mins to solve #4 *)
let () = print_int (find_palindrome [9;0;0;] [9;9;9])
