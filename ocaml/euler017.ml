#use "bignum.ml"

let hundreds = [ (1, 13); (2, 13); (3, 15); (4, 14); (5, 14); (6, 13); (7, 15); (8, 15); (9, 14); ]
let tens = [ (0, 0); (2, 6); (3, 6); (4, 5); (5, 5); (6, 5); (7, 7); (8, 6); (9, 6); ]

let teens = [(10, 3); (11, 6); (12, 6); (13, 8); (14, 8); (15, 7); (16, 7); (17,
9); (18, 8); (19, 8)]

let ones = [ (0, 0); (1, 3); (2, 3); (3, 5); (4, 4); (5, 4); (6, 3); (7, 5); (8, 5); (9, 4); ]

let rec count (l: int list): int =
  match l with
  | [] -> 0
  | [x] -> List.assoc x ones
  | [x;y] when x = 1 -> print_string "hi\n"; List.assoc ((x * 10) + y) teens
  | [x;y] -> (List.assoc x tens) + (List.assoc y ones)
  | [x;y;z;] when y = 0 && z = 0 -> (List.assoc x hundreds) - 3
  | [x;y;z;] -> (List.assoc x hundreds) + (count [y;z;])
  | _ -> failwith "sry don't handle these cases"

let counter n =
  let one = [1;] in
  let rec inner acc cur_num =
    print_int (BigNum.to_int cur_num);
    print_newline ();
    if BigNum.to_int cur_num > n then acc
    else inner (acc + (count cur_num)) (BigNum.add cur_num one) in
  inner 0 one

