
let rec count_coin amt coins =
    match coins with
    | [] -> 0;
    | hd::tl ->;
      if hd = amt then 1
      else if hd > amt then 0
      else count_coin (amt - hd) coins + count_coin amt tl
