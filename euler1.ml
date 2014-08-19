#use "util.ml"

let main1 () =
  let filter_sum acc x =
    if x mod 3 = 0 or x mod 5 = 0 then
      acc + x
    else acc in
  List.fold_left filter_sum 0 (Util.range 1 999)