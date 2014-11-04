#require "core"
#require "batteries"

let str = List.hd (Core.Std.In_channel.read_lines "./txt/euler022.txt")
let slist = List.map (BatString.strip ~chars:"\"") (BatString.nsplit str ",")
let cmp x y = if x = y then 0 else if x > y then 1 else -1
let points (c: char) = 
  let base = (int_of_char 'A') - 1 in
  (int_of_char c) - base

let foldf (pts, i) s =
  let newpts = List.fold_left ( + ) 0 (List.map points (BatString.explode s)) in
  ((newpts*i) + pts, i+1)

let _ = List.fold_left foldf (0, 1) (List.fast_sort cmp slist)
