#require "core"
#require "batteries"

let strlist = Core.Std.In_channel.read_lines "./txt/euler008.txt"
let str = List.fold_left ( ^ ) "" strlist
let charlist = BatString.to_list str

let series_product len = 
  let base = int_of_char '0' in
  let foldf (i, maxval, curlist) x =
    let newi = i + 1 in
    let x = (int_of_char x) - base in
    if newi < len then
      let newval = List.fold_left ( * ) 1 (x :: curlist) in
      let newlist = curlist @ [x] in
      (newi, newval, newlist)
    else match curlist with
    | [] -> failwith "impossibru"
    | hd :: tl ->
        let newval = List.fold_left ( * ) 1 (x :: tl) in
        let newlist = tl @ [x] in
        if newval > maxval then
          (newi, newval, newlist) 
        else
          (newi, maxval, newlist) in
  let _, maxval, _ = List.fold_left foldf (-1, 1, []) charlist in
  maxval
