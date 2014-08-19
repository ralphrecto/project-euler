module Util = struct
  let rec memoize f =
    let memo = Hashtbl.create 10000 in
    let rec inner g x =
      if Hashtbl.mem memo x then
        Hashtbl.find memo x
      else 
        let v = g (inner g) x in
        Hashtbl.add memo x v;
        v in
    inner f

  let range bottom top =
    let rec inner acc i =
      if i != bottom then
        inner (i :: acc) (i - 1)
      else acc in
    inner [] top
end

