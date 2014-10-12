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
      if i >= bottom then
        inner (i :: acc) (i - 1)
      else acc in
    inner [] top

  let permute =
    let permute' f els =
      match els with
      | [] -> [[]]
      | _ -> 
        List.flatten (List.map (fun x -> 
          let tl_permute = f (List.filter (fun y -> y <> x) els) in
          List.map (fun subl -> x :: subl) tl_permute) els) in
    memoize permute'

end

