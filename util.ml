#require "batteries"
#require "core"

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
        BatList.flatten (BatList.map (fun x -> 
          let tl_permute = f (BatList.remove els x) in
          BatList.map (fun subl -> x :: subl) tl_permute) els) in
    memoize permute'

  let listmax l =
    List.fold_left (fun max n -> if n > max then n else max) min_int l

  let take_first k =
    let first n =
      let x = ref n in
      fun a ->
        if !x > 0 then (x:= !x -1; true)
        else false in
    BatList.take_while (first k)

  let factorize =
    let factorize' f n =  
      let rec inner m =
        if n = m then [m]
        else if n mod m = 0 then
          List.rev_append (f m) (f (n/m))
        else inner (m + 1) in
      if n = 1 then [1]
      else inner 2 in
    memoize factorize'

  let unique l =
    let rec inner acc subl =
      match subl with
      | [] -> List.rev acc
      | hd :: tl -> 
          if List.mem hd tl then inner acc tl
          else inner (hd::acc) tl in
    inner [] l

end

