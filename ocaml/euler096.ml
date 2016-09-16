#require "core"
#require "batteries"
#use "util.ml"

open ListMonad

(* sq, cell, row *)
type cellnpos = int * int * int

(* value, possibleValues *)
type cell = {pos: cellpos; value: int option; val_opts: int list}

type board = cell list

exception BadConstaint
exception BadBoard

let neighbor (c1: cell) (c2: cell) = 
  let s1, r1, c1 = c1.pos in
  let s2, r2, c2 = c2.pos in
  s1 = s2 || r1 = r2 || c1 = c2

let has_val (c: cell): bool = 
  match c.value with
  | Some _ -> true
  | None -> false

let add_constraint (c: cell) (n: int): cell = 
  {c with val_opts = List.filter (fun x -> x <> n) (c.val_opts)}

(* if c already has a val, do nothing *)
let set_val (c: cell) (n: int): cell =
  {c with value = Some n}

let bad_cell (c: cell): bool =
  if has_val c then true else (List.length c.val_opts) = 0

let game_over = List.for_all has_val

let change_board (b: board)(c: cell)(n: int) = 
  let f acc el =
    let el' = if el.pos = c.pos then
      set_val el n else el in
    el' :: acc in
  let b' = List.fold_left f [] b in
  let g e = 
    if neighbor e c then add_constraint e n else e in
  List.map g b'

let solvercmp (c1: cell)(c2: cell) =
  if has_val c1 && has_val c2  then 0
  else if has_val c1 then 1
  else if has_val c2 then -1
  else compare (List.length (c1.val_opts)) (List.length (c2.val_opts))

(* returns Some c where c is a solution;
 * returns None if board is impossible to solve *)
let rec solve (b: board): board option =
  let b' = List.sort solvercmp b in
  match b' with
  | [] -> raise BadBoard
  | c::rest ->
      if game_over b' then Some b'
      else if bad_cell c then None
      else
        let results = 
          List.map (fun x -> solve (change_board b' c x)) c.val_opts in
        let find acc result =
          match acc, result with
          | None, _  -> result
          | Some _, _ -> acc in
        List.fold_left find None results

let sq row col = (row/3)*3 + (col/3)

let new_board =
  let pairs = BatList.cartesian_product (Util.range 0 8) (Util.range 0 8) in
  let f (row, col) = 
    {pos = (sq row col, row, col); value = None; val_opts = (Util.range 0 8)} in
  List.map f pairs

let char_to_int ch =
  (int_of_char ch) - (int_of_char '0')

let boards = 
  let strlist = Core.Std.In_channel.read_lines "./txt/euler096.txt" in
  let listnum = List.combine strlist (Util.range 0 ((List.length strlist)-1)) in
  let f (acc, cur_board) (str, ln) =
    let row = ln mod 10 in
    if row = 0 then (cur_board::acc, new_board)
    else 
      let mapi_f col ch = (col, ch) in
      let chars = BatList.mapi mapi_f (BatString.to_list str) in
      let fold_f board' (col, ch) = 
        let c = {pos = (sq row col, row, col); value = None; val_opts = []} in
        let i = char_to_int ch in
        if i <> 0 then
          change_board board' c (char_to_int ch) 
        else board' in
      (acc, List.fold_left fold_f cur_board chars) in
  let acc, last_board = List.fold_left f ([], new_board) listnum in
  last_board::acc

let c00 = {pos = (0, 0, 0); value = None; val_opts = [0;1]}
let c01 = {pos = (1, 0, 1); value = None; val_opts = [0;1]}
let c10 = {pos = (2, 1, 0); value = None; val_opts = [0;1]}
let c11 = {pos = (3, 1, 1); value = None; val_opts = [0;1]}
let testb = [c00; c01; c10; c11]
