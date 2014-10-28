
module MyStream = struct
  type 'a t = Nil | Cons of 'a * (unit -> 'a t)

  let take (s: 'a t) (n: int): 'a list =
    let rec inner acc s' m =
      match s' with
      | Nil -> acc
      | Cons (el, thunk) ->
        if m = n then List.rev acc
        else inner (el::acc) (thunk ()) (m + 1) in
    inner [] s 0

  let rec map (s: 'a t) (f: 'a -> 'b): 'b t =
    match s with
    | Nil -> Nil
    | Cons (el, thunk) -> 
        Cons (f el, (fun () -> map (thunk ()) f))

  let rec filter (s: 'a t)(f: 'a -> bool): 'a t =
    match s with
    | Nil -> Nil
    | Cons (el, thunk) ->
        if f el then Cons (el, fun () -> filter (thunk ()) f)
        else filter (thunk ()) f

  let rec merge (s1: 'a t)(s2: 'a t)(f: 'a -> 'a -> 'a): 'a t =
    match s1, s2 with
    | Nil, _ -> s2
    | _, Nil -> s1
    | Cons (el1, thunk1), Cons (el2, thunk2) ->
        Cons (f el1 el2, fun () ->
          merge (thunk1 ()) (thunk2 ()) f)
          
  let rec ones = Cons (1, (fun () -> ones))
  let rec nats = Cons (1, (fun () -> merge nats ones ( + )))
  let rec from2 = Cons (2, (fun () -> merge from2 ones ( + )))

  let rec sieve (s: 'a t) =
    match s with
    | Nil -> Nil
    | Cons (el, thunk) ->
        Cons (el, fun () -> sieve (filter (thunk ()) (fun x -> (x mod el) <> 0)))

  let primes = sieve from2

  let rec const n = Cons (n, fun () -> const n)
  let rec skip n = Cons (n, fun () -> merge (const n) (skip n) ( + ))

end

open MyStream
