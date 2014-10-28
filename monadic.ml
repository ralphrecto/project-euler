module type Monad = sig
  type 'a t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
end

module ListMonad: Monad with type 'a t = 'a list = struct
  type 'a t = 'a list
  let return x = [x]
  let ( >>= ) l f = List.flatten (List.map f l)
end
