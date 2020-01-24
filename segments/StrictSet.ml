module Make (Elt : Sigs.OrderedMonoid)
  : Sigs.S with type elt = Elt.t and type t = Set.Make(Elt).t
= struct
  include Set.Make(Elt)

  
  let merge = List.fold_left union empty
  let memoize x = x
  let return = singleton
  let append s1 s2 =
    fold (fun x1 acc1 ->
        fold (fun x2 acc2 -> add (Elt.append x1 x2) acc2) s2 acc1) s1 empty
  let to_iter e k = iter k e
end
