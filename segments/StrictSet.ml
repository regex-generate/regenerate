module Make (Elt : Sigs.OrderedMonoid) = struct
  include CCSet.Make(Elt)

  let merge = List.fold_left union empty
  let memoize x = x
  let return = singleton
  let append s1 s2 =
    fold (fun x1 acc1 ->
        fold (fun x2 acc2 -> add (Elt.append x1 x2) acc2) s2 acc1) s1 empty
end
