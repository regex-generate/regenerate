module Make (K : Sigs.OrderedMonoid) = struct
  include CCSet.Make(K)

  let merge = List.fold_left union empty
  let memoize x = x
  let return = singleton
  let append s1 s2 =
    fold (fun x1 acc1 ->
        fold (fun x2 acc2 -> add (K.append x1 x2) acc2) s2 acc1) s1 empty
end
