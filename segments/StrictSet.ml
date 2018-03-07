module Make (K : Set.OrderedType) = struct
  include CCSet.Make(K)

  let merge = OSeq.fold union empty
  let memoize x = x
  let difference = diff
  let return = singleton
  let cross_product f s1 s2 =
    fold (fun x1 acc1 ->
        fold (fun x2 acc2 -> add (f x1 x2) acc2) s2 acc1) s1 empty
end
