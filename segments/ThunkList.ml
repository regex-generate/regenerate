module Make (K : Sigs.OrderedMonoid)
  : Sigs.S with type elt = K.t and type t = K.t OSeq.t
= struct
  open OSeq

  type elt = K.t
  type t = elt OSeq.t

  let empty = empty
  let is_empty = is_empty
  let of_list l = of_list @@ CCList.sort_uniq ~cmp:K.compare l
  let return = return
  let to_iter x f = iter f x
  let memoize x = x

  type drop = Drop | Keep
  let dropX s s' = function Drop -> s' | Keep -> s
  let rec merge_with l r f s1 s2 () = match s1 (), s2 () with
    | Nil, Nil -> Nil
    | Cons _, Nil -> l s1 ()
    | Nil, Cons _ -> r s2 ()
    | Cons (x1, s1'), Cons (x2, s2') ->
      let d1, d2, res = f x1 x2 in
      let k = merge_with l r f (dropX s1 s1' d1) (dropX s2 s2' d2) in
      match res with
      | Some x -> Cons (x, k)
      | None -> k ()
  let keep x = x
  let drop _ = empty

  let union =
    let f x y =
      let i = K.compare x y in
      if i = 0 then      Drop, Drop, Some x
      else if i < 0 then Drop, Keep, Some x
      else               Keep, Drop, Some y
    in
    merge_with keep keep f

  let inter =
    let f x y =
      let i = K.compare x y in
      if i = 0 then      Drop, Drop, Some x
      else if i < 0 then Drop, Keep, None
      else               Keep, Drop, None
    in
    merge_with drop drop f

  let diff =
    let f x y =
      let i = K.compare x y in
      if i = 0 then      Drop, Drop, None
      else if i < 0 then Drop, Keep, Some x
      else               Keep, Drop, None
    in
    merge_with keep drop f

  let append l1 l2 =
    l1 >>= fun x -> l2 >|= fun y -> K.append x y

  let merge l =
    let cmp (v1,_) (v2,_) = K.compare v1 v2 in
    let merge (x1, s1) (_, s2) = (x1, s1@s2) in
    let push h s =
      match s() with Nil -> h | Cons (x, s') -> Heap.insert h (x, [s'])
    in
    let h0 = List.fold_left push (Heap.empty ~cmp ~merge) l in
    let rec next heap () =
      if Heap.is_empty heap then Nil else begin
        let (x, seq), heaps = Heap.pop heap in
        let new_heap = List.fold_left push heaps seq in
        Cons (x, next new_heap)
      end
    in
    next h0

end
