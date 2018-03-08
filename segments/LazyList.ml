module L = CCLazy_list
open L.Infix

let next = Lazy.force

module Make (K : Set.OrderedType) = struct

  type elt = K.t
  type t = elt L.t

  let empty = L.empty
  let is_empty = L.is_empty
  let return = L.return
  let map f = L.map ~f

  let of_list = L.of_list                 
  let iter f l =
    let rec aux l = match next l with
      | L.Cons (x, t) -> (f x; aux t)
      | Nil -> ()
    in aux l
  let to_seq x f = iter f x
      
      
  let memoize x = x
  (* let pp = pp *)

  type drop = Drop | Keep
  let dropX s s' = function Drop -> s' | Keep -> s
  let rec merge_with l r f s1 s2 = lazy (match Lazy.force s1, Lazy.force s2 with
    | L.Nil, L.Nil -> L.Nil
    | Cons _, Nil -> l s1
    | Nil, Cons _ -> r s2
    | Cons (x1, s1'), Cons (x2, s2') ->
      let d1, d2, res = f x1 x2 in
      let k = merge_with l r f (dropX s1 s1' d1) (dropX s2 s2' d2) in
      match res with
      | Some x -> Cons (x, k)
      | None -> Lazy.force k
    )
  let keep = Lazy.force
  let drop _ = L.Nil

  let union =
    let f x y =
      let i = K.compare x y in
      if i = 0 then      Drop, Drop, Some x
      else if i < 0 then Drop, Keep, Some x
      else               Keep, Drop, Some x
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

  let difference =
    let f x y =
      let i = K.compare x y in
      if i = 0 then      Drop, Drop, None
      else if i < 0 then Drop, Keep, Some x
      else               Keep, Drop, None
    in
    merge_with keep drop f

  let cross_product f l1 l2 =
    l1 >>= fun x -> l2 >|= fun y -> f x y

  (** merge *)

  module Heap = struct
    type 'a t = {
      tree : 'a tree;
      cmp : 'a -> 'a -> int;
      merge : 'a -> 'a -> 'a;
    } (** A pairing tree heap with the given comparison function *)

    and 'a tree =
      | Empty
      | Node of 'a * 'a tree * 'a tree

    let empty ~cmp ~merge = {
      tree = Empty;
      cmp;
      merge;
    }

    let is_empty h =
      match h.tree with
      | Empty -> true
      | Node _ -> false

    let rec union ({cmp;merge;_} as h) t1 t2 = match t1, t2 with
      | Empty, _ -> t2
      | _, Empty -> t1
      | Node (x1, l1, r1), Node (x2, l2, r2) ->
        let c = cmp x1 x2 in
        if c = 0 then Node (merge x1 x2, union h r1 r2, union h l1 l2)
        else if c < 0 then Node (x1, union h t2 r1, l1)
        else Node (x2, union h t1 r2, l2)

    let insert h x =
      { h with tree = union h (Node (x, Empty, Empty)) h.tree }

    let pop h = match h.tree with
      | Empty -> raise Not_found
      | Node (x, l, r) ->
        x, { h with tree = union h l r }
  end

  let merge l =
    let cmp (v1,_) (v2,_) = K.compare v1 v2 in
    let merge (x1, s1) (_, s2) = (x1, s1@s2) in
    let push h s =
      match Lazy.force s with L.Nil -> h | Cons (x, s') -> Heap.insert h (x, [s'])
    in
    let h0 = OSeq.fold push (Heap.empty ~cmp ~merge) l in
    let rec next heap =
      lazy (
        if Heap.is_empty heap then L.Nil else begin
          let (x, seq), heaps = Heap.pop heap in
          let new_heap = List.fold_left push heaps seq in
          L.Cons (x, next new_heap)
        end
      )
    in
    next h0

end
