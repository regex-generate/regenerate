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
