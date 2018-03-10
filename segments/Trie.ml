
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Prefix Tree} *)

type 'a sequence = ('a -> unit) -> unit

(** {2 Signatures} *)

(** {6 A Composite Word}

    Words are made of characters, who belong to a total order *)

module type WORD = sig
  type t
  type char

  val compare : char -> char -> int
  val append : t -> t -> t
  val to_seq : t -> char sequence
  val of_list : char list -> t
end

module Make(W : WORD)
  (* : Sigs.S with type elt = W.t *)
= struct
  type char_ = W.char
  type elt = W.t
  
  module M = Map.Make(struct
      type t = char_
      let compare = W.compare
    end)

  type +'a tree =
    | Empty
    | Cons of char_ * 'a tree  (* simple case *)
    | Node of 'a option * 'a tree M.t

  type t = elt tree
  
  (* invariants:
     - for Path(l,t) l is never empty
     - for Node (None,map) map always has at least 2 elements
     - for Node (Some _,map) map can be anything *)

  let empty = Empty

  let _invariant = function
    | Node (None, map) when M.is_empty map -> false
    | _ -> true

  let rec _check_invariants = function
    | Empty -> true
    | Cons (_, t) -> _check_invariants t
    | Node (None, map) when M.is_empty map -> false
    | Node (_, map) ->
      M.for_all (fun _ v -> _check_invariants v) map

  let is_empty = function
    | Empty -> true
    | _ -> false

  let _id x = x

  (** Smart constructors *)

  (* sub-tree t prefixed with c *)
  let _cons c t = if is_empty t then Empty else Cons (c, t)

  (* build a Node value *)
  let _node value map = match value with
    | Some _ -> Node (value, map)
    | None ->
      if M.is_empty map then Empty
      else
      if M.cardinal map = 1
      then
        let c, sub = M.min_binding map in
        _cons c sub
      else Node (value,map)

  let _node2 c1 t1 c2 t2 =
    match is_empty t1, is_empty t2 with
    | true, true -> Empty
    | true, false -> _cons c2 t2
    | false, true -> _cons c1 t1
    | false, false ->
      let map = M.add c1 t1 M.empty in
      let map = M.add c2 t2 map in
      _node None map
  
  (** Inserting/Removing *)
  
  (* fold [f] on [seq] with accumulator [acc], and call [finish]
     on the accumulator once [seq] is exhausted *)
  let _fold_seq_and_then f ~finish acc seq =
    let acc = ref acc in
    seq (fun x -> acc := f !acc x);
    finish !acc

  let update key f t =
    (* first arg: current subtree and rebuild function; [c]: current char *)
    let goto (t, rebuild) c =
      match t with
        | Empty -> empty, fun t -> rebuild (_cons c t)
        | Cons (c', t') ->
          if W.compare c c' = 0
          then t', (fun t -> rebuild (_cons c t))
          else
            let rebuild' new_child =
              rebuild (
                if is_empty new_child then t
                else
                  let map = M.singleton c new_child in
                  let map = M.add c' t' map in
                  _node None map
              ) in
            empty, rebuild'
        | Node (value, map) ->
          try
            let t' = M.find c map in
            (* rebuild: we modify [t], so we put the new version in [map]
               if it's not empty, and make the node again *)
            let rebuild' new_child =
              rebuild (
                if is_empty new_child
                then _node value (M.remove c map)
                else _node value (M.add c new_child map)
              )
            in
            t', rebuild'
          with Not_found ->
            let rebuild' new_child =
              if is_empty new_child
              then rebuild t (* ignore *)
              else
                let map' = M.add c new_child map in
                rebuild (_node value map')
            in
            empty, rebuild'
    in
    let finish (t,rebuild) = match t with
      | Empty -> rebuild (_node (f None) M.empty)
      | Cons (c, t') ->
        rebuild
          (match f None with
            | None -> t
            | Some _ as v -> _node v (M.singleton c t')
          )
      | Node (value, map) ->
        let value' = f value in
        rebuild (_node value' map)
    in
    let word = W.to_seq key in
    _fold_seq_and_then goto ~finish (t, _id) word

  let add k v t = update k (fun _ -> Some v) t
  (* let remove k t = update k (fun _ -> None) t *)

  let singleton k v = add k v Empty
  
  (** Iter/Fold *)
  
  type 'a difflist = 'a list -> 'a list

  let _difflist_add
    : 'a difflist -> 'a -> 'a difflist
    = fun f x -> fun l' -> f (x :: l')

  (* fold that also keeps the path from the root, so as to provide the list
      of chars that lead to a value. The path is a difference list, ie
      a function that prepends a list to some suffix *)
  let rec _fold f path t acc = match t with
    | Empty -> acc
    | Cons (c, t') -> _fold f (_difflist_add path c) t' acc
    | Node (v, map) ->
      let acc = match v with
        | None -> acc
        | Some v -> f acc path v
      in
      M.fold
        (fun c t' acc -> _fold f (_difflist_add path c) t' acc)
        map acc

  (* let fold f acc t =
   *   _fold
   *     (fun acc path v ->
   *        let key = W.of_list (path []) in
   *        f acc key v)
   *     _id t acc *)

  (*$T
    T.fold (fun acc k v -> (k,v) :: acc) [] t1 \
      |> List.sort Pervasives.compare = List.sort Pervasives.compare l1
  *)

  (* let iter f t =
   *   _fold
   *     (fun () path y -> f (W.of_list (path [])) y)
   *     _id t () *)

  let rec fold_values f acc t = match t with
    | Empty -> acc
    | Cons (_, t') -> fold_values f acc t'
    | Node (v, map) ->
      let acc = match v with
        | None -> acc
        | Some v -> f acc v
      in
      M.fold
        (fun _c t' acc -> fold_values f acc t')
        map acc

  let iter_values f t = fold_values (fun () x -> f x) () t


  (** Merging operations *)
    
  let[@specialize] rec merge_with ~f ~left ~right t1 t2 = match t1, t2 with
    | Empty, _ -> right t2
    | _, Empty -> left t1
    | Cons (c1,t1'), Cons (c2,t2') ->
      if W.compare c1 c2 = 0
      then _cons c1 (merge_with ~f ~left ~right t1' t2')
      else _node2 c1 (left t1') c2 (right t2')

    | Cons (c1, t1'), Node (value, map) ->
      begin try
          (* collision *)
          let t2' = M.find c1 map in
          let new_t = merge_with ~f ~left ~right t1' t2' in
          let map' = if is_empty new_t
            then M.remove c1 map
            else M.add c1 new_t map
          in
          _node value map'
        with Not_found ->
          (* no collision *)
          assert (not(is_empty t1'));
          let t1' = left t1' in
          let map' = if is_empty t1' then map else M.add c1 t1' map in
          Node (value, map')
      end
    | Node (value, map), Cons (c2, t2') ->
      begin try
          (* collision *)
          let t1' = M.find c2 map in
          let new_t = merge_with ~f ~left ~right t1' t2' in
          let map' = if is_empty new_t
            then M.remove c2 map
            else M.add c2 new_t map
          in
          _node value map'
        with Not_found ->
          (* no collision *)
          assert (not(is_empty t2'));
          let t2' = left t2' in
          let map' = if is_empty t2' then map else M.add c2 t2' map in
          Node (value, map')
      end
    | Node(v1, map1), Node (v2, map2) ->
      let v = f v1 v2 in
      let as_option t = if is_empty t then None else Some t in 
      let map' = M.merge
          (fun _c t1 t2 -> match t1, t2 with
             | None, None -> assert false
             | Some t, None -> as_option @@ left t
             | None, Some t -> as_option @@ right t
             | Some t1, Some t2 ->
               let new_t = merge_with ~f ~left ~right t1 t2 in
               as_option new_t
          ) map1 map2
      in
      _node v map'

  let keep x = x
  let drop _ = Empty
  
  let union l l' =
    let left = keep and right = keep and f a b = match a,b with
      | Some _, _ -> a
      | None, _ -> b
    in
    merge_with ~f ~left ~right l l'

  let inter l l' = 
    let left = drop and right = drop and f a b = match a,b with
      | Some _, Some _ -> a
      | _ -> None
    in
    merge_with ~f ~left ~right l l'

  let diff l l' =
    let left = keep and right = drop and f a b = match a,b with
      | Some _, None -> a
      | _ -> None
    in 
    merge_with ~f ~left ~right l l'

  let merge l =
    OSeq.fold union Empty l

  (** Grafting/flatmap *)

  let map f t =
    let rec map_ = function
      | Empty -> Empty
      | Cons (c, t') -> Cons (c, map_ t')
      | Node (v, map) ->
        let v' = match v with
          | None -> None
          | Some v -> Some (f v)
        in let map' = M.map map_ map
        in Node (v', map')
    in map_ t

  let rec append t t0 = match t with
    | Empty -> Empty
    | Cons (c, t') -> Cons (c, append t' t0)
    | Node (Some s, m) ->
      assert (M.is_empty m);
      map (fun x -> W.append s x) t0
    | Node (v, map) ->
      let map = M.map (fun t' -> append t' t0) map in
      Node (v, map)
  
  (** Misc *)
  
  (* let rec size t = match t with
   *   | Empty -> 0
   *   | Cons (_, t') -> size t'
   *   | Node (v, map) ->
   *     let s = if v=None then 0 else 1 in
   *     M.fold
   *       (fun _ t' acc -> size t' + acc)
   *       map s *)

  let of_list l =
    List.fold_left (fun acc v -> add v v acc) empty l

  let to_seq t k = iter_values k t

  (** External API *)

  let return x = singleton x x
  let memoize x = x
  
end

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MakeArray(X : ORDERED) = Make(struct
    type t = X.t array
    type char = X.t
    let append = Array.append
    let compare = X.compare
    let to_seq a k = Array.iter k a
    let of_list = Array.of_list
  end)

module MakeList(X : ORDERED) = Make(struct
    type t = X.t list
    type char = X.t
    let append = List.append
    let compare = X.compare
    let to_seq a k = List.iter k a
    let of_list l = l
  end)

module String = Make(struct
    type t = string
    type nonrec char = char
    let append = (^)
    let compare = Char.compare
    let to_seq s k = String.iter k s
    let of_list l =
      let buf = Buffer.create (List.length l) in
      List.iter (fun c -> Buffer.add_char buf c) l;
      Buffer.contents buf
  end)
