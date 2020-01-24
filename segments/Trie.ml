
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Prefix Tree} *)

(** {2 Signatures} *)

(** {6 A Composite Word}

    Words are made of characters, who belong to a total order *)

module type WORD = sig
  type t
  type char

  val compare_char : char -> char -> int
  val append : t -> t -> t
  val to_iter : t -> char Iter.t
  val of_list : char list -> t
end

module Make(W : WORD)
  (* : Sigs.S with type elt = W.t *)
= struct
  type char = W.char
  type elt = W.t
  
  module M = Map.Make(struct
      type t = char
      let compare = W.compare_char
    end)

  type +'a trie =
    | Empty
    | Leaf of 'a
    | Node of 'a trie M.t

  type t = unit trie
  
  let empty = Empty

  let rec _check_invariants = function
    | Empty | Leaf (_ , _) -> true
    | Node map ->
      not (M.is_empty map) &&
      M.for_all (fun _ v -> _check_invariants v) map

  let is_empty = function
    | Empty -> true
    | _ -> false

  let _id x = x

  (** Smart constructors *)

  (* sub-tree t prefixed with c *)
  let _cons c t = if is_empty t then Empty else Node (M.singleton c t)

  let _leaf x = Leaf x
  
  (* build a Node value *)
  let _node map =
      if M.is_empty map then Empty
      else
      if M.cardinal map = 1
      then
        let c, sub = M.min_binding map in
        _cons c sub
      else Node map

  let _node2 c1 t1 c2 t2 =
    match is_empty t1, is_empty t2 with
    | true, true -> Empty
    | true, false -> _cons c2 t2
    | false, true -> _cons c1 t1
    | false, false ->
      let map = M.add c1 t1 M.empty in
      let map = M.add c2 t2 map in
      _node map
  
  (** Inserting/Removing *)
  
  (* fold [f] on [iter] with accumulator [acc], and call [finish]
     on the accumulator once [iter] is exhausted *)
  let _fold_iter_and_then f ~finish acc iter =
    let acc = ref acc in
    iter (fun x -> acc := f !acc x);
    finish !acc

  let update key f t =
    (* first arg: current subtree and rebuild function; [c]: current char *)
    let goto (t, rebuild) c =
      match t with
        | Empty | Leaf _ -> t, fun t -> rebuild (_cons c t)
        | Node map ->
          try
            let t' = M.find c map in
            (* rebuild: we modify [t], so we put the new version in [map]
               if it's not empty, and make the node again *)
            let rebuild' new_child =
              rebuild (
                if is_empty new_child
                then _node (M.remove c map)
                else _node (M.add c new_child map)
              )
            in
            t', rebuild'
          with Not_found ->
            let rebuild' new_child =
              if is_empty new_child
              then rebuild t (* ignore *)
              else
                let map' = M.add c new_child map in
                rebuild (_node map')
            in
            empty, rebuild'
    in
    let leaf_or_empty rebuild o =  match f o with
        | None -> rebuild (_node M.empty)
        | Some x' -> rebuild (_leaf x')
    in
    let finish (t,rebuild) = match t with
      | Leaf x -> leaf_or_empty rebuild @@ Some x
      | Empty -> leaf_or_empty rebuild @@ None
      | Node map -> rebuild (_node map)
    in
    let word = W.to_iter key in
    _fold_iter_and_then goto ~finish (t, _id) word

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
    | Leaf v -> f acc path v
    (* | Cons (c, t') -> _fold f (_difflist_add path c) t' acc *)
    | Node map ->
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

  let iter f t =
    _fold
      (fun () path y -> f (W.of_list (path [])) y)
      _id t ()

  (* let rec fold_values f acc t = match t with
   *   | Empty -> acc
   *   | Leaf v -> f acc v
   *   (\* | Cons (_, t') -> fold_values f acc t' *\)
   *   | Node map ->
   *     M.fold
   *       (fun _c t' acc -> fold_values f acc t')
   *       map acc
   * 
   * let iter_values f t = fold_values (fun () x -> f x) () t *)


  (** Merging operations *)
  let _mk = function Some x -> _leaf x | None -> empty
  
  let[@specialize] rec merge_with ~f ~left ~right t1 t2 = match t1, t2 with
    | Empty, Empty -> f None None
    | Empty, Node _ -> right t2
    | Node _, Empty -> left t1
    | Leaf v, Empty -> f (Some v) None
    | Empty, Leaf v -> f None (Some v)
    | Leaf v, Leaf v' -> f (Some v) (Some v')
    | Leaf _, Node _ | Node _, Leaf _ -> assert false
    (* | Cons (c1,t1'), Cons (c2,t2') ->
     *   if W.compare_char c1 c2 = 0
     *   then _cons c1 (merge_with ~f ~left ~right t1' t2')
     *   else _node2 c1 (left t1') c2 (right t2')
     * 
     * | Cons (c1, t1'), Node (value, map) ->
     *   begin try
     *       (\* collision *\)
     *       let t2' = M.find c1 map in
     *       let new_t = merge_with ~f ~left ~right t1' t2' in
     *       let map' = if is_empty new_t
     *         then M.remove c1 map
     *         else M.add c1 new_t map
     *       in
     *       _node value map'
     *     with Not_found ->
     *       (\* no collision *\)
     *       assert (not(is_empty t1'));
     *       let t1' = left t1' in
     *       let map' = if is_empty t1' then map else M.add c1 t1' map in
     *       Node (value, map')
     *   end
     * | Node (value, map), Cons (c2, t2') ->
     *   begin try
     *       (\* collision *\)
     *       let t1' = M.find c2 map in
     *       let new_t = merge_with ~f ~left ~right t1' t2' in
     *       let map' = if is_empty new_t
     *         then M.remove c2 map
     *         else M.add c2 new_t map
     *       in
     *       _node value map'
     *     with Not_found ->
     *       (\* no collision *\)
     *       assert (not(is_empty t2'));
     *       let t2' = left t2' in
     *       let map' = if is_empty t2' then map else M.add c2 t2' map in
     *       Node (value, map')
     *   end *)
    | Node map1, Node map2 ->
      (* let v = f v1 v2 in *)
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
      _node map'

  let keep x = x
  let drop _ = Empty
  
  let union l l' =
    let left = keep and right = keep and f a b = match a,b with
      | Some _, _ -> _mk a
      | None, _ -> _mk b
    in
    merge_with ~f ~left ~right l l'

  let inter l l' = 
    let left = drop and right = drop and f a b = match a,b with
      | Some _, Some _ -> _mk a
      | _ -> empty
    in
    merge_with ~f ~left ~right l l'

  let diff l l' =
    let left = keep and right = drop and f a b = match a,b with
      | Some _, None -> _mk a
      | _ -> empty
    in 
    merge_with ~f ~left ~right l l'

  let merge l =
    List.fold_left union Empty l

  (** Grafting/flatmap *)

  (* let map f t =
   *   let rec map_ = function
   *     | Empty -> Empty
   *     (\* | Cons (c, t') -> Cons (c, map_ t') *\)
   *     | Leaf x -> Leaf (f x)
   *     | Node map ->
   *       let map' = M.map map_ map
   *       in Node map'
   *   in map_ t *)

  let rec append t t0 = match t with
    | Empty -> Empty
    | Leaf _v -> t0
    (* | Cons (c, t') -> Cons (c, append t' t0) *)
    | Node map ->
      let map = M.map (fun t' -> append t' t0) map in
      Node map
  
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
    List.fold_left (fun acc v -> add v () acc) empty l

  let to_iter t k = iter (fun x () -> k x) t

  (** External API *)

  let return x = singleton x ()
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
    let compare_char = X.compare
    let to_iter a k = Array.iter k a
    let of_list = Array.of_list
  end)

module MakeList(X : ORDERED) = Make(struct
    type t = X.t list
    type char = X.t
    let append = List.append
    let compare_char = X.compare
    let to_iter a k = List.iter k a
    let of_list l = l
  end)

module String = Make(struct
    type t = string
    type nonrec char = char
    let append = (^)
    let compare_char = Char.compare
    let to_iter s k = String.iter k s
    let of_list l =
      let buf = Buffer.create (List.length l) in
      List.iter (fun c -> Buffer.add_char buf c) l;
      Buffer.contents buf
  end)
