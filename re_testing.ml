open Iter.Infix

module type CHAR = Set.OrderedType
module type WORD = sig
  type char
  type t
  val empty : t
  val singleton : char -> t
  val length : t -> int
  val append : t -> t -> t
  val cons : char -> t -> t
  val compare : t -> t -> int
end

module type SEGMENT = sig
  type +'a t
  val empty : _ t
  val return : 'a -> 'a t
  val map : ('a -> 'b) ->  'a t -> 'b t
  val fold : ('a -> 'b -> 'a) ->  'a -> 'b t -> 'a
  val range : int -> int -> int t
  val cross_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val merge : ('a -> 'a -> int) -> 'a t t -> 'a t
  val exists : ('a -> bool) -> 'a t -> bool
  val partitions : int -> int t t
  val union : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val inter : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val difference : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
end

module Make (C : CHAR) (W : WORD with type char := C.t) (Segment : SEGMENT) = struct

  module IntMap = CCMap.Make(CCInt)
  
  type word = W.t
  
  type re
    = Zero
    | One
    | Atom of C.t
    | Seq of re * re
    | Or of re * re
    | And of re * re
    | Not of re
    | Star of re

  type rest = Nothing | Everything
  type lang = (W.t Segment.t, rest) Iter.t
      
  let segment0 = Segment.return W.empty
  
  (* type drop = Drop | Keep
   * let dropX s s' = function Drop -> s' | Keep -> s
   * let rec merge_with l r f s1 s2 () = match s1 (), s2 () with
   *   | Iter.Nil, Iter.Nil -> Iter.Nil
   *   | Cons _, Nil -> l s1 ()
   *   | Nil, Cons _ -> r s2 ()
   *   | Cons (x1, s1'), Cons (x2, s2') ->
   *     let d1, d2, res = f x1 x2 in
   *     let k = merge_with l r f (dropX s1 s1' d1) (dropX s2 s2' d2) in
   *     match res with
   *     | Some x -> Cons (x, k)
   *     | None -> k ()
   * let keep x = x
   * let drop _ = Iter.empty
   * 
   * let union =
   *   let f x y =
   *     let i = W.compare x y in
   *     if i = 0 then      Drop, Drop, Some x
   *     else if i < 0 then Drop, Keep, Some x
   *     else               Keep, Drop, Some x
   *   in
   *   merge_with keep keep f
   *     
   * let intersection =
   *   let f x y =
   *     let i = W.compare x y in
   *     if i = 0 then      Drop, Drop, Some x
   *     else if i < 0 then Drop, Keep, None
   *     else               Keep, Drop, None
   *   in
   *   merge_with drop drop f
   * 
   * let difference =
   *   let f x y =
   *     let i = W.compare x y in
   *     if i = 0 then      Drop, Drop, None
   *     else if i < 0 then Drop, Keep, Some x
   *     else               Keep, Drop, None
   *   in
   *   merge_with keep drop f *)

  (* let union_n = Iter.sorted_merge_n ~cmp:W.compare *)

  let union = Iter.map2 @@ Segment.union W.compare
  let inter = Iter.map2 @@ Segment.inter W.compare
  let difference = Iter.map2 @@ Segment.difference W.compare
  
  (** Concatenation *)
  
  (* let segmentize_infite =
   *   let rec aux i x () =
   *     let p w = W.length w = i in
   *     let segment_i = Iter.take_while p x in
   *     let nexts = Iter.drop_while p x in
   *     (segment_i @: aux (i+1) nexts) ()
   *   in
   *   aux 0
   *     
   * let segmentize =
   *   let rec aux i x () = match x () with
   *     | Iter.Nil -> Iter.empty ()
   *     | _ ->
   *       let p w = W.length w = i in
   *       let segment_i = Iter.take_while p x in
   *       let nexts = Iter.drop_while p x in
   *       (segment_i @: aux (i+1) nexts) ()
   *   in
   *   aux 0 *)
  
  let concatenate =
    let subterms_of_length map1 map2 n =
      let combine_segments i =
        Segment.cross_product
          W.append
          (IntMap.find (n - i) map1)
          (IntMap.find i map2)
      in
      Segment.range 0 n
      |> Segment.map combine_segments
      |> Segment.merge W.compare
    in
    let rec do_merge n map1 map2 segm1 segm2 seq1 seq2 =
      let map1 = IntMap.add n segm1 map1 in 
      let map2 = IntMap.add n segm2 map2 in
      subterms_of_length map1 map2 n @: collect (n+1) map1 map2 seq1 seq2
    and collect n map1 map2 seq1 seq2 () = match seq1 (), seq2 () with
      | Iter.Nil Nothing, x | x, Iter.Nil Nothing-> x
      | Iter.Nil _, Iter.Nil _ -> Iter.Nil Everything
      | Nil Everything, Cons (segm2, s2) -> (??)
      | Cons (segm1, s1), Nil Everything -> (??)
      | Cons (segm1, seq1), Cons (segm2, seq2) ->
        do_merge  n map1 map2 segm1 segm2 seq1 seq2 ()
    in 
    collect 0 IntMap.empty IntMap.empty
    
  (** Star *)
  let is_infinite = Iter.exists (Segment.exists (fun w -> W.length w > 0))
  
  (* let rec iter_partitions n =
   *   if n = 0 then Segment.return Iter.empty
   *   else
   *     let open Segment.Infix in
   *     1 -- n >>= fun i ->
   *     iter_partitions (n - i) >|= fun s ->
   *     i @: s *)
        
  let star (seq : lang) =
    let infinite = is_infinite seq in
    let rec words_of_partition p =
      let aux s i = 
        Segment.cross_product W.append s (Iter.nth seq i)
      in
      Segment.fold aux (Segment.return W.empty) p
    in
    let subterm_of_length n =
      Segment.partitions n
      |> Segment.map words_of_partition
      |> Segment.merge W.compare
    in
    let rec collect n () =
      (subterm_of_length n @: collect (n + 1)) ()
    in
    if infinite
    then segment0 @: collect 1
    else Iter.return segment0
  
  let sigma_star sigma =
    let cons_all term_k' =
      Segment.cross_product W.cons sigma term_k'
    in
    let rec collect acc =
      acc @: fun () -> collect (cons_all acc) ()
    in
    collect segment0

  (****)
  
  let rec gen sigma r : lang = match r with
    | Zero -> Iter.empty 
    | One -> Iter.return segment0
    | Atom x -> Iter.return @@ Segment.return @@ W.singleton x
    | Seq (r1, r2) -> concatenate (gen sigma r1) (gen sigma r2)
    | Or (r1, r2) -> union (gen sigma r1) (gen sigma r2)
    | And (r1, r2) -> inter (gen sigma r1) (gen sigma r2)
    | Not r -> difference (sigma_star sigma) (gen sigma r)
    | Star r -> star (gen sigma r)

end


module WList (C : CHAR) = struct
  type t = C.t list
  let empty = []
  let singleton x = [x]
  let length = List.length
  let append = List.append
  let cons = List.cons

  (* Length lexicographic order *)
  let compare l1 l2 =
    if l1 == l2 then 0
    else CCOrd.(
        int (length l1) (length l2)
        <?> (list C.compare, l1, l2))
  let pp = CCFormat.(list ~sep:(fun _ () -> ()) char)
end

module WString = struct
  include CCString
  let empty = ""
  let singleton = make 1
  let cons c s = singleton c ^ s
  let append = (^)
  let compare l1 l2 = 
    if l1 == l2 then 0
    else CCOrd.(
        int (length l1) (length l2)
        <?> (string, l1, l2))
  let pp = CCFormat.string
end
