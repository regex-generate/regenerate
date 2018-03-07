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
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val return : elt -> t
  val map : (elt -> elt) ->  t -> t
  val cross_product : (elt -> elt -> elt) -> t -> t -> t

  val union : t -> t -> t
  val inter : t -> t -> t
  val difference : t -> t -> t
  val merge : t OSeq.t -> t

  val of_list : elt list -> t
  val to_seq : t -> elt Sequence.t

  val memoize : t -> t
end

module Make (C : CHAR) (W : WORD with type char := C.t) (Segment : SEGMENT with type elt = W.t) = struct

  module M = struct
    include CCMap.Make(CCInt)
    let save k s m = add k (Segment.memoize s) m
  end
    
  type re
    = Zero
    | One
    | Atom of C.t
    | Seq of re * re
    | Or of re * re
    | And of re * re
    | Not of re
    | Star of re

  type word = W.t
  type rest = Nothing (* | Everything *)
  type lang = (Segment.t, rest) Iter.t

  (* let nothing = Iter.return Nothing *)
  let rec nothing () = Iter.Cons (Segment.empty, nothing)
  (* let everything = Iter.return Everything *)
  
  let segment0 = Segment.return W.empty

  
  
  (** Classic operations *)

  let rec union s1 s2 () = let open Iter in match s1(), s2() with
    (* | Ret Everything, _ | _, Ret Everything -> Ret Everything *)
    | Ret Nothing, x | x, Ret Nothing -> x
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.union x1 x2, union next1 next2)
        
  let rec inter s1 s2 () = let open Iter in match s1(), s2() with
    (* | Ret Everything, x | x, Ret Everything -> x *)
    | Ret Nothing, _ | _, Ret Nothing -> Ret Nothing
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.inter x1 x2, inter next1 next2)

  let rec difference s1 s2 () = let open Iter in match s1(), s2() with
    | Ret Nothing, _ -> Ret Nothing
    (* | _, Ret Everything -> Ret Nothing *)
    | x, Ret Nothing -> x
    (* | Ret Everything, Cons (x, next) -> (??) *)
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.difference x1 x2, difference next1 next2)
  
  (** Concatenation *)
    
  let concatenate =
    let subterms_of_length map1 map2 n =
      let combine_segments i =
        Segment.cross_product
          W.append
          (M.find i map1)
          (M.find (n - i) map2)
      in
      OSeq.(0 -- n)
      |> OSeq.map combine_segments
      |> Segment.merge
    in
    let rec do_merge n map1 map2 segm1 segm2 seq1 seq2 =
      let map1 = M.save n segm1 map1 in 
      let map2 = M.save n segm2 map2 in
      subterms_of_length map1 map2 n @: collect (n+1) map1 map2 seq1 seq2
    and collect n map1 map2 seq1 seq2 () = match seq1 (), seq2 () with
      | Iter.Ret Nothing, _x | _x, Iter.Ret Nothing -> assert false
      (* | Ret _, Ret _ -> Iter.Ret Everything
       * | Ret Everything, Cons (segm2, s2) -> (??)
       * | Cons (segm1, s1), Ret Everything -> (??) *)
      | Cons (segm1, seq1), Cons (segm2, seq2) ->
        do_merge n map1 map2 segm1 segm2 seq1 seq2 ()
    in 
    collect 0 M.empty M.empty
    
  (** Star *)
  
  (** xs \in pn => sum xs = n, xi \in xs => xi \in ns /\ xi > 0
      no repetitions
      ns is sorted decreasingly
  *)
  let rec valid_partitions ns n =
    if n = 0 then OSeq.return []
    else
      let ns = OSeq.drop_while (fun x -> x > n) ns in
      let open OSeq.Infix in
      ns >>= fun i ->
      valid_partitions ns (n - i) >|= fun s ->
      i :: s
      
  let star (seq : lang) =
    let words_of_partition map p =
      let aux ws i =
        Segment.cross_product W.append ws (M.find i map)
      in
      List.fold_left aux (Segment.return W.empty) p
    in
    let subterm_of_length indices map n =
      valid_partitions indices n
      |> OSeq.map (words_of_partition map)
      |> Segment.merge
    in
    let rec collect n indices map s () =
      (let indices, map, s = match s() with
        | Iter.Ret _ -> assert false
        | Cons (segm, s) ->
          (if Segment.is_empty segm then indices else n :: indices),
          (M.save n segm map), s
       in
       let new_segm_n = subterm_of_length (OSeq.of_list indices) map n in
       new_segm_n @: collect (n + 1) indices map s) ()
    in
    segment0 @: collect 1 [] M.empty (Iter.tail seq)


  let sigma_star sigma =
    let cons_all term_k' =
      sigma
      |> OSeq.map (fun c -> Segment.map (fun x -> W.cons c x) term_k')
      |> Segment.merge
    in
    let rec collect acc =
      acc @: fun () -> collect (cons_all acc) ()
    in
    collect segment0

  (****)

  let rec flatten s = match s () with
    | Iter.Ret Nothing -> Sequence.empty
    | Cons (x, s) -> (Sequence.append (Segment.to_seq x) @@ fun k -> flatten s k)
  
  let gen sigma =
    let sigma_star = Iter.memoize @@ sigma_star sigma in
    let rec g r : lang = match r with
      | Zero -> nothing
      | One -> segment0 @: nothing
      | Atom x -> Segment.empty @: (Segment.return @@ W.singleton x) @: nothing
      | Seq (r1, r2) -> concatenate (g r1) (g r2)
      | Or (r1, r2) -> union (g r1) (g r2)
      | And (r1, r2) -> inter (g r1) (g r2)
      | Not r -> difference sigma_star (g r)
      | Star r -> star (g r)
    in
    g

  (** Utils *)
  
  let pp ?(pp_sep=Format.pp_print_cut) pp_item fmt (l : lang) =
    let rec pp fmt l = match l() with
      | Iter.Ret Nothing -> ()
      | Cons (x,l') ->
        pp_sep fmt ();
        pp_item fmt x;
        pp fmt l'
    in
    match l() with
    | Iter.Ret Nothing -> ()
    | Cons (x,l') -> pp_item fmt x; pp fmt l'
  
  let of_list l =
    let rec aux n l () = match l with
      | [] -> nothing ()
      | _ ->
        let x, rest = CCList.partition (fun s -> W.length s = n) l in
        Cons (Segment.of_list x, aux (n+1) rest)
    in aux 0 l

end
