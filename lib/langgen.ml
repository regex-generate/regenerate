open Iter.Infix

module type WORD = sig
  type char
  type t
  val empty : t
  val singleton : char -> t
  val length : t -> int
  val append : t -> t -> t
  val cons : char -> t -> t
end

module type SIGMA = sig
  type t
  val sigma : t
end

module[@inline always] Make
    (W : WORD)
    (Segment : Segments.S with type elt = W.t)
    (Sigma : SIGMA with type t = Segment.t)
= struct

  module Word = W
  module Segment = Segment

  type rest = Nothing | Everything
  type lang = (Segment.t, rest) Iter.t


  (** Utilities *)
  
  let segmentEpsilon = Segment.return W.empty
  let nothing () = Iter.Ret Nothing
  let everything () = Iter.Ret Everything

  module IMap = struct
    include CCMap.Make(CCInt)
    let save k s m =
      if Segment.is_empty s then m
      else add k (Segment.memoize s) m
  end

  (** Precomputed full language. Used to replace "Everything" when need *)
  module Sigma_star = struct
    type t = (Segment.t, CCVector.rw) CCVector.t 

    let v = CCVector.return segmentEpsilon

    let rec complete_from_to i j =
      if i > j then CCVector.get v i
      else
        let s = Segment.append Sigma.sigma (CCVector.get v (i-1)) in
        CCVector.set v i s;
        complete_from_to (i+1) j
            
    let get i =
      let l = CCVector.size v in
      if i < l then CCVector.get v i
      else complete_from_to l i

    let rec iter n k =
      k (get n) ;
      iter (n+1) k
  end
  
  (** Classic operations *)

  let rec union s1 s2 () = let open Iter in match s1(), s2() with
    | Ret Everything, _ | _, Ret Everything -> Ret Everything
    | Ret Nothing, x | x, Ret Nothing -> x
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.union x1 x2, union next1 next2)
        
  let rec inter s1 s2 () = let open Iter in match s1(), s2() with
    | Ret Everything, x | x, Ret Everything -> x
    | Ret Nothing, _ | _, Ret Nothing -> Ret Nothing
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.inter x1 x2, inter next1 next2)

  let rec difference_aux i s1 s2 () =
    let open Iter in match s1(), s2() with
    | Ret Nothing, _ -> Ret Nothing
    | _, Ret Everything -> Ret Nothing
    | x, Ret Nothing -> x
    | Ret Everything, Cons (x2, next2) ->
      Cons (Segment.diff (Sigma_star.get i) x2,
            difference_aux (i+1) s1 next2)
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.diff x1 x2, difference_aux (i+1) next1 next2)

  let difference = difference_aux 0
  
  (** Concatenation *)
    
  let concatenate =
    let subterms_of_length map1 map2 n =
      let combine_segments i =
        if IMap.mem i map1 && IMap.mem (n - i) map2 
        then Segment.append (IMap.find i map1) (IMap.find (n - i) map2)
        else Segment.empty
      in
      CCList.(0 -- n)
      |> List.rev_map combine_segments
      |> Segment.merge
    in
    let[@inline] rec do_merge n map1 map2 segm1 segm2 seq1 seq2 =
      let map1 = IMap.save n segm1 map1 in 
      let map2 = IMap.save n segm2 map2 in
      Iter.Cons (subterms_of_length map1 map2 n,
                 collect (n+1) map1 map2 seq1 seq2)
    and collect n map1 map2 seq1 seq2 () =
      let open Iter in match seq1 (), seq2 () with
      | Ret _, Ret _ -> (??)
      | Ret Nothing, Cons (segm2, seq2) ->
        let map2 = IMap.save n segm2 map2 in
        Cons (subterms_of_length map1 map2 n,
              collect (n+1) map1 map2 seq1 seq2)
      | Cons (segm1, seq1), Ret Nothing -> 
        let map1 = IMap.save n segm1 map1 in
        Cons (subterms_of_length map1 map2 n,
              collect (n+1) map1 map2 seq1 seq2)
      | Ret Everything, Cons (segm2, seq2) ->
        let segm1 = Sigma_star.get n in
        do_merge n map1 map2 segm1 segm2 seq1 seq2
      | Cons (segm1, seq1), Ret Everything ->
        let segm2 = Sigma_star.get n in
        do_merge n map1 map2 segm1 segm2 seq1 seq2
      | Cons (segm1, seq1), Cons (segm2, seq2) ->
        do_merge n map1 map2 segm1 segm2 seq1 seq2
    in 
    collect 0 IMap.empty IMap.empty
    
  (** Star *)
   
  let star =
    let subterms_of_length ~stop validIndices mapS =
      let combine_segments (i, segm) =
        match IMap.get (stop - i) mapS with
        | None -> Segment.empty
        | Some s -> Segment.append segm s
      in
      validIndices
      |> List.rev_map combine_segments
      |> Segment.merge
    in
    let rec do_star n mapS seq validIndices = 
      let segmS = subterms_of_length ~stop:n validIndices mapS in
      let mapS = IMap.save n segmS mapS in
      Iter.Cons (segmS, collect (n+1) mapS seq validIndices)
    and collect n mapS seq validIndices () = match seq () with
      | Iter.Ret Everything as v -> v
      | Iter.Ret Nothing ->
        do_star n mapS nothing validIndices
      | Cons (segm, seq) ->
        let validIndices =
          if Segment.is_empty segm then validIndices else (n, segm) :: validIndices
        in
        do_star n mapS seq validIndices
    in
    fun s () -> match s() with
      | Iter.Ret Nothing -> Iter.Cons (segmentEpsilon, nothing)
      | Iter.Ret Everything as v -> v
      | Cons (_, seq) ->
        let mS = IMap.singleton 0 segmentEpsilon in
        Iter.Cons (segmentEpsilon, collect 1 mS seq [])

  let add_epsilon x () = match x () with
    | Iter.Ret Nothing -> Iter.Cons (segmentEpsilon, nothing)
    | Iter.Ret Everything as x -> x
    | Cons (_, t) -> Iter.Cons (segmentEpsilon, t)
  
  let rec rep i j re = match (i, j, re) with
    | 0, None, re -> star re
    | i, j, re ->
      let dec i = max (i-1) 0 in
      (if i = 0 then add_epsilon else fun x -> x) @@
      concatenate re @@ rep (dec i) (CCOpt.map dec j) re
    
  


  (****)

  let rec flatten n s k = match s () with
    | Iter.Ret Nothing -> Sequence.empty
    | Iter.Ret Everything ->
      Sigma_star.iter n (fun s -> Segment.to_seq s k)
    | Cons (x, s) ->
      Segment.to_seq x k;
      flatten (n+1) s k 
  
  let gen ~sigma =
    let rec g (r : _ Regex.t) : lang = match r with
      | Zero -> nothing
      | One -> segmentEpsilon @: nothing
      | Atom x -> Segment.empty @: (Segment.return @@ W.singleton x) @: nothing
      | Seq (r1, r2) -> concatenate (g r1) (g r2)
      | Or (r1, r2) -> union (g r1) (g r2)
      | And (r1, r2) -> inter (g r1) (g r2)
      | Not r -> difference everything (g r)
      | Rep (i, j, r) -> rep i j (g r)
    in
    g

  (** Utils *)

  let pp_item pp_word =
    Fmt.hbox @@ Fmt.iter ~sep:(Fmt.unit ", ") (CCFun.flip Segment.to_seq) pp_word
  let pp ?(pp_sep=Format.pp_print_cut) pp_word fmt (l : lang) =
    let rec pp n fmt l = match l() with
      | Iter.Ret Nothing -> ()
      | Iter.Ret Everything ->
        let x = Sigma_star.get n and l' = everything in
        pp_next n fmt x l'
      | Cons (x,l') ->
        pp_next n fmt x l'
    and pp_next n fmt x l' =
        pp_sep fmt ();
        pp_item pp_word fmt x ;
        pp (n+1) fmt l'
    in
    match l() with
    | Iter.Ret Nothing -> ()
    | Iter.Ret Everything ->
      let x = Sigma_star.get 0 and l' = everything in
      pp_item pp_word fmt x; pp 1 fmt l'
    | Cons (x,l') ->
      pp_item pp_word fmt x; pp 1 fmt l'
  
  let of_list l =
    let rec aux n l () = match l with
      | [] -> nothing ()
      | _ ->
        let x, rest = CCList.partition (fun s -> W.length s = n) l in
        Cons (Segment.of_list x, aux (n+1) rest)
    in aux 0 l

  let print pp_word : lang -> unit =
    pp ~pp_sep:(Fmt.unit "@.") pp_word Fmt.stdout
  
end
