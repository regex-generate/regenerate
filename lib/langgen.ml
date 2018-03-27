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
  let nothing_ = Iter.Ret Nothing
  let nothing () = nothing_
  let everything_ = Iter.Ret Everything
  let everything () = everything_

  module IMap = struct
    include CCMap.Make(CCInt)
    let save k s m =
      if Segment.is_empty s then m
      else add k (Segment.memoize s) m
  end

  (** Precomputed full language. Used to replace "Everything" when need *)
  module Sigma_star = struct
    type t = (Segment.t, CCVector.rw) CCVector.t 

    let v = CCVector.make 1 segmentEpsilon

    let rec complete_from_to i j =
      if i > j then ()
      else
        let s = Segment.append Sigma.sigma (CCVector.get v (i-1)) in
        CCVector.push v @@ Segment.memoize s;
        complete_from_to (i+1) j
            
    let get i =
      assert (i >= 0);
      (* Fmt.epr "Sigma_star.%i@." i ; *)
      let l = CCVector.size v in
      (* Fmt.epr "Sigma_star.size = %i@." l ; *)
      if i < l then CCVector.get v i
      else begin
        CCVector.ensure_with ~init:Segment.empty v (i+1);
        complete_from_to l i ;
        CCVector.get v i
      end

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
    | Ret Nothing, _ | _, Ret Nothing -> nothing_
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.inter x1 x2, inter next1 next2)

  let rec difference_aux i s1 s2 () =
    let open Iter in match s1(), s2() with
    | Ret Nothing, _ -> nothing_
    | _, Ret Everything -> nothing_
    | x, Ret Nothing -> x
    | Ret Everything, Cons (x2, next2) ->
      Cons (Segment.diff (Sigma_star.get i) x2,
            difference_aux (i+1) s1 next2)
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.diff x1 x2, difference_aux (i+1) next1 next2)

  let difference = difference_aux 0
  
  (** Concatenation *)
    
  let[@inline] explode_head seq vec bound indices n =
    match bound with
    | Some _ -> nothing, bound, indices
    | None -> match seq() with
      | Iter.Ret Nothing -> nothing, Some n, indices
      | Ret Everything ->
        let segm = Sigma_star.get n in
        CCVector.push vec segm ;
        everything, None, (n :: indices)
      | Cons (segm, s) ->
        CCVector.push vec @@ Segment.memoize segm ;
        s, None, (if Segment.is_empty segm then indices else n :: indices)
  
  let concat_subterms_of_length ~n ~f validIndicesA vecA vecB =
    let rec combine_segments acc = function
      | [] -> acc
      | i :: l ->
        (* indices are in decreasing order, we can bail early. *)
        if n - i >= CCVector.size vecB then acc
        else
          combine_segments
            (f ~a:(CCVector.get vecA i) (CCVector.get vecB (n - i)) :: acc)
            l
    in
    validIndicesA
    |> combine_segments []
    |> Segment.merge

  let combine_segments_left ~n indL vecL vecR =
    concat_subterms_of_length
      ~n ~f:(fun ~a b -> Segment.append a b) indL vecL vecR
  let combine_segments_right ~n vecL indR vecR =
    concat_subterms_of_length
      ~n ~f:(fun ~a b -> Segment.append b a) indR vecR vecL
  
  let concatenate seqL0 seqR0 =
    let vecL = CCVector.make 0 Segment.empty in
    let vecR = CCVector.make 0 Segment.empty in
    let rec collect_right n seqL seqR boundL boundR indL indR () =
      let seqL, boundL, indL = explode_head seqL vecL boundL indL n in
      let seqR, boundR, indR = explode_head seqR vecR boundR indR n in
      let bound = CCOpt.map2 (+) boundL boundR in
      match bound with
      | Some b when n >= b - 1 -> nothing_
      | _ ->
        let head = combine_segments_right ~n vecL indR vecR in
        match boundR with
        | Some _ ->
          Iter.Cons (head, collect_right (n+1) seqL seqR boundL boundR indL indR)
        | None ->
          Iter.Cons (head, collect_left (n+1) seqL seqR boundL boundR indL indR)
    and collect_left n seqL seqR boundL boundR indL indR () =
      let seqL, boundL, indL = explode_head seqL vecL boundL indL n in
      let seqR, boundR, indR = explode_head seqR vecR boundR indR n in
      let bound = CCOpt.map2 (+) boundL boundR in
      match bound with
      | Some b when n >= b - 1 -> nothing_
      | _ ->
        let head = combine_segments_left ~n indL vecL vecR in
        let tail = collect_left (n+1) seqL seqR boundL boundR indL indR in
        Iter.Cons (head, tail)
    in
    collect_right 0 seqL0 seqR0 None None [] []

  
  (** Star *)
   
  let star_subterms_of_length ~max validIndices mapS =
    let combine_segments (i, segm) =
      match IMap.get (max - i) mapS with
      | None -> Segment.empty
      | Some s -> Segment.append segm s
    in
    validIndices
    |> List.rev_map combine_segments
    |> Segment.merge

  let star =
    let rec do_star n mapS seq validIndices = 
      let segmS = star_subterms_of_length ~max:n validIndices mapS in
      let mapS = IMap.save n segmS mapS in
      Iter.Cons (segmS, collect (n+1) mapS seq validIndices)
    and collect n mapS seq validIndices () = match seq () with
      | Iter.Ret Everything as v -> v
      | Iter.Ret Nothing ->
        do_star n mapS nothing validIndices
      | Cons (segm, seq) ->
        let validIndices =
          if Segment.is_empty segm
          then validIndices
          else (n, segm) :: validIndices
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

  let rec flatten_from n s k = match s () with
    | Iter.Ret Nothing -> ()
    | Iter.Ret Everything ->
      Sigma_star.iter n (fun s -> Segment.to_seq s k)
    | Cons (x, s) ->
      Segment.to_seq x k;
      flatten_from (n+1) s k

  let flatten s = flatten_from 0 s
  
  let rec gen : W.char Regex.t -> lang = function
    | Zero -> nothing
    | One -> segmentEpsilon @: nothing
    | Atom x -> Segment.empty @: (Segment.return @@ W.singleton x) @: nothing
    | Seq (r1, r2) -> concatenate (gen r1) (gen r2)
    | Or (r1, r2) -> union (gen r1) (gen r2)
    | And (r1, r2) -> inter (gen r1) (gen r2)
    | Not r -> difference everything (gen r)
    | Rep (i, j, r) -> rep i j (gen r)
  
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
