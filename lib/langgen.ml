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
  
module[@inline always] Make
    (W : WORD)
    (Segment : Segments.S with type elt = W.t)
= struct

  module Segment = Segment
  module M = struct
    include CCMap.Make(CCInt)
    let save k s m =
      if Segment.is_empty s then m
      else add k (Segment.memoize s) m
  end
    
  type word = W.t
  type rest = Nothing (* | Everything *)
  type lang = (Segment.t, rest) Iter.t

  (* let nothing = Iter.return Nothing *)
  let rec nothing () = Iter.Cons (Segment.empty, nothing)
  (* let everything = Iter.return Everything *)
  
  let segmentEpsilon = Segment.return W.empty
  
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
      Cons (Segment.diff x1 x2, difference next1 next2)
  
  (** Concatenation *)
    
  let concatenate =
    let subterms_of_length map1 map2 n =
      let combine_segments i =
        if M.mem i map1 && M.mem (n - i) map2 
        then Segment.append (M.find i map1) (M.find (n - i) map2)
        else Segment.empty
      in
      CCList.(0 -- n)
      |> List.rev_map combine_segments
      |> Segment.merge
    in
    let rec do_merge n map1 map2 segm1 segm2 seq1 seq2 =
      let map1 = M.save n segm1 map1 in 
      let map2 = M.save n segm2 map2 in
      Iter.Cons (subterms_of_length map1 map2 n, collect (n+1) map1 map2 seq1 seq2)
    and collect n map1 map2 seq1 seq2 () = match seq1 (), seq2 () with
      | Iter.Ret Nothing, _x | _x, Iter.Ret Nothing -> assert false
      (* | Ret _, Ret _ -> Iter.Ret Everything
       * | Ret Everything, Cons (segm2, s2) -> (??)
       * | Cons (segm1, s1), Ret Everything -> (??) *)
      | Cons (segm1, seq1), Cons (segm2, seq2) ->
        do_merge n map1 map2 segm1 segm2 seq1 seq2
    in 
    collect 0 M.empty M.empty
    
  (** Star *)
   
  let star =
    let subterms_of_length ~stop validIndices mapS =
      let combine_segments (i, segm) =
        match M.get (stop - i) mapS with
        | None -> Segment.empty
        | Some s -> Segment.append segm s
      in
      validIndices
      |> List.rev_map combine_segments
      |> Segment.merge
    in
    let rec collect n mapS seq validIndices () = match seq () with
      | Iter.Ret Nothing -> assert false
      | Cons (segm, seq) ->
        let validIndices =
          if Segment.is_empty segm
          then validIndices
          else (n, segm) :: validIndices
        in
        let segmS = subterms_of_length ~stop:n validIndices mapS in
        let mapS = M.save n segmS mapS in
        Iter.Cons (segmS, collect (n+1) mapS seq validIndices)
    in
    fun s () -> match s() with
    | Iter.Ret Nothing -> assert false
    | Cons (_, seq) ->
      let mS = M.singleton 0 segmentEpsilon in
      Iter.Cons (segmentEpsilon, collect 1 mS seq [])

  
  let sigma_star sigma =
    let f term_k' =
      Segment.append term_k' sigma
    in
    let rec collect acc () =
      Iter.Cons (acc, collect (f acc))
    in
    collect segmentEpsilon

  (****)

  let rec flatten s = match s () with
    | Iter.Ret Nothing -> Sequence.empty
    | Cons (x, s) -> (Sequence.append (Segment.to_seq x) @@ fun k -> flatten s k)
  
  let gen ~sigma =
    let sigma_star = Iter.memoize @@ sigma_star sigma in
    let rec g (r : _ Regex.t) : lang = match r with
      | Zero -> nothing
      | One -> segmentEpsilon @: nothing
      | Atom x -> Segment.empty @: (Segment.return @@ W.singleton x) @: nothing
      | Seq (r1, r2) -> concatenate (g r1) (g r2)
      | Or (r1, r2) -> union (g r1) (g r2)
      | And (r1, r2) -> inter (g r1) (g r2)
      | Not r -> difference sigma_star (g r)
      | Star r -> star (g r)
    in
    g

  (** Utils *)

  let pp_item pp_word =
    Fmt.hbox @@ Fmt.iter ~sep:(Fmt.unit ", ") (CCFun.flip Segment.to_seq) pp_word
  let pp ?(pp_sep=Format.pp_print_cut) pp_word fmt (l : lang) =
    let rec pp fmt l = match l() with
      | Iter.Ret Nothing -> ()
      | Cons (x,l') ->
        pp_sep fmt ();
        pp_item pp_word fmt x;
        pp fmt l'
    in
    match l() with
    | Iter.Ret Nothing -> ()
    | Cons (x,l') -> pp_item pp_word fmt x; pp fmt l'
  
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
