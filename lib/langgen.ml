module type SIGMA = sig
  type t
  val sigma : t
end

module[@inline always] Make
    (Word : Word.S)
    (Segment : Segments.S with type elt = Word.t)
    (Sigma : SIGMA with type t = Segment.t)
= struct

  module Word = Word
  module Segment = Segment

  (** Spline of a language, a cascade-like thunk list with multiple nils. *)
  type node =
    | Nothing
    | Everything
    | Cons of Segment.t * lang
  and lang = unit -> node

  (** Utilities *)
  
  let segmentEpsilon = Segment.return Word.empty
  let nothing () = Nothing
  let everything () = Everything
  let (@:) h t () = Cons (h, t)

  let langEpsilon = segmentEpsilon @: nothing
  
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
      let l = CCVector.size v in
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

  let pp_item =
    Fmt.hbox @@ Fmt.iter ~sep:(Fmt.unit ", ") (CCFun.flip Segment.to_seq) Word.pp
  let pp fmt (l : lang) =
    let pp_sep = Fmt.unit "@." in
    let rec pp n fmt l = match l() with
      | Nothing -> ()
      | Everything ->
        let x = Sigma_star.get n and l' = everything in
        pp_next n fmt x l'
      | Cons (x,l') ->
        pp_next n fmt x l'
    and pp_next n fmt x l' =
        pp_sep fmt ();
        pp_item fmt x ;
        pp (n+1) fmt l'
    in
    match l() with
    | Nothing -> ()
    | Everything ->
      let x = Sigma_star.get 0 and l' = everything in
      pp_item fmt x; pp 1 fmt l'
    | Cons (x,l') ->
      pp_item fmt x; pp 1 fmt l'
  
  let of_list l =
    let rec aux n l () = match l with
      | [] -> nothing ()
      | _ ->
        let x, rest = CCList.partition (fun s -> Word.length s = n) l in
        Cons (Segment.of_list x, aux (n+1) rest)
    in aux 0 l
  
  (** Classic operations *)

  let rec union s1 s2 () = match s1(), s2() with
    | Everything, _ | _, Everything -> Everything
    | Nothing, x | x, Nothing -> x
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.union x1 x2, union next1 next2)
        
  let rec inter s1 s2 () = match s1(), s2() with
    | Everything, x | x, Everything -> x
    | Nothing, _ | _, Nothing -> Nothing
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.inter x1 x2, inter next1 next2)

  let rec difference_aux i s1 s2 () = match s1(), s2() with
    | Nothing, _ -> Nothing
    | _, Everything -> Nothing
    | x, Nothing -> x
    | Everything, Cons (x2, next2) ->
      let x1 = Sigma_star.get i and next1 = everything in
      Cons (Segment.diff x1 x2, difference_aux (i+1) next1 next2)
    | Cons (x1, next1), Cons (x2, next2) ->
      Cons (Segment.diff x1 x2, difference_aux (i+1) next1 next2)

  let difference = difference_aux 0
  
  (** Concatenation *)
    
  let[@inline] explode_head vec (seq, bound, nbSeg, indices) n =
    match bound with
    | Some _ -> nothing, bound, nbSeg, indices
    | None -> match seq() with
      | Nothing -> nothing, Some n, nbSeg, indices
      | Everything ->
        if n = CCVector.size vec then
          CCVector.push vec @@ Sigma_star.get n ;
        everything, None, nbSeg+1, n :: indices
      | Cons (segm, s) ->
        if n = CCVector.size vec then
          CCVector.push vec @@ Segment.memoize segm ;
        s, None, nbSeg+1, if Segment.is_empty segm then indices else n :: indices
  
  let[@inline] concat_subterms_of_length ~n ~f validIndicesA vecA vecB =
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

  let[@inline] combine_segments_left ~n indL vecL vecR =
    concat_subterms_of_length
      ~n ~f:(fun ~a b -> Segment.append a b) indL vecL vecR
  let[@inline] combine_segments_right ~n vecL indR vecR =
    concat_subterms_of_length
      ~n ~f:(fun ~a b -> Segment.append b a) indR vecR vecL
  
  let concatenate seqL0 seqR0 =
    let vecL = CCVector.make 0 Segment.empty in
    let vecR = CCVector.make 0 Segment.empty in
    let[@specialize] rec collect n descL descR () =
      let (_, boundL, nbSegL, indL) as descL = explode_head vecL descL n in
      let (_, boundR, nbSegR, indR) as descR = explode_head vecR descR n in
      match boundL, boundR with
      | Some bL, Some bR when n >= bL + bR - 1 -> Nothing
      | Some 0, _ | _, Some 0 -> Nothing
      | _ ->
        let head =
          if nbSegL <= nbSegR then
            combine_segments_left ~n indL vecL vecR
          else
            combine_segments_right ~n vecL indR vecR
        in
        let tail = collect (n+1) descL descR in
        Cons (head, tail)
    in
    collect 0 (seqL0, None, 0, []) (seqR0, None, 0, [])

  
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
    let rec collect n mapS seq validIndices () = match seq () with
      | Everything -> Everything
      | Nothing ->
        let segmS = star_subterms_of_length ~max:n validIndices mapS in
        let mapS = IMap.save n segmS mapS in
        Cons (segmS, collect (n+1) mapS seq validIndices)
      | Cons (segm, seq) ->
        let validIndices =
          if Segment.is_empty segm
          then validIndices
          else (n, segm) :: validIndices
        in
        let segmS = star_subterms_of_length ~max:n validIndices mapS in
        let mapS = IMap.save n segmS mapS in
        Cons (segmS, collect (n+1) mapS seq validIndices)
    in
    fun s () -> match s() with
      | Nothing -> Cons (segmentEpsilon, nothing)
      | Everything as v -> v
      | Cons (_, seq) ->
        match seq() with
        | Nothing -> Cons (segmentEpsilon, nothing)
        | seq ->
          let mS = IMap.singleton 0 segmentEpsilon in
          Cons (segmentEpsilon, collect 1 mS (fun () -> seq) [])

  let add_epsilon x = union x langEpsilon
  let dec k = max (k-1) 0
  let rec rep_with_acc acc i j lang =
    match i, j with
    | 0, None -> concatenate acc (star lang)
    | 0, Some 0 -> acc 
    | i, j ->
      let acc =
        concatenate (if i = 0 then add_epsilon lang else lang) acc
      in
      rep_with_acc acc (dec i) (CCOpt.map dec j) lang
  let rep i j lang = match i,j with
    | 0, None -> star lang
    | i, j ->
      let acc = lang in
      rep_with_acc acc (dec i) (CCOpt.map dec j) lang

  (** Others *)

  let charset l = match l with
    | [] -> nothing
    | l ->
      let segm1 = Segment.of_list @@ List.map Word.singleton l in
      Segment.empty @: segm1 @: nothing


  (****)

  let rec flatten_from n s k = match s () with
    | Nothing -> ()
    | Everything ->
      Sigma_star.iter n (fun s -> Segment.to_seq s k)
    | Cons (x, s) ->
      Segment.to_seq x k;
      flatten_from (n+1) s k

  let flatten s = flatten_from 0 s
  
  let rec gen : Word.char Regex.t -> lang = function
    | Set l -> charset l
    | One -> langEpsilon
    | Seq (r1, r2) -> concatenate (gen r1) (gen r2)
    | Or (r1, r2) -> union (gen r1) (gen r2)
    | And (r1, r2) -> inter (gen r1) (gen r2)
    | Not r -> difference everything (gen r)
    | Rep (i, j, r) -> rep i j (gen r)
  
end

(** [sample_infinite ~skip n seq] returns a sequence of on average [n] elements.
    [seq] is only consumed when needed. 

    This is done by skipping elements in [seq] (following a power law of average
    [skip]). *)
exception ExitSample
let sample_infinite ?(st=Random.State.make_self_init ()) ~skip n seq (k : _ -> unit) = 
  let i = ref (-1) in
  let draw st =
    let u = Random.State.float st 1. in
    1 + int_of_float (-. (float skip) *. log1p (-. u))
  in
  let continue st = Random.State.float st 1. > (1. /. float n) in
  let next = ref (draw st) in
  let f x =
    incr i ;
    if i < next then ()
    else begin
      k x ;
      if not (continue st) then raise ExitSample
      else
        next := !next + draw st
    end
  in
  try seq f with ExitSample -> ()

let arbitrary
    (type t) (type char)
    (module W : Word.S with type char = char and type t = t)
    (module S : Segments.S with type elt = W.t)
    ~compl
    ~pp
    ~samples
    (alphabet : W.char list) =
  let sigma = S.of_list @@ List.map W.singleton alphabet in
  let module Sigma = struct type t = S.t let sigma = sigma end in
  let module L = Make (W) (S) (Sigma) in
  let gen st =
    let open QCheck.Gen in
    let f re =
      L.gen re |> L.flatten
      |> sample_infinite ~st ~skip:5 samples
      |> Sequence.to_list
    in
    let re = Regex.gen ~compl (oneofl alphabet) st in
    let pos_examples = f re in
    let neg_examples = f (Regex.compl re) in
    (re, pos_examples, neg_examples)
  in
  let pp fmt (x, l , l') =
    Fmt.pf fmt "@[<2>re =@ %a@]@.@[<v2>pos =@ %a@]@.@[<v2>neg =@ %a@]"
      (Regex.pp pp) x   Fmt.(list W.pp) l   Fmt.(list W.pp) l'
  in
  let print = Fmt.to_to_string pp in
  let small (x, _, _) = Regex.size x in
  let shrink = QCheck.Shrink.(triple nil list list) in
  QCheck.make ~print ~small ~shrink gen

