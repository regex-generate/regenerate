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

    let v : t = CCVector.make 1 segmentEpsilon

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
    Fmt.parens @@
    Fmt.hbox @@
    Fmt.iter ~sep:(Fmt.unit ", ") (CCFun.flip Segment.to_seq) Word.pp
  let pp fmt (l : lang) =
    let pp_sep = Fmt.unit "@." in
    let rec pp fmt l = 
      pp_sep fmt ();
      match l() with
      | Nothing -> Fmt.pf fmt "(Nothing)"
      | Everything -> Fmt.pf fmt "(Everything)"
      | Cons (x,l') ->
        pp_item fmt x ; pp fmt l'
    in
    pp fmt l
  
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
  let compl = difference everything
  
  (** Concatenation *)

  (** Invariants for each language: 
      - [nbSeg = List.length indices]
      - After [explode_head], [CCVector.size vec >= n]
      - if [bound = Some n] then [n >= nbSeg] and [seqₙ = Nothing].
  *)
    
  let[@inline] explode_head vec (seq, bound, nbSeg, indices) n =
    match bound with
    | Some _ -> nothing, bound, nbSeg, indices
    | None -> match seq() with
      | Nothing -> nothing, Some n, nbSeg, indices
      | Everything ->
        begin if n = CCVector.size vec then
            CCVector.push vec @@ Sigma_star.get n
        end ;
        everything, None, nbSeg+1, n :: indices
      | Cons (segm, s) ->
        begin if n = CCVector.size vec then
            CCVector.push vec @@ Segment.memoize segm
        end;
        let b = Segment.is_empty segm in
        let indices' = if b then indices else n :: indices in
        let nbSeg' = if b then nbSeg else nbSeg+1 in
        s, None, nbSeg', indices'
  
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
      | Some _, _ when nbSegL = 0 -> Nothing
      | _, Some _ when nbSegR = 0 -> Nothing
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

  let add_epsilonX i x = if i = 0 then union x langEpsilon else x
  let dec k = max (k-1) 0
  let rec rep_with_acc acc i j lang =
    match i, j with
    | 0, None -> concatenate acc (star lang)
    | 0, Some 0 -> acc 
    | i, j ->
      let acc =
        concatenate (add_epsilonX i lang) acc
      in
      rep_with_acc acc (dec i) (CCOpt.map dec j) lang
  let rep i j lang = match i,j with
    | 0, None -> star lang
    | 0, Some 0 -> langEpsilon
    | i, j ->
      let acc = add_epsilonX i lang in
      rep_with_acc acc (dec i) (CCOpt.map dec j) lang

  (** Others *)

  let charset b l = match l with
    | [] when b -> nothing
    | l ->
      let set = Segment.of_list @@ List.map Word.singleton l in
      let segm1 =
        if b then set else Segment.diff Sigma.sigma set
      in
      Segment.empty @: segm1 @: nothing


  (****)

  let rec gen : Word.char Regex.t -> lang = function
    | Set (b, l) -> charset b l
    | One -> langEpsilon
    | Seq (r1, r2) -> concatenate (gen r1) (gen r2)
    | Or (r1, r2) -> union (gen r1) (gen r2)
    | And (r1, r2) -> inter (gen r1) (gen r2)
    | Not r -> compl (gen r)
    | Rep (i, j, r) -> rep i j (gen r)
  
  (** Exporting *)

  let rec flatten_from n s k = match s () with
    | Nothing -> ()
    | Everything ->
      Sigma_star.iter n (fun s -> Segment.to_seq s k)
    | Cons (x, s) ->
      Segment.to_seq x k;
      flatten_from (n+1) s k

  let flatten s = flatten_from 0 s

  type res = Done | Finite | GaveUp
  
  (** [sample ~skip ~n lang] returns a sequence of on average [n] elements.
      [lang] is only consumed when needed. 

      We sample one element every [k], where [k] follows a power law of
      average [skip]. Furthermore, if we consume more than [sqrt k] empty segments,
      we assume that the rest of the segments will be infinitely empty and
      stop. 
      
      If [firsts] is provided, we always output the [firsts] first elements.
  *)
  exception ExitSample
  let sample ?(st=Random.State.make_self_init ()) ?n ?(firsts=0) ~skip lang (k : _ -> unit) = 
    let i = ref (-1) in

    (* The number of element to always take at the beginning. *)
    let rem_firsts = ref firsts in 

    (* Draw the amount of element we skip and store the next element to take. *)
    let draw_skip st =
      let f = !rem_firsts in
      if f > 0
      then (decr rem_firsts ; 0)
      else
        let u = Random.State.float st 1. in
        1 + int_of_float (-. (float skip) *. log1p (-. u))
    in
    let next = ref (draw_skip st) in

    (* Our chance to continue after each sample. *)
    let continue st =
      match n with
      | Some n -> Random.State.float st 1. > (1. /. float n)
      | None -> true
    in

    (* Our "empty segment" budget. If we exceed this, we stop. *)
    let budget_of_skip n = 2 + (int_of_float @@ sqrt @@ float n) in    
    let budget = ref (budget_of_skip !next) in
    
    let onSegm x =
      incr i ;
      if i < next then ()
      else begin
        k x ;
        if not (continue st) then raise ExitSample
        else begin
          let newskip = draw_skip st in
          next := !next + newskip ;
          budget := budget_of_skip newskip ;
        end
      end
    in
    let rec walk_lang n seq =
      let i0 = !i in
      let next segm seq =
        match Segment.to_seq segm onSegm with
        | exception ExitSample -> Done
        | () ->
          let i1 = !i in
          if i0 <> i1 then walk_lang (n+1) seq
          else if !budget < 0 then GaveUp
          else begin
            decr budget ;
            walk_lang (n+1) seq
          end
      in
      match seq () with
      | Nothing -> Finite
      | Everything ->
        let segm = Sigma_star.get n in
        next segm everything
      | Cons (segm, seq) ->
        next segm seq
    in
    walk_lang 0 lang

end

let arbitrary
    (type t) (type char)
    (module W : Word.S with type char = char and type t = t)
    (module S : Segments.S with type elt = W.t)
    ?(skip=8)
    ~compl
    ~pp
    ~samples
    (alphabet : W.char list) =
  let sigma = S.of_list @@ List.map W.singleton alphabet in
  let module Sigma = struct type t = S.t let sigma = sigma end in
  let module L = Make (W) (S) (Sigma) in
  let gen st =
    let open QCheck.Gen in
    let re = Regex.gen ~compl (oneofl alphabet) st in
    Fmt.epr "Regex: %a@." (Regex.pp pp) re;
    let print_samples s l =
      Fmt.epr "@[<2>%s:@ %a@]@." s Fmt.(list ~sep:(unit ",@ ") W.pp) l
    in
    let lang = L.gen re in
    let f s l =
      l
      |> L.sample ~st ~skip ~n:samples
      |> CCFun.(%) ignore
      |> Iter.to_list
      |> CCFun.tap (print_samples s)
    in
    let pos_examples = f "Pos" lang in
    let neg_examples = f "Neg" @@ L.compl lang in
    (re, pos_examples, neg_examples)
  in
  let pp fmt (x, l , l') =
    Fmt.pf fmt "@[<2>Regex:@ %a@]@.@[<v2>Pos:@ %a@]@.@[<v2>Neg:@ %a@]"
      (Regex.pp pp) x   Fmt.(list W.pp) l   Fmt.(list W.pp) l'
  in
  let print = Fmt.to_to_string pp in
  let small (x, _, _) = Regex.size x in
  let shrink = QCheck.Shrink.(triple nil list list) in
  QCheck.make ~print ~small ~shrink gen

