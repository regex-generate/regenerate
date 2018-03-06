open Iter.Infix

module Iter = Iter

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

module Make (C : CHAR) (W : WORD with type char := C.t) = struct

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

  type lang = W.t Iter.t

  let rec iter_partitions n =
    if n = 0 then Iter.return Iter.empty
    else
      1 -- n >>= fun i ->
      iter_partitions (n - i) >|= fun s ->
      i @: s
  
  type drop = Drop | Keep
  let dropX s s' = function Drop -> s' | Keep -> s
  let rec merge_with l r f s1 s2 () = match s1 (), s2 () with
    | Iter.Nil, Iter.Nil -> Iter.Nil
    | Cons _, Nil -> l s1 ()
    | Nil, Cons _ -> r s2 ()
    | Cons (x1, s1'), Cons (x2, s2') ->
      let d1, d2, res = f x1 x2 in
      let k = merge_with l r f (dropX s1 s1' d1) (dropX s2 s2' d2) in
      match res with
      | Some x -> Cons (x, k)
      | None -> k ()
  let id x = x
  let drop _ = Iter.empty
  
  let union =
    let f x y =
      let i = W.compare x y in
      if i = 0 then      Drop, Drop, Some x
      else if i < 0 then Drop, Keep, Some x
      else               Keep, Drop, Some x
    in
    merge_with id id f
      
  let intersection =
    let f x y =
      let i = W.compare x y in
      if i = 0 then      Drop, Drop, Some x
      else if i < 0 then Drop, Keep, None
      else               Keep, Drop, None
    in
    merge_with drop drop f

  let difference =
    let f x y =
      let i = W.compare x y in
      if i = 0 then      Drop, Drop, None
      else if i < 0 then Drop, Keep, Some x
      else               Keep, Drop, None
    in
    merge_with id drop f

  let union_n = Iter.fold_left union Iter.empty

  (** Concatenation *)
  
  let segmentize_infite =
    let rec aux i x () =
      let p w = W.length w = i in
      let segment_i = Iter.take_while p x in
      let nexts = Iter.drop_while p x in
      (segment_i @: aux (i+1) nexts) ()
    in
    aux 0
      
  let segmentize =
    let rec aux i x () = match x () with
      | Iter.Nil -> Iter.empty ()
      | _ ->
        let p w = W.length w = i in
        let segment_i = Iter.take_while p x in
        let nexts = Iter.drop_while p x in
        (segment_i @: aux (i+1) nexts) ()
    in
    aux 0
  
  let concatenate s1 s2 =
    let segms1 = segmentize s1 in
    let segms2 = segmentize s2 in
    let is_exhausted seq n =
      Iter.for_all CCOpt.is_none @@
      Iter.map (Iter.nth_opt seq) (n / 2 -- n)
    in
    let subterms_of_length n =
      let get_segment s k =
        CCOpt.get_or ~default:Iter.empty (Iter.nth_opt s k)
      in
      let combine_segments i =
        get_segment segms1 (n - i) >>= fun x1 ->
        get_segment segms2 i >|= fun x2 ->
        W.append x1 x2
      in
      union_n @@ Iter.map combine_segments (0 -- n)
    in 
    let rec collect n () =
      if is_exhausted segms1 n && is_exhausted segms2 n then
        Iter.empty ()
      else
        (subterms_of_length n @ collect (n+1)) ()
    in 
    collect 0
    
  (** Star *)
  let star seq =
    let is_infinite = Iter.exists (fun w -> W.length w > 0) seq in
    let segms = segmentize_infite seq in
    let rec words_of_partition p = match p () with
      | Iter.Nil -> Iter.return W.empty
      | Cons (i, p') ->
        Iter.nth segms i >>= fun w' ->
        words_of_partition p' >>= fun w ->
        Iter.return (W.append w w')
    in
    let subterm_of_length n =
      union_n @@ Iter.map words_of_partition @@ iter_partitions n
    in
    let rec collect n () =
      (subterm_of_length n @ collect (n + 1)) () in
    if is_infinite
    then W.empty @: collect 1
    else Iter.return W.empty
  
  let sigma_star sigma =
    let cons_all term_k' =
      sigma >>= fun c ->
      term_k' >|= fun w ->
      W.cons c w
    in
    let rec collect acc =
      acc @ fun () -> collect (cons_all acc) ()
    in
    collect (Iter.return W.empty)

  (****)
  
  let rec gen sigma r = match r with
    | Zero -> Iter.empty
    | One -> Iter.return W.empty
    | Atom x -> Iter.return @@ W.singleton x
    | Seq (r1, r2) -> concatenate (gen sigma r1) (gen sigma r2)
    | Or (r1, r2) -> union (gen sigma r1) (gen sigma r2)
    | And (r1, r2) -> intersection (gen sigma r1) (gen sigma r2)
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
