(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Seq]: functional iterators *)

type (+'a,+'e) node =
  | Ret of 'e
  | Cons of 'a * ('a, 'e) t

and ('a, 'e) t = unit -> ('a, 'e) node

let return x () = Ret x

let cons e s () = Cons(e, s)

(* let return x () = Cons (x, empty) *)

let tail s () = match s() with
  | Ret _ -> assert false
  | Cons (_, next) -> next ()
                        
let rec map f g seq () = match seq() with
  | Ret x -> Ret (g x)
  | Cons (x, next) -> Cons (f x, map f g next)

(* let rec filter_map f seq () = match seq() with
 *   | Ret x -> Ret x
 *   | Cons (x, next) ->
 *       match f x with
 *         | None -> filter_map f next ()
 *         | Some y -> Cons (y, filter_map f next)
 * 
 * let rec filter f seq () = match seq() with
 *   | Ret x -> Ret x
 *   | Cons (x, next) ->
 *       if f x
 *       then Cons (x, filter f next)
 *       else filter f next () *)

let rec take n e l () =
  if n=0 then Ret e
  else match l () with
    | Ret e -> Ret e
    | Cons (x,l') -> Cons (x, take (n-1) e l')

(* let rec flat_map f seq () = match seq () with
 *   | Ret -> Ret
 *   | Cons (x, next) ->
 *     flat_map_app f (f x) next ()
 * 
 * (\* this is [append seq (flat_map f tail)] *\)
 * and flat_map_app f seq tail () = match seq () with
 *   | Ret -> flat_map f tail ()
 *   | Cons (x, next) ->
 *     Cons (x, flat_map_app f next tail) *)

(* let fold f acc seq =
 *   let rec aux f g acc seq = match seq () with
 *     | Ret e -> g e
 *     | Cons (x, next) ->
 *         let acc = f acc x in
 *         aux f g acc next
 *   in
 *   aux f acc seq *)

(* let iter f seq =
 *   let rec aux seq = match seq () with
 *     | Ret x -> ()
 *     | Cons (x, next) ->
 *         f x;
 *         aux next
 *   in
 *   aux seq *)

(* let rec append s1 s2 () = match s1 () with
 *   | Ret -> s2 ()
 *   | Cons (x, next) -> Cons (x, append next s2) *)

(* let rec append_node s1 s2 () = match s1 () with
 *   | Ret -> s2
 *   | Cons (x, next) -> Cons (x, append_node next s2) *)

(* let rec take_while f s () = match s () with
 *   | Ret ->
 *     Ret
 *   | Cons(e, s) ->
 *     if f e then
 *       Cons(e, take_while f s)
 *     else
 *       Ret
 * 
 * let rec drop_while f s = match s () with
 *   | Ret -> empty
 *   | Cons(e, s) ->
 *     if f e then
 *       drop_while f s
 *     else
 *       cons e s *)

(* let rec drop n s =
 *   if n <= 0 then s else match s () with
 *     | Ret _ as x -> fun () -> x
 *     | Cons(e, s) -> drop (n-1) s *)

let rec for_all f s = match s () with
  | Ret _ -> true
  | Cons(e, s) -> f e && for_all f s

let rec exists f s = match s () with
  | Ret _ -> false
  | Cons(e, s) -> f e || exists f s

(* let rec range i j =
 *   if i > j then empty
 *   else cons i @@ range (i+1) j *)

(* let rec nth s n =
 *   match s () with
 *   | Ret -> raise Not_found
 *   | Cons (x, s') ->
 *     if n = 0 then x else nth s' (n-1)
 *         
 * let rec nth_opt s n =
 *   match s () with
 *   | Ret -> None
 *   | Cons (x, s') ->
 *     if n = 0 then Some x else nth_opt s' (n-1) *)

(* let of_list l =
 *   let rec aux l () = match l with
 *     | [] -> Ret
 *     | x::l' -> Cons(x, aux l')
 *   in
 *   aux l *)


(* (\** {4 Mutable heap (taken from heap.ml to avoid dependencies)} *\)
 * module Heap = struct
 *   type 'a t = {
 *     tree : 'a tree;
 *     cmp : 'a -> 'a -> int;
 *   } (\** A pairing tree heap with the given comparison function *\)
 * 
 *   and 'a tree =
 *     | Empty
 *     | Node of 'a * 'a tree * 'a tree
 * 
 *   let empty ~cmp = {
 *     tree = Empty;
 *     cmp;
 *   }
 * 
 *   let is_empty h =
 *     match h.tree with
 *     | Empty -> true
 *     | Node _ -> false
 * 
 *   let rec union ~cmp t1 t2 = match t1, t2 with
 *     | Empty, _ -> t2
 *     | _, Empty -> t1
 *     | Node (x1, l1, r1), Node (x2, l2, r2) ->
 *         if cmp x1 x2 <= 0
 *         then Node (x1, union ~cmp t2 r1, l1)
 *         else Node (x2, union ~cmp t1 r2, l2)
 * 
 *   let insert h x =
 *     { h with tree = union ~cmp:h.cmp (Node (x, Empty, Empty)) h.tree }
 * 
 *   let pop h = match h.tree with
 *     | Empty -> raise Not_found
 *     | Node (x, l, r) ->
 *       x, { h with tree = union ~cmp:h.cmp l r }
 * end
 * 
 * let sorted_merge_n ?(cmp=Pervasives.compare) l =
 *   (\* make a heap of (value, generator) *\)
 *   let cmp (v1,_) (v2,_) = cmp v1 v2 in
 *   (\* add initial values *\)
 *   let h0 = fold_left
 *     (fun h s -> match s() with
 *        | Cons (x, s') -> Heap.insert h (x, s')
 *        | Ret -> h)
 *     (Heap.empty ~cmp)
 *     l
 *   in
 *   let rec f heap () =
 *     if Heap.is_empty heap then Ret
 *     else begin
 *       let (x, seq), heap = Heap.pop heap in
 *       match seq() with
 *       | Cons (y, seq') ->
 *           let heap = Heap.insert heap (y, seq') in  (\* insert next value *\)
 *           Cons (x, f heap)
 *       | Ret ->
 *         Cons (x, f heap) (\* gen empty, drop it *\)
 *     end
 *   in
 *   f h0 *)

type ('a, 'e) memoize =
  | MemoThunk
  | MemoSave of ('a, 'e) node

let rec memoize f =
  let r = ref MemoThunk in
  fun () -> match !r with
    | MemoSave l -> l
    | MemoThunk ->
      let l = match f() with
        | Ret x -> Ret x
        | Cons (x, tail) -> Cons (x, memoize tail)
      in
      r := MemoSave l;
      l

module Infix = struct
  (* let (--) = range
   * let (>>=) x f = flat_map f x
   * let (>|=) x f = map f x
   * let (@@@) = append *)
  let (@:) = cons
end
