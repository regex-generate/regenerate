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

(** @since NEXT_RELEASE *)

type (+'a,+'e) node =
  | Ret of 'e
  | Cons of 'a * ('a, 'e) t

and ('a, 'e) t = unit -> ('a, 'e) node

val return : 'e -> ('a, 'e) t

val cons : 'a -> ('a, 'e) t -> ('a, 'e) t

val tail : ('a, 'e) t -> ('a, 'e) t

(* val return : 'a -> ('a, 'e) t *)

val map : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t

(* val flat_map : ('a -> 'b t) -> 'a t -> 'b t *)

(* val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a *)

(* val iter : ('a -> unit) -> ('a, 'e) t -> unit *)

(* val append : 'a t -> 'a t -> 'a t *)

val take : int -> 'e -> ('a, 'e) t -> ('a, 'e) t

(* val take_while : ('a -> bool) -> ('a, 'e) t -> ('a, 'e) t *)
    
(* val drop_while : ('a -> bool) -> ('a, 'e) t -> ('a, 'e) t *)

val for_all : ('a -> bool) -> ('a, 'e) t -> bool
val exists : ('a -> bool) -> ('a, 'e) t -> bool

(* val range : int -> int -> int t *)

(* val nth : ('a, 'e) t -> int -> 'a
 * val nth_opt : ('a, 'e) t -> int -> 'a option *)

(* val of_list : 'a list -> ('a, 'e) t *)

(* val sorted_merge_n : 
 *   ?cmp:('a -> 'a -> int) ->
 *   'a t t -> 'a t *)

(* val pp : 
 *   ?pp_sep:(Format.formatter -> unit -> unit) ->
 *   (Format.formatter -> 'a -> unit) ->
 *   (Format.formatter -> 'e -> unit) ->
 *   Format.formatter -> ('a, 'e) t -> unit *)

val memoize : ('a, 'e) t -> ('a, 'e) t

module Infix : sig
  (* val (--) : int -> int -> int t *)
  (* val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
   * val (>|=) : 'a t -> ('a -> 'b) -> 'b t
   * val (@@@) : 'a t -> 'a t -> 'a t *)
  val (@:) : 'a -> ('a, 'e) t -> ('a, 'e) t
end
