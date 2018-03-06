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

type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

val empty : 'a t

val cons : 'a -> 'a t -> 'a t

val return : 'a -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val iter : ('a -> unit) -> 'a t -> unit

val append : 'a t -> 'a t -> 'a t

val take_while : ('a -> bool) -> 'a t -> 'a t
    
val drop_while : ('a -> bool) -> 'a t -> 'a t

val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool

val range : int -> int -> int t

val nth : 'a t -> int -> 'a
val nth_opt : 'a t -> int -> 'a option

val of_list : 'a list -> 'a t

module Infix : sig
  val (--) : int -> int -> int t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (@) : 'a t -> 'a t -> 'a t
  val (@:) : 'a -> 'a t -> 'a t
end
