module type OrderedMonoid = sig
  include Set.OrderedType
  val append : t -> t -> t
end

module type S = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val return : elt -> t

  val append: t -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val merge : t list -> t

  val of_list : elt list -> t
  val to_seq : t -> elt Iter.t

  val memoize : t -> t
end
