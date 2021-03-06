  
module type WORD = sig
  type t
  type char

  val compare_char : char -> char -> int
  val append : t -> t -> t
  val to_iter : t -> char Iter.t
  val of_list : char list -> t
end


module Make(W : WORD) : Sigs.S with type elt = W.t
module String : Sigs.S with type elt = string
