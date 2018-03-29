module type S = sig
  type char
  type t
  val empty : t
  val singleton : char -> t
  val length : t -> int
  val append : t -> t -> t
  val cons : char -> t -> t
  val pp : Format.formatter -> t -> unit
end

module List (C : sig include Set.OrderedType val pp : t Fmt.t end) = struct
  type char = C.t
  type t = C.t list
  let empty = []
  let singleton x = [x]
  let length = List.length
  let append = List.append
  let cons = List.cons
  let pp = CCFormat.(list ~sep:(fun _ () -> ()) C.pp)
end

module String = struct
  type nonrec char = char
  include CCString
  let empty = ""
  let singleton = make 1
  let cons c s = singleton c ^ s
  let append = (^)
  let compare_char = Char.compare
  let pp fmt s =
    if s = "" then
      Format.pp_print_string fmt "ε"
    else
      Format.pp_print_string fmt s
  let to_seq s k = String.iter k s
end
