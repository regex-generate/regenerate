module List (C : Set.OrderedType) = struct
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

module String = struct
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
  let pp = CCFormat.Dump.string
end
