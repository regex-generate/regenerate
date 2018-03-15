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
  type nonrec char = char
  include CCString
  let empty = ""
  let singleton = make 1
  let cons c s = singleton c ^ s
  let append = (^)
  let compare_char = Char.compare
  let compare = CCString.compare
  let pp = CCFormat.Dump.string
  let to_seq s k = String.iter k s
  let of_list l =
    let buf = Buffer.create (CCList.length l) in
    CCList.iter (fun c -> Buffer.add_char buf c) l;
    Buffer.contents buf
end
