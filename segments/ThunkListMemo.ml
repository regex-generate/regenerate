module Make (K : Sigs.OrderedMonoid) 
  : Sigs.S with type elt = K.t and type t = ThunkList.Make(K).t
= struct
  open OSeq
  include ThunkList.Make(K)
      
  let memoize f =
    let r = CCVector.create () in
    let rec f' i seq () =
      if i < CCVector.length r
      then CCVector.get r i
      else 
        let l = match seq() with
          | Nil -> Nil
          | Cons (x, tail) -> Cons (x, f' (i+1) tail)
        in
        CCVector.push r l;
        l
    in
    f' 0 f
end
