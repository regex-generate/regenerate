
type 'a t
  = Zero
  | One
  | Atom of 'a
  | Seq of 'a t * 'a t
  | Or of 'a t * 'a t
  | And of 'a t * 'a t
  | Not of 'a t
  | Star of 'a t


let epsilon = One
let void = Zero
let atom s = Atom s
let char c = atom c
(* let charset cs = Chars (CSet.of_list cs) *)
let enumerate c1 c2 =
  let open OSeq in
  if c1 > c2 then None
  else
    let rec aux i m () =
      if i > m then Nil
      else Cons (Char.chr i, aux (i+1) m)
    in
    Some (aux (Char.code c1) (Char.code c2))

let rec reduce init f = function
  | [] -> init
  | [x] -> x
  | x :: l -> f x (reduce init f l)
                    
let concat l = reduce One (fun x y -> Seq (x,y)) l
let alt x y = Or (x,y)
let inter x y = And (x,y)
let compl x = Not x

let star x = Star x
let plus x = Seq (x, star x)
let opt x = Or (x, One)
