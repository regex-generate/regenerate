type 'a cset = 'a list

type 'a t
  = One
  | Set of 'a cset
  | Seq of 'a t * 'a t
  | Or of 'a t * 'a t
  | And of 'a t * 'a t
  | Not of 'a t
  | Rep of int * int option * 'a t

(** Smart constructors *)

let epsilon = One
let void = Set []
let atom c = Set [c]
let char c = atom c
let charset cs = Set cs
let enumerate c1 c2 =
  if c1 > c2 then None
  else
    let rec aux i m =
      if i > m then []
      else Char.chr i :: aux (i+1) m
    in
    Some (aux (Char.code c1) (Char.code c2))

let rec reduce init f = function
  | [] -> init
  | [x] -> x
  | x :: l -> f x (reduce init f l)
                    
let seq l = reduce One (fun x y -> Seq (x,y)) l
let alt x y = Or (x,y)
let inter x y = And (x,y)
let compl x = Not x

let rep i j x = Rep (i, j, x)
let star x = rep 0 None x
let plus x = rep 1 None x
let opt x = rep 0 (Some 1) x

(** QCheck utilities *)

let rec size = function
  | One -> 1
  | Set _ -> 1
  | Rep (_,_,a)
  | Not a -> size a + 1
  | Or (a,b)
  | And (a,b)
  | Seq (a,b) -> size a + size b + 1

let prio = function
  | Seq (_,_) -> 1
  | And (_,_) -> 2
  | Or (_,_) -> 3
  | Not _ -> 4
  | Rep (_,_,_) -> 5
  | One
  | Set _ -> 6

let rec pp ppalpha fmt x =
  let f fmt y =
    if prio y < prio x
    then Fmt.parens (pp ppalpha) fmt y
    else pp ppalpha fmt y
  in
  match x with
  | One -> Fmt.pf fmt ""
  | Set [x] -> Fmt.pf fmt "%a" ppalpha x
  | Set l -> Fmt.pf fmt "[%a]" (Fmt.list ~sep:Fmt.nop ppalpha) l
  | Seq (a,b) -> Fmt.pf fmt "%a%a" f a f b
  | Or (a,b) -> Fmt.pf fmt "%a|%a" f a f b
  | And (a,b) -> Fmt.pf fmt "%a&%a" f a f b
  | Not a -> Fmt.pf fmt "~%a" f a
  | Rep (0,None,a) -> Fmt.pf fmt "%a*" f a
  | Rep (1,None,a) -> Fmt.pf fmt "%a+" f a
  | Rep (i,None,a) -> Fmt.pf fmt "%a{%i,}" f a i
  | Rep (i,Some j,a) -> Fmt.pf fmt "%a{%i,%i}" f a i j

let gen ~compl:with_compl alphabet =
  let open QCheck.Gen in
  let opt a = frequency [ 1, pure None ; 1, map CCOpt.return a] in

  let proba_compl = if with_compl then 3 else 0 in
  let gatom = alphabet >|= atom in
  let gset =
    map (fun l -> charset @@ CCList.uniq ~eq:(=) l) @@ list alphabet
  in
  let gbase = frequency [
      1, pure void ;
      1, pure epsilon ;
      8, gatom ;
      5, gset ;
    ] in
  let rec gen n st =
    if n <= 1 then gbase st else
      frequency [
        1, gbase ;
        proba_compl, gcompl n ;
        3, gbin n alt ;
        3, gbin n inter ;
        5, gbin n (fun x y -> Seq (x,y)) ;
        3, grep n ;
      ] st
  and grep n =
    int_bound 10 >>= fun i ->
    opt (int_bound 10) >>= fun j ->
    gen (n-1) >|= fun a ->
    rep i (CCOpt.map ((+) i) j) a
  and gcompl n = gen (n-1) >|= compl
  and gbin n f =
    gen ((n-1)/2) >>= fun a ->
    gen ((n-1)/2) >|= fun b ->
    f a b
  in
  sized_size (int_range 2 5) gen
