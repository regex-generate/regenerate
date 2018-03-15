open Regenerate

(* module W = Word.List(Char) *)
module C = Char
module W = Word.String
module S = Segments.Trie.String
module L = Make (Char) (W) (S)

let assert_sorted s =
  let rec aux x = function
    | OSeq.Nil -> ()
    | Cons (y, s) ->
      if W.compare x y >= 0 then
        Fmt.invalid_arg "Not sorted: %a %a" W.pp x W.pp y
      else aux y (s())
  in
  match s () with
  | OSeq.Nil -> ()
  | Cons (x, s) -> aux x (s())

(* let (!!) = of_list
 * 
 * let sigma = S.of_list ['a';'b';'c']
 * let full = sigma_star sigma
 * let l = !!["a"; "ab"; "c" ;"abc"]
 * let a = !!["a"] *)

let sigma = S.of_list ["a"; "b" ; "c"]

let langs = [|
  Seq (Not (Atom 'a'), Atom 'a') ;
  Star (Atom 'a') ;
  Star (Seq (Atom 'a', Star (Atom 'b'))) ;
  Star (Seq (Or (Atom 'a', One), Star (Atom 'b'))) ;
|]

let id x = x

let print_all ?n (lang : L.lang) =
  lang
  (* |> L.flatten *)
  |> (match n with Some n -> Iter.take n L.Nothing | None -> id)
  |> L.print W.pp

let time_up_to_gen n lang =
  let i = lang
          |> Iter.take n L.Nothing
          |> L.flatten
          |> Sequence.length
  in
  Fmt.pr "Max length: %i@.Count: %i@.Time: %a@." n i
    Mtime.Span.pp (Mtime_clock.elapsed())

let time_up_to_length n lang =
  let i = lang
          |> L.flatten
          |> Sequence.take n
          |> Sequence.length
  in
  Fmt.pr "Max count: %i@.Actual Count: %i@.Time: %a@." n i
    Mtime.Span.pp (Mtime_clock.elapsed())

let measure_until ~limit ~interval oc lang =
  let c = Mtime_clock.counter () in
  let r = ref 0 in
  let fmt = Format.formatter_of_out_channel oc in
  let output i s = Fmt.pf fmt "%i\t%f@." i (Mtime.Span.to_s s) in
  let f _ =
    incr r ;
    let i = !r in
    if i mod interval = 0 then begin
      let t = Mtime_clock.count c in
      if Mtime.Span.compare limit t < 0
      then raise Exit
      else output i t
    end
  in
  (try Sequence.iter f (L.flatten lang) with Exit -> ());
  close_out oc ;
  !r
                                                    
