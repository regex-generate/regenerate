open Regenerate

(* module W = Word.List(Char) *)
module W = Word.String
module S = Segments.ThunkList(W)
module Sigma = struct type t = S.t
  let sigma = S.of_list ["a"; "b" ; "c"]
end
module L = Make (W) (S) (Sigma)

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

let langs = Regex.[|
  seq [compl (char 'a'); char 'a'] ;
  star (atom 'a') ;
  star (Seq (atom 'a', star (atom 'b'))) ;
  star (Seq (Or (atom 'a', One), star (atom 'b'))) ;
|]

let id x = x

let rec take n s () = match s() with
  | L.Everything | L.Nothing as v -> v
  | L.Cons (seg, s) ->
    if n <= 0 then L.Nothing
    else L.Cons (seg, take (n-1) s)

let print_all ?n (lang : L.lang) =
  lang
  (* |> L.flatten *)
  |> (match n with Some n -> take n | None -> id)
  |> Fmt.pr "%a@." L.pp

let time_up_to_gen n lang =
  let i = lang
          |> take n
          |> L.flatten
          |> Iter.length
  in
  Fmt.pr "Max length: %i@.Count: %i@.Time: %a@." n i
    Mtime.Span.pp (Mtime_clock.elapsed())

let time_up_to_length n lang =
  let i = lang
          |> L.flatten
          |> Iter.take n
          |> Iter.length
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
  (try Iter.iter f (L.flatten lang) with Exit -> ());
  close_out oc ;
  !r
                                                    
(*

open Regenerate ;;
open Test ;;
#install_printer pp_seg ;;
#install_printer pp ;;

*)
