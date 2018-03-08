open Re_testing

(* module W = Word.List(Char) *)
module C = Char
module W = Word.String
module S = Segments.ThunkList(W)
module L = Make (Char) (W) (S)

(* let print : L.lang -> unit =
 *   L.pp ~pp_sep:(Fmt.unit "@.")
 *     (Fmt.hbox @@ S.pp ~sep:", " W.pp) Fmt.stdout *)


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


let () =
  (* let re = L.(Seq (Not (Atom 'a'), Atom 'a')) in *)
  (* let re = L.(Star (Atom 'a')) in *)
  (* let re = L.(Star (Seq (Atom 'a', Star (Atom 'b')))) in *)
  let re = L.(Star (Seq (Or (Atom 'a', One), Star (Atom 'b')))) in
  let sigma = OSeq.of_list ['a'; 'b' ; 'c'] in
  L.gen sigma re
  |> L.flatten
  |> Sequence.take 50000
  (* |> (fun x -> assert_sorted x ; x) *)
  |> Fmt.pr "%a@." CCFormat.(map Sequence.length int)
  (* |> print *)
