open Re_testing

module W = WList(Char)
module S = Make (Char) (W)

let () =
  (* let re = S.(Seq (Not (Atom 'a'), Atom 'a')) in *)
  let re = S.(Star (Atom 'a')) in
  let sigma = Iter.of_list ['a'; 'b' ; 'c'] in
  S.gen sigma re |>
  Iter.take_while (fun i -> W.length i < 20) |>
  Iter.iter (Format.printf "%a@.%!" W.pp)
