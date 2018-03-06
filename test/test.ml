open Re_testing

module W = WString
module S = Make (Char) (WString)

let () =
  (* let re = S.(Seq (Not (Atom 'a'), Atom 'a')) in *)
  let re = S.(Star (Atom 'a')) in
  let sigma = Iter.of_list ['a'; 'b' ; 'c'] in
  S.gen sigma re |>
  Iter.take_while (fun i -> W.length i < 10) |>
  Iter.iter (Format.printf "%a@.%!" W.pp)
