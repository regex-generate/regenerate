open Test

let f i l =
  let file = Fmt.strf "re_trie_%i.csv" i in
  let oc = open_out file in
  let n = 
    L.gen sigma l
    |> Test.measure_until
      ~limit:(Mtime.Span.of_uint64_ns (5_000_000_000L))
      ~interval:20
      oc
  in
  Gc.full_major ();
  Fmt.pr "Re %i: %i elements@." i n

let () =
  Array.iteri f langs

