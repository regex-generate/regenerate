open Regenerate

let () = Printexc.record_backtrace true

let rec to_re = let open Regex in function
  | One -> Re.epsilon
  | Set (true, l) -> Re.set @@ CCString.of_list l
  | Set (false, l) -> Re.compl [Re.set @@ CCString.of_list l]
  | Seq (re, re') -> Re.seq [to_re re; to_re re']
  | Or (re, re') -> Re.alt [to_re re; to_re re']
  | And (re, re') -> Re.inter [to_re re; to_re re']
  | Not re -> assert false
  | Rep (i,j,re) -> Re.repn (to_re re) i j

let f (re, pos, neg) =
  let cre =
    try
      Re.compile @@ Re.whole_string @@ to_re re
    with _ -> QCheck.assume_fail ()
  in
  List.for_all (fun s -> Re.execp cre s) pos &&
  List.for_all (fun s -> not @@ Re.execp cre s) neg

let test =
  let sigma = ['a'; 'b'; 'c'] in
  let module W = Word.String in
  let module S = Segments.ThunkList(W) in
  let a =
    arbitrary
      (module W) (module S)
      ~compl:false ~pp:Fmt.char ~samples:100
      sigma
  in
  QCheck.Test.make a f

let () = QCheck_runner.run_tests_main [test]
