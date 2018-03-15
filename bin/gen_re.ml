open Regenerate
open Cmdliner

module W = Word.String
module S = Segments.Trie.String

type segment_imple =
  | ThunkList
  | ThunkListMemo
  | LazyList
  | StrictSet
  | Trie

module LThunkList = Make (Word.String) (Segments.ThunkList (Word.String))
module LThunkListMemo = Make (Word.String) (Segments.ThunkListMemo (Word.String))
module LLazyList = Make (Word.String) (Segments.LazyList (Word.String))
module LStrictSet = Make (Word.String) (Segments.StrictSet (Word.String))
module LTrie = Make (Word.String) (Segments.Trie.Make (Word.String))

let comp f g h sigma re =
  let sigma = h @@ List.map W.singleton @@ CCString.to_list sigma in
  g @@ f ~sigma re 

let get_impl  = function
  | ThunkList -> comp LThunkList.gen LThunkList.flatten LThunkList.Segment.of_list
  | ThunkListMemo -> comp LThunkListMemo.gen LThunkListMemo.flatten LThunkListMemo.Segment.of_list
  | LazyList -> comp LLazyList.gen LLazyList.flatten LLazyList.Segment.of_list
  | StrictSet -> comp LStrictSet.gen LStrictSet.flatten LStrictSet.Segment.of_list
  | Trie -> comp LTrie.gen LTrie.flatten LTrie.Segment.of_list

let backend = 
  let doc = Arg.info ~docv:"IMPLEM" ~doc:"Implementation to use."
      ["i";"implementation"]
  in
  let c = Arg.enum [
      "ThunkList", ThunkList ;
      "ThunkListMemo", ThunkListMemo ;
      "LazyList", LazyList ;
      "StrictSet", StrictSet ;
      "Trie", Trie ;
    ]
  in
  Arg.(value & opt c ThunkList & doc)


let re_arg =
  let err_msg = function
    | `Parse_error -> `Msg "Incorrect regular expression"
    | `Not_supported -> `Msg "Unsupported syntax"
  in
  let printer fmt _ = Fmt.pf fmt "<re>" in
  let parser s = match parse s with Ok x -> Ok x | Error e -> Error (err_msg e) in
  let reconv = Arg.conv ~docv:"REGEX" (parser, printer) in
  let doc =
    Arg.info ~docv:"REGEX" ~doc:"Regular expression following Posix's Extended Regular Expression syntax."
      []
  in
  Arg.(required & pos 0 (some reconv) None & doc)

let bound =
  let doc = Arg.info ~docv:"BOUND" ~doc:"Limit the number of samples."
      ["b";"bound"]
  in
  Arg.(value & opt (some int) None & doc)

let stutter =
  let doc = Arg.info ~docv:"STUTTER" ~doc:"Interval for stuttering."
      ["s";"stutter"]
  in
  Arg.(value & opt int 20 & doc)

let time_limit = 
  let doc = Arg.info ~docv:"LIMIT" ~doc:"Time limit for stuttering in seconds."
      ["l";"limit"]
  in
  Arg.(value & opt int64 5L & doc)

let sigma =
  let doc = Arg.info ~docv:"ALPHABET" ~doc:"Alphabet used by the regular expression"
      ["a";"sigma"]
  in
  let default = OSeq.to_string @@ CCOpt.get_exn @@ enumerate ' ' '~' in
  Arg.(value & opt string default & doc)

let setup backend re sigma = 
  Fmt_tty.setup_std_outputs ();
  get_impl backend re sigma

let print_all backend re sigma n =
  setup backend sigma re
  |> CCOpt.map_or ~default:(fun x -> x) Sequence.take n
  |> Fmt.pr "%a@." (CCFormat.seq ~sep:(Fmt.unit "@.") W.pp)

let count backend re sigma n =
  let n = CCOpt.get_or ~default:1000 n in
  let c = Mtime_clock.counter () in
  let i =
    setup backend sigma re
    |> Sequence.take n
    |> Sequence.length
  in
  Fmt.pr "Max count: %i@.Actual Count: %i@.Time: %a@." n i
    Mtime.Span.pp (Mtime_clock.count c)

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
      output i t ;
      if Mtime.Span.compare limit t < 0
      then raise Exit
      else ()
    end
  in
  (try Sequence.iter f lang with Exit -> ());
  close_out oc ;
  ()

let running_profile backend re sigma stutter limit =
  let oc = stdout in
  setup backend sigma re
  |> measure_until
      ~limit:(Mtime.Span.of_uint64_ns (Int64.mul limit 1_000_000_000L))
      ~interval:stutter
      oc

let gen_cmd =
  let info =
    Term.info "generate"
      ~doc:"Generate strings matching a given regular expression."
  in
  let t = Term.(const print_all $ backend $ re_arg $ sigma $ bound) in
  (t, info)

let count_cmd =
  let info =
    Term.info "count"
      ~doc:"Time language generation up to a certain number of strings."
  in
  let t = Term.(const count $ backend $ re_arg $ sigma $ bound) in
  (t, info)

let profile_cmd = 
  let info =
    Term.info "profile"
      ~doc:"Profile language generation for the given regular expression."
  in
  let t = Term.(const running_profile $ backend $ re_arg $ sigma $ stutter $ time_limit) in
  (t, info)

let cmds = [ profile_cmd ; count_cmd ; gen_cmd ]
let default_cmd = 
  let doc = "Language generation for regular expressions." in
  let info = Term.info "regenerate" ~doc in
  let t = Term.(ret (const @@ `Help (`Pager, None))) in
  (t, info)

let () = Term.exit @@ Term.eval_choice default_cmd cmds
