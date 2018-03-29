open Regenerate
open Cmdliner

module W = Word.String

type segment_impl =
  | ThunkList
  | ThunkListMemo
  | LazyList
  | StrictSet
  | Trie

module type ARG = sig
  include Segments.OrderedMonoid
  include Segments.Trie.WORD with type t := t
end
module type S =
  functor (W : ARG) -> (Segments.S with type elt = W.t)

let get_impl_mod : segment_impl -> (module S) = let open Segments in function
  | ThunkList -> (module ThunkList)
  | ThunkListMemo -> (module ThunkListMemo)
  | LazyList -> (module LazyList)
  | StrictSet -> (module StrictSet)
  | Trie -> (module Trie.Make)

type conf =
  | All
  | Sample of { skip : int ; length : int option }
  | Take of int

let[@inline] make_impl ~impl =
  let module M = (val get_impl_mod impl) in
  let module S = M(W) in
  let module A = Regenerate.Make (W) (S) in
  fun[@inline] ~sigma ->
    let sigma = S.of_list @@ List.map W.singleton @@ CCString.to_list sigma in
    let module Sigma = struct type t = S.t let sigma = sigma end in
    let module A = A (Sigma) in
    fun conf re ->
      let lang = A.gen re in
      match conf with
      | All -> A.flatten lang
      | Sample { skip ; length } -> A.sample ~skip ?n:length lang
      | Take n -> Sequence.take n @@ A.flatten lang

let tl = make_impl ~impl:ThunkList ~sigma:"ab"
let tlm = make_impl ~impl:ThunkListMemo ~sigma:"ab"
let ll = make_impl ~impl:LazyList ~sigma:"ab"
let set = make_impl ~impl:StrictSet ~sigma:"ab"
let trie = make_impl ~impl:Trie ~sigma:"ab"

let get_impl ~impl ~sigma = if sigma <> "ab" then
    make_impl ~impl ~sigma
  else match impl with
  | ThunkList -> tl
  | ThunkListMemo -> tlm
  | LazyList -> ll
  | StrictSet -> set
  | Trie -> trie

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

let shutter =
  let doc = Arg.info ~docv:"SKIP"
      ~doc:"Interval for stuttering."
      ["s";"shutter"]
  in
  Arg.(value & opt int 20 & doc)

let skip =
  let doc = Arg.info ~docv:"SAMPLE"
      ~doc:"Average sampling interval."
      ["s";"sample"]
  in
  Arg.(value & opt (some int) None & doc)


let time_limit = 
  let doc = Arg.info ~docv:"LIMIT" ~doc:"Time limit for stuttering in seconds."
      ["l";"limit"]
  in
  Arg.(value & opt int64 5L & doc)

let sigma =
  let doc = Arg.info ~docv:"ALPHABET" ~doc:"Alphabet used by the regular expression"
      ["a";"sigma"]
  in
  let default = CCString.of_list @@ CCOpt.get_exn @@ Regex.enumerate ' ' '~' in
  Arg.(value & opt string default & doc)

let setup ~impl ~sigma re = 
  Fmt_tty.setup_std_outputs ();
  get_impl ~impl ~sigma re

let print_all impl sigma re length skip =
  let conf = match length, skip with
    | Some n, None -> Take n
    | None, None -> All
    | _, Some skip -> Sample { skip ; length }
  in 
  setup ~impl ~sigma conf re
  |> Fmt.pr "%a@." (CCFormat.seq ~sep:(Fmt.unit "@.") W.pp)

let count impl sigma re n =
  let n = CCOpt.get_or ~default:1000 n in
  let c = Mtime_clock.counter () in
  let i =
    setup ~impl ~sigma (Take n) re
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

let running_profile impl re sigma stutter limit =
  let oc = stdout in
  setup ~impl ~sigma All re
  |> measure_until
      ~limit:(Mtime.Span.of_uint64_ns (Int64.mul limit 1_000_000_000L))
      ~interval:stutter
      oc

let gen_cmd =
  let info =
    Term.info "generate"
      ~doc:"Generate all strings matching a given regular expression."
  in
  let t = Term.(const print_all $ backend $ sigma $ re_arg $ bound $ skip) in
  (t, info)
  
let count_cmd =
  let info =
    Term.info "count"
      ~doc:"Time language generation up to a certain number of strings."
  in
  let t = Term.(const count $ backend $ sigma $ re_arg $ bound) in
  (t, info)

let profile_cmd = 
  let info =
    Term.info "profile"
      ~doc:"Profile language generation for the given regular expression."
  in
  let t = Term.(const running_profile $ backend $ re_arg $ sigma $ shutter $ time_limit) in
  (t, info)

let cmds = [ profile_cmd ; count_cmd ; gen_cmd ]
let default_cmd = 
  let doc = "Language generation for regular expressions." in
  let info = Term.info "regenerate" ~doc in
  let t = Term.(ret (const @@ `Help (`Pager, None))) in
  (t, info)

let () = Term.exit @@ Term.eval_choice default_cmd cmds
