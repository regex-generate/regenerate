open Regenerate
open Cmdliner

module C = Char
module W = Word.String
module S = Segments.StrictSet(W)
module L = Make (Char) (W) (S)

let alphabet =
  S.of_list @@
  OSeq.to_list @@
  OSeq.map W.singleton @@
  CCOpt.get_exn @@ enumerate ' ' '~'

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

let print_all n re =
  Fmt_tty.setup_std_outputs ();
  L.gen alphabet re
  |> L.flatten
  |> CCOpt.map_or ~default:(fun x -> x) Sequence.take n
  |> Fmt.pr "%a@." (CCFormat.seq ~sep:(Fmt.unit "@.") W.pp)

let term =
  let info =
    Term.info "regenerate"
      ~doc:"Generate strings matching a given regular expression."
  in
  let t = Term.(const print_all $ bound $ re_arg) in
  (t, info)

let () = Term.exit @@ Term.eval term
