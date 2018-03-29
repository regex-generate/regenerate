open Js_of_ocaml
open Regenerate

(* let alphabet = CCOpt.get_exn @@ Regex.enumerate ' ' '~' *)
let alphabet = ['a';'b']
module W = Word.String
module S = Segments.ThunkList(W)
let sigma = S.of_list @@ List.map W.singleton alphabet
module Sigma = struct type t = S.t let sigma = sigma end
module L = Make (W) (S) (Sigma)

type mode = All | Sample

let gen_examples mode re =
  let f l = match mode with
    | Sample -> Sequence.take 20 @@ L.sample ~skip:5 l
    | All -> Sequence.take 500 @@ L.flatten l
  in 
  let lang = L.gen re in
  let pos = f lang in
  let neg = f @@ L.compl lang in
  pos, neg

(** Web part *)

let instances = Dom_html.getElementById "instances"
let pos_instances = Dom_html.getElementById "pos-instances"
let neg_instances = Dom_html.getElementById "neg-instances"
let re_form = Dom_html.getElementById "re-form"

let re_input =
  CCOpt.get_exn @@
  Dom_html.getElementById_coerce "re-input" Dom_html.CoerceTo.input
let re_select = 
  CCOpt.get_exn @@
  Dom_html.getElementById_coerce "re-select" Dom_html.CoerceTo.select

let get_mode () =
  match Js.to_string re_select##.value with
  | "all" -> All
  | "sample" -> Sample
  | _ -> assert false

(** Prepare the page to show new instances. *)
let clear () =
  instances##.classList##remove (Js.string "is-hidden") ;
  pos_instances##.innerHTML := Js.string "" ;
  neg_instances##.innerHTML := Js.string "" ;
  ()

(** Push a new instance. *)
let push b s =
  let item = Dom_html.createLi Dom_html.document in
  item##.textContent := (Js.some @@ Js.string s) ;
  let parent = if b then pos_instances else neg_instances in
  let _ = Dom.appendChild parent item in
  ()

let handler _ _ =
  let s = re_input##.value in
  let mode = get_mode () in
  match parse @@ Js.to_string s with
  | Error `Not_supported | Error `Parse_error -> false
  | Ok re ->
    clear ();
    let pos_examples, neg_examples = gen_examples mode re in
    pos_examples (push true) ;
    neg_examples (push false) ;
    false
  
let () =
  let _listener =
    Dom_events.listen re_form Dom_events.Typ.submit handler
  in
  ()
