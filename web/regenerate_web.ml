open Js_of_ocaml
open Regenerate

let alphabet = ['a'; 'b'; 'c']
module W = Word.String
module S = Segments.ThunkList(W)
let sigma = S.of_list @@ List.map W.singleton alphabet
module Sigma = struct type t = S.t let sigma = sigma end
module L = Make (W) (S) (Sigma)

let instances = Dom_html.getElementById "instances"
let pos_instances = Dom_html.getElementById "pos-instances"
let neg_instances = Dom_html.getElementById "neg-instances"
let generate_button = Dom_html.getElementById "gen-button"

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

let text_field =
  CCOpt.get_exn @@ Dom_html.getElementById_coerce "re-input" Dom_html.CoerceTo.input
let handler _ _ =
  let s = text_field##.value in
  match parse @@ Js.to_string s with
  | Error `Not_supported | Error `Parse_error -> true
  | Ok re ->
    clear ();
    let lang = L.gen re in
    let f = L.sample ~skip:5 ~n:20 in
    let pos_examples = f lang in
    let neg_examples = f @@ L.compl lang in
    pos_examples (push true) ;
    neg_examples (push false) ;
    true
  
let () =
  let _listener =
    Dom_events.listen generate_button Dom_events.Typ.click handler
  in
  ()
