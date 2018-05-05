open Js_of_ocaml
open Regenerate

(* let alphabet = CCOpt.get_exn @@ Regex.enumerate ' ' '~' *)
let alphabet = ['a';'b']
module W = Word.String
module S = Segments.ThunkList(W)
let sigma = S.of_list @@ List.map W.singleton alphabet
module Sigma = struct type t = S.t let sigma = sigma end
module L = Make (W) (S) (Sigma)

(** Web part *)

let (!$) = Dom_html.getElementById
let ($$) id c = CCOpt.get_exn @@ Dom_html.getElementById_coerce id c

let instances = !$"instances"
let pos_instances = !$"pos-instances"
let neg_instances = !$"neg-instances"
let re_form = !$"re-form"
let fail_div = !$"fail"
let pos_msg = !$"pos-msg"
let neg_msg = !$"neg-msg"
let re_list = !$"re-list"

let re_input = "re-input" $$ Dom_html.CoerceTo.input
let mode_select = "mode-select" $$ Dom_html.CoerceTo.select

let createLicode s = 
  let itemLi = Dom_html.createLi Dom_html.document in
  let item = Dom_html.createCode Dom_html.document in
  item##.textContent := (Js.some @@ Js.string s) ;
  let _ = Dom.appendChild itemLi item in
  itemLi
  
type mode = All | Sample
let get_mode () =
  match Js.to_string mode_select##.value with
  | "all" -> All
  | "sample" -> Sample
  | _ -> assert false

(** Prepare the page to show new instances. *)
let clear_children s = s##.innerHTML := Js.string ""
let clear () =
  instances##.classList##remove (Js.string "is-hidden") ;
  List.iter clear_children [
    pos_instances ;
    neg_instances ;
    pos_msg ;
    neg_msg ;
    fail_div ;
  ];
  ()

(** Push a new instance. *)
let push b s =
  let elem = createLicode s in
  let parent = if b then pos_instances else neg_instances in
  let _ = Dom.appendChild parent elem in
  ()

(** On failure. *)
let fail s =
  instances##.classList##add (Js.string "is-hidden") ;
  let html =
    Fmt.strf {|<div class="callout small alert">%s</div>|} s
  in
  fail_div##.innerHTML := Js.string html ;
  ()  

(** When a stream is done, we show a message. *)
let show_note elem res = 
  let s = match res with
    | L.Done -> ""
    | L.GaveUp ->
      {|<p>I give up! It doesn't look like I will produce more strings for this 
        regular expression.
        Maybe the next string is very long, or maybe there isn't any more strings.
        If you want me to try harder, use the native version!</p>|}
    | L.Finite ->
      {|<p>That's it! 
        This regular expression recognizes a finite number of strings.</p>|}
  in
  elem##.innerHTML := Js.string s

(** Reimplementation of Sequence.take with an exit state. *)
exception ExitTake
let take n seq k =
  let count = ref 0 in
  try
    seq
      (fun x ->
        if !count = n then raise ExitTake;
        incr count;
        k x)
  with ExitTake -> L.Done

let st = Random.State.make_self_init ()
let gen mode re =
  let firsts, n = match mode with
    | Sample -> 5, 20
    | All -> 200, 200
  in
  let f l = take n @@ L.sample ~st ~firsts ~skip:5 l in
  let lang = L.gen re in

  let pos = f lang in
  let pos_res = pos (push true) in
  show_note pos_msg pos_res ;
  
  let neg = f @@ L.compl lang in
  let neg_res = neg (push false) in
  show_note neg_msg neg_res ;

  ()

let handler_generate _ _ =
  let s = re_input##.value in
  let mode = get_mode () in
  clear ();
  begin match parse @@ Js.to_string s with
    | Error `Not_supported -> fail "This feature is not supported."
    | Error `Parse_error -> fail "The parsing of your regular expression failed."
    | Ok re ->
      try gen mode re
      with exn ->
        Firebug.console##error (Js.string @@ Printexc.to_string exn)
  end;
  false


(** Regular expression list and generator. *)

let re_examples = [
  "(b(ab*a)*b|a)*";
  "(ab*)*";
  "~(a*)|a*";
  "~(a*)&a*";
  "(b*ab*ab*a)*b*";
]
let () =
  let add_re_to_list re =
    let handler _ _ =
      re_input##.value := Js.string re ;
      false
    in
    (* let elem = createLicode re in *)
    let button = Dom_html.createA Dom_html.document in
    button##setAttribute (Js.string "href") (Js.string "#") ;
    button##setAttribute (Js.string "data-close") (Js.string "") ;
    (* let _ = Dom.appendChild button elem in *)
    button##.textContent := Js.Opt.return (Js.string re) ;
    let _ = Dom_events.listen button Dom_events.Typ.click handler in
    let _ = Dom.appendChild re_list button in
    ()
  in
  List.iter add_re_to_list re_examples


let re_gen_button = !$"re-gen"
let handler_gen_re _ _ =
  let re =
    Regex.gen ~compl:true (QCheck.Gen.oneofl alphabet) st
  in
  re_input##.value :=
    Fmt.kstrf Js.string "%a@." (Regex.pp ~epsilon:false Fmt.char) re ;
  false


let () =
  ignore @@
  Dom_events.listen re_form Dom_events.Typ.submit handler_generate ;
  ignore @@
  Dom_events.listen re_gen_button Dom_events.Typ.click handler_gen_re ;
  ()
