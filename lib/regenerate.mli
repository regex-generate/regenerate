(** Regenerate is a library to generate test cases for regular expression engines.

    Here is a typical use of the library, for creating a test harness
    with {!QCheck}.
{[
let test =
  (* The alphabet is [abc] *)
  let alphabet = ['a'; 'b'; 'c'] in

  (* Words are made of regular strings. *)
  let module Word = Regenerate.Word.String in

  (* Streams are made of ThunkLists. *)
  let module Stream = Regenerate.Segments.ThunkList(Word) in

  let generator =
    Regenerate.arbitrary
      (module Word)
      (module Stream)
      ~compl:false (* Do not generate complement operators. *)
      ~pp:Fmt.char (* Printer for characters. *)
      ~samples:100 (* We want on average 100 samples for each regex. *)
      alphabet
  in

  QCheck.Test.make generator check (* Test the [check] function. *)
]}
*)

type 'a regex = 'a Regex.t
(** The type of regular expressions on characters of type ['a]. *)

val arbitrary:
  (module Word.S with type char = 'char and type t = 'word) ->
  (module Segments.S with type elt = 'word) ->
  ?skip:int ->
  compl:bool ->
  pp:'char Fmt.t ->
  samples:int ->
  'char list ->
  ('char Regex.t * 'word list * 'word list) QCheck.arbitrary
(** [Regenerate.arbitrary (module W) (module S) ~compl ~pp ~samples alpha]
    creates a {!QCheck} generator that generates triples containing
    a {!regex} and a list of positive and negative samples.
    
    @param W is a module implementing operation on words. See {!Word} for some predefined modules.
    @param S is a module implementing a data-structure enumerating words. See {!Segments} for some predefined modules. We recommend {!Segments.ThunkList}.
    @param skip specifies how many samples should we skip on average. Default is [8].
    @param compl specifies if we generate regex containing the complement operator.
    @param pp specifies how to print individual characters.
    @param alpha describes the alphabet as a list of characters.
    
*)

val parse :
  string ->
  (char regex, [> `Not_supported | `Parse_error ]) result
(** [Regenerate.parse s] returns the associated {!regex}. 
    It recognizes the Posix Extended Regular Expression syntax plus complement ([~a]) and intersection ([a&b]).
    Character classes are not supported.
*)

module Regex = Regex
(** Definition of Regular expressions and associated utilities. *)

module Word = Word
(** Generic definitions of words on which regular expression can match. *)

module Segments = Segments
(** Streaming data-structures that will contain the generate samples. *)


(** {2:functor Functorial API}
    
    This API allows full access to generators and regular operators.
    For casual use of the library, consider using {!arbitrary} instead.
*)

module type SIGMA = sig
  type t
  val sigma : t
end

(** [Regenerate.Make(W)(S)(A)] is a module that implements
    sample generation for words implemented by the module [W] with the alphabet [A].
    [S] describes the data structure used internally for the enumeration of
    words.

    For casual use of the library, consider using {!arbitrary} instead.
*)
module Make
    (Word : Word.S)
    (Segment : Segments.S with type elt = Word.t)
    (Sigma : SIGMA with type t = Segment.t) : sig
  
  type node =
    | Nothing
    | Everything
    | Cons of Segment.t * lang
  and lang = unit -> node
  (** A language is a lazy stream of words segmented by growing length. *)

  val pp : Format.formatter -> lang -> unit
  (** [pp fmt lang] pretty print the language [lang]. *)

  val gen : Word.char Regex.t -> lang
  (** [gen regex] returns the language recognized by [regex]. *)

  (** {2 Sampling} *)

  type res =
    | Done
    | Finite
    | GaveUp
  exception ExitSample

  val sample :
    ?st:Random.State.t ->
    ?n:int ->
    ?firsts:int ->
    skip:int -> lang -> (Segment.elt -> unit) -> res
  (** [sample ~skip ~n lang] returns a sequence of on average [n] elements.
      [lang] is only consumed when needed. 

      We sample one element every [k], where [k] follows a power law of
      average [skip]. Furthermore, if we consume more than [sqrt k] empty segments,
      we assume that the rest of the segments will be infinitely empty and
      stop. 
      
      If [firsts] is provided, we always output the [firsts] first elements.
  *)

  (** {2 Operations on languages} *)

  val flatten : lang -> Segment.elt Sequence.t
  (** [flatten lang] returns the sequence of its segments. *)

  (** {3 Regular operations} *)

  val union : lang -> lang -> lang
  val inter : lang -> lang -> lang
  val difference : lang -> lang -> lang
  val compl : lang -> lang
  val concatenate : lang -> lang -> lang
  val star : (unit -> node) -> unit -> node
  val rep : int -> int option -> lang -> lang
  val charset : bool -> Word.char list -> unit -> node

end
