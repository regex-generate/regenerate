module Regex = Regex
module Word = Word
module Segments = Segments
  
type 'a regex = 'a Regex.t

let parse = Parsing.parse

include Langgen
