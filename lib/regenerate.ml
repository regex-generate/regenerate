type 'a regex = 'a Regex.t
module Regex = Regex
module Word = Word
module Segments = Segments

include Langgen

let parse = Parsing.parse
