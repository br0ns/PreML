signature Parsers =
sig
  type token
  val until : (token, token, 'x) Parser.parser ->
              (token, token list * token, 'x) Parser.parser
end
