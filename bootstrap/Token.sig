(* Parsers that parse exactly one token *)
signature Token =
sig
  type 'a t = (char, unit, 'a) Parser.parser
  val project : 'a t
  val sml : 'a t
end
