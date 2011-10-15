signature Rewrite =
sig

  type token
  type 'x rule = (token, token list, 'x) Parser.parser
  type phantom

  (* reports number of rule applications *)
  val run : Source.t Token.t -> Source.t -> phantom rule -> int * Source.t
  val token : string -> (token, token, 'x) Parser.parser
  val new : string -> token
  val compareTokens : token -> token -> order
  val sameToken : token -> token -> bool
  val tokenToString : token -> string
  val tokenSpan : token -> Source.span
end
