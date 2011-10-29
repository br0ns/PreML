structure PreMLProject :> PreMLProject =
struct
open Parser val op >>= = op -->


fun loop next pathi =
    let
      open Parser val op >>= = op -->
      open Rewrite
      val parse =
          do token "%pre"
           ; t <- any
           ; file := tokenToString t
           ; path := Path.new' (Path.dir pathi) file
           ; return [new $ Path.toString $ next path]
          end
      val src = Source.fromString $ TextIO.readFile $ Path.toString pathi
    in
      Rewrite.run Token.project src parse
    end

fun walk next pathi =
    let
      val (n, _) = loop (fn file => (next file ; file)) pathi
    in
      n
    end

fun run next pathi patho =
    let
      val (n, src) = loop next pathi
    in
      TextIO.writeFile (Path.toString patho) $ Source.toString src
    ; n
    end
end
