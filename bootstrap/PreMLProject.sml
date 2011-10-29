structure PreMLProject :> PreMLProject =
struct
open Parser val op >>= = op -->


fun loop next pathi =
    let
      open Parser val op >>= = op -->
      open Rewrite
      val parse = let infix 0 >>= in ( 
             token "%pre" ) >>= (fn _ => ( 
                  any ) >>= (fn  t => let val 
             file =  tokenToString t in let val 
             path =  Path.new' (Path.dir pathi) file in 
             return [new $ Path.toString $ next path] end end ) ) end 

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