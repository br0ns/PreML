structure Parsers :> Parsers  where type token = Rewrite.token =
struct

open Parser Rewrite
infix 0 |||
infix 1 --- |-- --| ^^^ ::: @@@
infix 2 >>> --> ??? produce

(* TODO: Remove this when Parser moves from --> to >>= *)
infix >>=
val op>>= = op-->

structure TokMap = OrderedMapFn (type t = token
                                 val compare = compareTokens)

fun until e =
    let
      val nesting =
          TokMap.fromList
            [ (new "(", new ")")
            , (new "{", new "}")
            , (new "[", new "]")
            , (new "let", new "end")
            , (new "local", new "end")
            , (new "struct", new "end")
            , (new "abstype", new "end")
            , (new "sig", new "end")
            , (new "do", new "end")
            ]
      fun loop st =
          let
            fun endOr t f =
                case st of
                  e :: st' => if sameToken t e then
                                loop st'
                              else
                                f
                | _        => f
            fun beginOr t f =
                case TokMap.lookup nesting t of
                  SOME e => loop $ e :: st
                | NONE   => f
            val next = let infix 0 >>= in ( 
                        any ) >>= (fn  t => ( 
                              endOr t $ beginOr t $ loop st ) >>= (fn  (ts, e) => 
                   return (t :: ts, e) ) ) end 

          in
            if null st then let infix 0 >>= in ( 
                      e ) >>= (fn  t => 
                 return ([], t) ) end 

                ||| next
            else
              next
          end
    in
      loop nil
    end
end