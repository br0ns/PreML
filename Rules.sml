structure Rules :> Rules =
struct

type 'x t = 'x Rewrite.rule

open Parser Rewrite
infix 0 |||
infix 1 --- |-- --| ^^^ ::: @@@
infix 2 >>> --> ??? produce

structure TokMap = OrderedMapFn (type t = token
                                 val compare = compareTokens)
fun doBlock ? =
      let
        val dict = TokMap.fromList
                     [ (new "(", new ")")
                     , (new "{", new "}")
                     , (new "[", new "]")
                     , (new "let", new "end")
                     , (new "local", new "end")
                     , (new "do", new "end")
                     ]
        fun loop st =
            let
              val next =
                  any                                 --> (fn t =>
                  (
                  (case st of
                     e :: st' => if sameToken t e then
                                   loop st'
                                 else
                                   fail
                   | _        => fail
                  ) |||
                  (case TokMap.lookup dict t of
                     SOME e => loop $ e :: st
                   | NONE   => fail
                  ) |||
                  loop st
                  )                                   --> (fn (ts, e) =>
                  return (t :: ts, e)
                                                          ))
              val ender = choice $ map token ["<-", ":=", ";", "end"]
            in
              if null st then
                ender                                 --> (fn t =>
                return ([], t)
                                                          ) |||
                next
              else
                next
            end
        val one = loop []
        fun all ? =
            (
            one   --> (fn (fst, et) =>
            let
              val e = tokenToString et
            in
              if e = "<-" then
                one --> (fn (snd, _) =>
                all --> (fn rest =>
                return $ [ new "(" ]
                         @ snd @
                         [ new ") >>= (fn " ]
                         @ fst @
                         [ new "=>" ]
                         @ rest @
                         [ new ")" ]
                        ))
              else if e = ":=" then
                one --> (fn (snd, _) =>
                all --> (fn rest =>
                return $ [ new "let" ]
                         @ fst @
                         [ new "=" ]
                         @ snd @
                         [ new "in" ]
                         @ rest @
                         [ new "end" ]
                        ))
              else if e = ";" then
                all --> (fn rest =>
                return $ [ new "(" ]
                         @ fst @
                         [ new ") >>= (fn _ =>" ]
                         @ rest @
                         [ new ")" ]
                        )
              else
                return fst
              end
                      )
            ) ?
      in
        token "do" |--
        (token "with" |--
         (any --| token ";") --> (fn str =>
         all --> (fn block =>
         return $ [ new "let open"
                  , str
                  , new "infix >>="
                  , new "in" ]
                  @ block @
                  [ new "end" ]
                 ))
         ||| all
        )
      end ?

fun extendNew ? = fail ?
fun extendExisting ? = (fail ??? "extendExisting") ?
fun openFunctor ? = (fail ??? "openFunctor") ?


end
