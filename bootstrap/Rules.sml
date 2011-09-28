structure Rules :> Rules =
struct

type 'x t = 'x Rewrite.rule

open Parser Rewrite Parsers
infix 0 |||
infix 1 --- |-- --| ^^^ ::: @@@
infix 2 >>> --> ??? produce

(* TODO: Remove this when Parser moves from --> to >>= *)
infix >>=
val op>>= = op-->

fun newName () = new $ "PreML__TMP__" ^ UniqId.next ()

fun doBlock ? =
    let
      val one = until $ choice $ map token ["<-", ":=", "end", ";"]
      fun all ? = ( ( 
                          one ) >>= (fn  (fst, et) => let val 
             e =  tokenToString et in 
             case e of
               "<-" => ( ( 
                              one ) >>= (fn  (snd, _) => ( 
                          all ) >>= (fn  rest => 
                  return $ [ new "(" ]
                  @ snd @
                  [ new ") >>= (fn " ]
                  @ fst @
                  [ new "=>" ]
                  @ rest @
                  [ new ")" ] ) ) ) 

             | ":=" => ( ( 
                              one ) >>= (fn  (snd, _) => ( 
                          all ) >>= (fn  rest => 
                  return $ [ new "let val" ]
                  @ fst @
                  [ new "=" ]
                  @ snd @
                  [ new "in" ]
                  @ rest @
                  [ new "end" ] ) ) ) 

             | ";" => ( ( 
                          all ) >>= (fn  rest => 
                  return $ [ new "(" ]
                  @ fst @
                  [ new ") >>= (fn _ =>" ]
                  @ rest @
                  [ new ")" ] ) ) 

             | _ =>
               return fst end ) ) 
              ?
    in ( ( 
         token "do" ) >>= (fn _ => ( ( 
            token "with" ) >>= (fn _ => ( 
                     any ) >>= (fn  monad => ( 
            token ";" ) >>= (fn _ => ( 
                     all ) >>= (fn  block => 
            return $ [ new "let open"
                     , monad
                     , new "infix >>="
                     , new "in" ]
                     @ block @
                     [ new "end" ] ) ) ) ) ) 
             ||| ( ( 
                     all ) >>= (fn  block => 
            return $ [ new "(" ]
                     @ block @
                     [ new ")" ] ) ) ) ) 


    end ?

fun openFunctor ? = ( ( 
       token "open" ) >>= (fn _ => ( 
               any ) >>= (fn  func => ( 
             token "(" ) >>= (fn  tb => ( 
                   until $ token ")" ) >>= (fn  (ts, te) => let val 
       tmpName =  newName () in 
       return $ [ new "local structure", tmpName, new "=" ]
                @ func :: tb :: ts @ te ::
                [ new "in open", tmpName, new "end" ] end ) ) ) ) ) 
        ?

fun classes ? = ( ( 
       token "(" ) >>= (fn _ => ( 
              any ) >>= (fn  fst => ( 
               many ( ( token "," ) >>= (fn _ => 
                       any ) ) ) >>= (fn  rest => ( 

       token ")" ) >>= (fn _ => 
       return $ fst :: rest ) ) ) ) ) 
        ?

fun unClasses cls =
    let
      fun loop (t :: (ts as _ :: _)) = t :: new "," :: loop ts
        | loop t = t @ [new ")"]
    in
      new "(" :: loop cls
    end

fun extendExisting ? = ( ( 
       token "extend" ) >>= (fn _ => ( 
              any ) >>= (fn  str => ( 
       token "as" ) >>= (fn _ => ( 
              classes ) >>= (fn  cls => let val 
       (toks, tmps) = 
       let
         fun loop tmps nil = (nil, tmps)
           | loop tmps (cl :: cls) =
             let
               val tmp = newName ()
               val (rest, tmps') = loop (tmp :: tmps) cls
             in
               ( [ new "structure"
                 , tmp
                 , new "="
                 , cl
                 , new "("
                 , new "open" ]
                 @ rev tmps @
                 [ str
                 , new ")" ]
                 @ rest
               , tmps')
             end
       in
         loop nil cls
       end in 
       return $ [ new "local" ]
                @ toks @
                [ new "in"
                , new "structure"
                , str
                , new "="
                , new "struct"
                , new "open" ]
                @ rev tmps @
                [ str
                , new "end"
                , new "end" ] end ) ) ) ) ) 
        ?

fun extendNew ? = ( ( 
       token "struct" ) >>= (fn _ => ( 
              classes ) >>= (fn  cls => ( 
                   until $ token "end" ) >>= (fn  (str, _) => let val 
       tmp =  newName () in 
       return $ [ new "struct"
                , new "local"
                , new "structure"
                , tmp
                , new "="
                , new "struct" ]
                @ str @
                [ new "end"
                , new "extend"
                , tmp
                , new "as" ]
                @ unClasses cls @
                [ new "in"
                , new "open"
                , tmp
                , new "end"
                , new "end" ] end ) ) ) ) 
        ?


end