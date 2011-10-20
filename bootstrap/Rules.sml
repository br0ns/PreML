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
      fun all ? = let infix 0 >>= in ( 
                          one ) >>= (fn  (fst, et) => let val 
             e =  tokenToString et in 
             case e of
               "<-" => let infix 0 >>= in ( 
                              one ) >>= (fn  (snd, _) => ( 
                          all ) >>= (fn  rest => 
                  return $ [ new "(" ]
                  @ snd @
                  [ new ") >>= (fn " ]
                  @ fst @
                  [ new "=>" ]
                  @ rest @
                  [ new ")" ] ) ) end 

             | ":=" => let infix 0 >>= in ( 
                              one ) >>= (fn  (snd, _) => ( 
                          all ) >>= (fn  rest => 
                  return $ [ new "let val" ]
                  @ fst @
                  [ new "=" ]
                  @ snd @
                  [ new "in" ]
                  @ rest @
                  [ new "end" ] ) ) end 

             | ";" => let infix 0 >>= in ( 
                          all ) >>= (fn  rest => 
                  return $ [ new "(" ]
                  @ fst @
                  [ new ") >>= (fn _ =>" ]
                  @ rest @
                  [ new ")" ] ) end 

             | _ =>
               return fst end ) end 
              ?
    in let infix 0 >>= in ( 
         token "do" ) >>= (fn _ => ( let infix 0 >>= in ( 
                         token "with" ) >>= (fn _ => ( 
                                   any ) >>= (fn  tmonad => let val 
                         monad =  tokenToString tmonad in ( 
                         token ";" ) >>= (fn _ => 
                         return [ new $ "val op>>= = " ^
                                  monad ^
                                  ".>>="
                                , new $ "val return = " ^
                                  monad ^
                                  ".return" ] ) end ) ) end 
                          ||| return nil ) >>= (fn  maybeWith => ( 
                  all ) >>= (fn  block => 
         return $ [ new "let infix 0 >>=" ]
                  @ maybeWith @
                  [ new "in" ]
                  @ block @
                  [ new "end" ] ) ) ) end 

    end ?

fun openFunctor ? = let infix 0 >>= in ( 
       token "open" ) >>= (fn _ => ( 
               any ) >>= (fn  func => ( 
             token "(" ) >>= (fn  tb => ( 
                   until $ token ")" ) >>= (fn  (ts, te) => let val 
       tmpName =  newName () in 
       return $ [ new "local structure", tmpName, new "=" ]
                @ func :: tb :: ts @ te ::
                [ new "in open", tmpName, new "end" ] end ) ) ) ) end 
        ?

fun classes ? = let infix 0 >>= in ( 
       token "(" ) >>= (fn _ => ( 
              any ) >>= (fn  fst => ( 
               many let infix 0 >>= in ( token "," ) >>= (fn _ => 
                       any ) end ) >>= (fn  rest => ( 

       token ")" ) >>= (fn _ => 
       return $ fst :: rest ) ) ) ) end 
        ?

fun unClasses cls =
    let
      fun loop (t :: (ts as _ :: _)) = t :: new "," :: loop ts
        | loop t = t @ [new ")"]
    in
      new "(" :: loop cls
    end

fun extendExisting ? = let infix 0 >>= in ( 
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
                , new "end" ] end ) ) ) ) end 
        ?

fun extendNew ? = let infix 0 >>= in ( 
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
                , new "end" ] end ) ) ) end 
        ?

fun stripQuotes s = String.substring (s, 1, size s - 2);
fun isString s = size s > 0 andalso String.sub(s, 0) = #"\""

fun failWithPosition file source = let infix 0 >>= in ( 
       token "FailWithPosition" ) >>= (fn _ => ( 
            any ) >>= (fn  s => let val 
       (p, _) =  tokenSpan s in let val 
       {row = r, column = c} =  Source.position source p in let val 
       s' =  Path.toString file ^ "(" ^ Int.toString r ^ ":" ^ Int.toString c ^
             "): " ^ stripQuotes (tokenToString s) in 
       return [ new ("Fail \"" ^ s' ^ "\"") ] end end end ) ) end 


fun includeFile path =
    let
      fun readFile s =
          TextIO.readFile $ Path.toString $ Path.new' path $ stripQuotes s
    in let infix 0 >>= in ( 
         token "include" ) >>= (fn _ => ( 
              any ) >>= (fn  t => let val 
         s =  tokenToString t in 
         if isString s then
           return [ new $ readFile s ]
         else if s = "singleline" then let infix 0 >>= in ( 
                   any ) >>= (fn  t => let val 
              s =  tokenToString t in 
              return
                [ new $ String.map (fn #"\n" => #" " | c => c) $ readFile s ] end ) end 

         else
           fail end ) ) end 

    end
end