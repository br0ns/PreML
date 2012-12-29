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
fun newNameInScope toks =
    let
      val cs = "xyzabcdefghijklmpqrstuvw"
      val n = size cs
      val first = [0]
      val toString = implode o List.map (curry String.sub cs)
      val rec next = fn nil => [0]
                      | c :: cs => if c = n
                                   then 0 :: next cs
                                   else (c + 1) :: cs
      fun loop v =
          if List.exists (fn t => tokenToString t = toString v) toks
          then loop $ next v
          else new $ toString v
    in
      loop first
    end

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

fun failHere file source = let infix 0 >>= in ( 
       token "FailHere" ) >>= (fn _ => ( 
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
      val strings = many1 $ (predicate isString o liftP tokenToString)
    in let infix 0 >>= in ( 
         token "include" ) >>= (fn _ => ( 
                       (token "singleline" produce true ||| return false) ) >>= (fn  singleline => ( 
               strings ) >>= (fn  ss => let val 
         ss =  map readFile ss in let val 
         ss =  if singleline
               then map (String.map (fn #"\n" => #" " | c => c)) ss
               else ss in 
         return $ map new ss end end ) ) ) end 

    end

val vals = classes
fun importVals str =
    List.concat
    o map (fn v =>
              let
                val lhs = tokenToString v
                val rhs = tokenToString str ^ "." ^ lhs
              in
                [ new "val"
                , new lhs
                , new "="
                , new rhs
                ]
              end
          )

fun openFiltered ? = let infix 0 >>= in ( 
       token "open" ) >>= (fn _ => ( 
             vals ) >>= (fn  vs => ( 
              any ) >>= (fn  str => 
       return $ importVals str vs ) ) ) end 
        ?

fun listComp ? =
    let
      fun bind pat list body =
          [ new "(List.concat (List.map (fn" ]
          @ pat @
          [ new "=>" ]
          @ body @
          [ new ")(" ]
          @ list @
          [ new ")))" ]
      fun letb pat expr body =
          [ new "let val" ]
          @ pat @
          [ new "=" ]
          @ expr @
          [ new "in" ]
          @ body @
          [ new "end"]
      fun filt con body =
          [ new "if" ]
          @ con @
          [ new "then" ]
          @ body @
          [ new "else List.nil" ]
      val one = until $ choice $ map token ["<-", ",", ":=", "]"]
      fun all (body, et) =
          if tokenToString et = "]" then
            return body
          else let infix 0 >>= in ( 
                            one ) >>= (fn  (fst, et) => 
               if tokenToString et = "<-" then let infix 0 >>= in ( 
                                  one ) >>= (fn  (list, et) => ( 
                             all (body, et) ) >>= (fn  body' => 
                    return $ bind fst list body' ) ) end 

               else if tokenToString et = ":=" then let infix 0 >>= in ( 
                                  one ) >>= (fn  (expr, et) => ( 
                             all (body, et) ) >>= (fn  body' => 
                    return $ letb fst expr body' ) ) end 

               else let infix 0 >>= in ( 
                             all (body, et) ) >>= (fn  body' => 
                    return $ filt fst body' ) end ) end 


    in let infix 0 >>= in ( 
         token "[" ) >>= (fn _ => ( 
                        until $ choice $ map token ["|", "]"] ) >>= (fn  (exp, pipe) => 
         if tokenToString pipe <> "|"
         then fail
         else all (new "[" :: exp @ [new "]"], pipe) ) ) end 

    end ?


fun partTuples ? = let infix 0 >>= in ( 
             token "(" ) >>= (fn  lp => let val 
       rps =  [new ")", new ")"] in let val 
       fnhead =  fn v => [new "(fn", v, new "=>", lp] in let val 
       untilNext =  until $ (try (token "," |-- (token "," ||| token ")"))
                             ||| (token ")" |-- fail)) in 
       ( let infix 0 >>= in ( token "," ) >>= (fn  c => ( 
                        until $ token ")" ) >>= (fn  (rest, _) => let val 
           v =  newNameInScope rest in 
           return $ fnhead v @ [v, new ","] @ rest @ rps end ) ) end 
            ||| let infix 0 >>= in ( 
                      untilNext ) >>= (fn  (l, et) => 
           if tokenToString et = ")"
           then let infix 0 >>= in let val 
                v =  newNameInScope l in 
                return $ fnhead v @ l @ [new ",", v] @ rps end end 

           else let infix 0 >>= in ( 
                          until $ token ")" ) >>= (fn  (r, _) => let val 
                v =  newNameInScope (l @ r) in 
                return $ fnhead v @ l @ [new ",", v, new ","] @ r @ rps end ) end ) end 


       ) end end end ) end 
        ?

fun lazy ? =
    let
      val expr = let infix 0 >>= in ( 
                  token "(" ) >>= (fn  _ => ( 
                          until $ token ")" ) >>= (fn  (body, _) => 
             return body ) ) end 
              ||| let infix 0 >>= in ( 
                  any ) >>= (fn  t => 
             return [t] ) end 

      fun rewrite a b = let infix 0 >>= in ( 
             token a ) >>= (fn _ => ( 
                     expr ) >>= (fn  body => 
             return $ map new [b, "(", "fn", "_", "=>", "("] @ body @
             [new ")", new ")"] ) ) end 

      fun replace a b = let infix 0 >>= in ( 
             token a ) >>= (fn _ => 
             return [new b] ) end 

    in
      choice
        [ rewrite "L" "lazy"
        , rewrite "D" "delay"
        , replace "F" "force"
        , replace "E" "eager"
        ]
    end ?

end