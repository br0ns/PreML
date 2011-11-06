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
      fun all ? =
          do (fst, et) <- one
           ; e := tokenToString et
           ; case e of
               "<-" =>
               do (snd, _) <- one
                ; rest <- all
                ; return $ [ new "(" ]
                  @ snd @
                  [ new ") >>= (fn " ]
                  @ fst @
                  [ new "=>" ]
                  @ rest @
                  [ new ")" ]
               end
             | ":=" =>
               do (snd, _) <- one
                ; rest <- all
                ; return $ [ new "let val" ]
                  @ fst @
                  [ new "=" ]
                  @ snd @
                  [ new "in" ]
                  @ rest @
                  [ new "end" ]
               end
             | ";" =>
               do rest <- all
                ; return $ [ new "(" ]
                  @ fst @
                  [ new ") >>= (fn _ =>" ]
                  @ rest @
                  [ new ")" ]
               end
             | _ =>
               return fst
          end ?
    in
      do token "do"
       ; maybeWith <- do token "with"
                       ; tmonad <- any
                       ; monad := tokenToString tmonad
                       ; token ";"
                       ; return [ new $ "val op>>= = " ^
                                  monad ^
                                  ".>>="
                                , new $ "val return = " ^
                                  monad ^
                                  ".return" ]
                      end ||| return nil
       ; block <- all
       ; return $ [ new "let infix 0 >>=" ]
                  @ maybeWith @
                  [ new "in" ]
                  @ block @
                  [ new "end" ]
      end
    end ?

fun openFunctor ? =
    do token "open"
     ; func <- any
     ; tb <- token "("
     ; (ts, te) <- until $ token ")"
     ; tmpName := newName ()
     ; return $ [ new "local structure", tmpName, new "=" ]
                @ func :: tb :: ts @ te ::
                [ new "in open", tmpName, new "end" ]
    end ?

fun classes ? =
    do token "("
     ; fst <- any
     ; rest <- many do token ","
                     ; any
                    end
     ; token ")"
     ; return $ fst :: rest
    end ?

fun unClasses cls =
    let
      fun loop (t :: (ts as _ :: _)) = t :: new "," :: loop ts
        | loop t = t @ [new ")"]
    in
      new "(" :: loop cls
    end

fun extendExisting ? =
    do token "extend"
     ; str <- any
     ; token "as"
     ; cls <- classes
     ; (toks, tmps) :=
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
       end
     ; return $ [ new "local" ]
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
                , new "end" ]
    end ?

fun extendNew ? =
    do token "struct"
     ; cls <- classes
     ; (str, _) <- until $ token "end"
     ; tmp := newName ()
     ; return $ [ new "struct"
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
                , new "end" ]
    end ?

fun stripQuotes s = String.substring (s, 1, size s - 2);
fun isString s = size s > 0 andalso String.sub(s, 0) = #"\""

fun failWithPosition file source =
    do token "FailWithPosition"
     ; s <- any
     ; (p, _) := tokenSpan s
     ; {row = r, column = c} := Source.position source p
     ; s' := Path.toString file ^ "(" ^ Int.toString r ^ ":" ^ Int.toString c ^
             "): " ^ stripQuotes (tokenToString s)
     ; return [ new ("Fail \"" ^ s' ^ "\"") ]
    end

fun includeFile path =
    let
      fun readFile s =
          TextIO.readFile $ Path.toString $ Path.new' path $ stripQuotes s
      val strings = many1 $ (predicate isString o liftP tokenToString)
    in
      do token "include"
       ; singleline <- (token "singleline" produce true ||| return false)
       ; ss <- strings
       ; ss := map readFile ss
       ; ss := if singleline
               then map (String.map (fn #"\n" => #" " | c => c)) ss
               else ss
       ; return $ map new ss
      end
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

fun openFiltered ? =
    do token "open"
     ; vs <- vals
     ; str <- any
     ; return $ importVals str vs
    end ?

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
      fun filt con body =
          [ new "if" ]
          @ con @
          [ new "then" ]
          @ body @
          [ new "else List.nil" ]
      val one = until $ choice $ map token ["<-", ",", "]"]
      fun all (body, et) =
          if tokenToString et = "]" then
            return body
          else
            do (fst, et) <- one
             ; if tokenToString et = "<-" then
                 do (list, et) <- one
                  ; body' <- all (body, et)
                  ; return $ bind fst list body'
                 end
               else
                 do body' <- all (body, et)
                  ; return $ filt fst body'
                 end
            end
    in
      do token "["
       ; (exp, pipe) <- until $ choice $ map token ["|", "]"]
       ; if tokenToString pipe <> "|"
         then fail
         else all (new "[" :: exp @ [new "]"], pipe)
      end
    end ?


fun partTuples ? =
    do lp <- token "("
     ; rps := [new ")", new ")"]
     ; fnhead := fn v => [new "(fn", v, new "=>", lp]
     ; untilNext := until $ (try (token "," |-- (token "," ||| token ")"))
                             ||| (token ")" |-- fail))
     ; (do c <- token ","
         ; (rest, _) <- until $ token ")"
         ; v := newNameInScope rest
         ; return $ fnhead v @ [v, new ","] @ rest @ rps
        end |||
        do (l, et) <- untilNext
         ; if tokenToString et = ")"
           then
             do v := newNameInScope l
              ; return $ fnhead v @ l @ [new ",", v] @ rps
             end
           else
             do (r, _) <- until $ token ")"
              ; v := newNameInScope (l @ r)
              ; return $ fnhead v @ l @ [new ",", v, new ","] @ r @ rps
             end
        end
       )
    end ?

end
