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
                       ; monad <- any
                       ; token ";"
                       ; return [new $ "val op>>= = " ^
                                 tokenToString monad ^
                                 ".>>="]
                      end ||| return nil
       ; block <- all
       ; return $ [ new "let infix 0 >>=" ]
                  @ maybeWith @
                  , new "in" ]
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


end
