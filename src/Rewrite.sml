structure Rewrite :> Rewrite =
struct
open Parser
infix 0 |||
infix 1 --- |-- --| ^^^ ::: @@@
infix 2 >>> --> ??? produce underlies

(* a chunk of source code is either unchanged (Old) in which case it has a
 * start and an ending position (pos * pos) in the source code, or it is
 * changed to something else (New) in which case it is just a string
 *)
datatype token =
         Old of int * (Source.t * Source.span)
       | New of string

type phantom = token list
(* type phantom = Source.t *)

type 'x rule = (token, token list, 'x) Parser.parser

fun tokenToString t =
    case t of
      Old (n, s) => Source.span s
    | New s => s

fun tokenSpan t =
    case t of
      Old (_, (_, s)) => s
    | New _ => raise FailWithPosition "`New` token has no span"

fun compareTokens a b = String.compare (tokenToString a, tokenToString b)
fun sameToken a b = compareTokens a b = EQUAL

fun run tok src rule =
    let
      val num = ref 0
      val tok = Lex.lexeme (
                getPosition -->        (fn l =>
                tok -->                (fn _ =>
                getPosition -->        (fn r =>
                return $ Old (inc num, (src, (l, r)))
                                       )))
                )

      val num = ref 0
      fun loop ? =
          (rule                           --> (fn ts =>
           loop                           --> (fn (rest, _) =>
           (inc num ; return (ts @ rest, true))
                                              )) |||
           any                            --> (fn t =>
           loop                           --> (fn (rest, b) =>
           return (t :: rest, b)
                                              )) |||
           eof produce (nil, false)
          ) ?

      fun fix toks =
          let
            val (toks', changed) =
                Parser.test
                  tokenToString
                  loop
                  List.getItem
                  toks
          in
            if changed then
              fix toks'
            else
              toks'
          end
      fun render toks =
          let
            open Layout infix \\ \ & && ^^
            val ls = map Substring.size
                         $ Substring.fields (#"\n" \< op=)
                         $ Substring.full
                         $ Source.toString src
            fun coord p =
                let
                  fun loop (l :: ls) x y =
                      if l < x then
                        (* Remember to subtract one for the newline *)
                        loop ls (x - l - 1) (y + 1)
                      else
                        {column = x, row = y}
                    | loop _ _ _ =
                      raise Domain
                in
                  loop ls p 0
                (* ; {row = 15, column = 40} *)
                end
            fun coalesce toks =
                case toks of
                  Old (n, x as (src, (l, _))) ::
                  Old (m, y as (_, (_, r))) :: rest =>
                  if n + 1 = m then
                    coalesce $ Old (m, (src, (l, r))) :: rest
                  else
                    Old (n, x) :: coalesce (Old (m, y) :: rest)
                | t :: ts => t :: coalesce ts
                | _ => nil
            fun one (New s) = txt s
              | one (Old (_, sp as (_, (p, _)))) =
                placeAt (coord p) $ txt $ Source.span sp
          in
            hsep $ map one
                 $ coalesce
                 toks
          end


      val toks =
          Parser.test
            Char.toString
            (Text.whitespace |-- many tok)
            Source.read
            src
      val src' = Source.fromString $ Layout.pretty NONE $ render $ fix toks
    in
      (!num, src')
    end

fun token s ? = predicate (fn Old (_, x) => Source.span x = s
                            | New x      => x = s) ?

val new = New
end
