structure Token :> Token =
struct
type 'a t = (char, unit, 'a) Parser.parser

open Parser
infix 0 ||| infix 1 --- |-- --| ^^^ ::: infix 2 >>> --> ??? produce

(* TODO: Remove this when Parser moves from --> to >>= *)
infix >>=
val op>>= = op-->

fun mlb ? =
    (many1' $ predicate Char.isGraph) ?

fun sml ? =
    let
      val alphanumc = Text.oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                                 \abcdefghijklmnopqrstuvwxyz\
                                 \1234567890\
                                 \'_"
      val alphanum = many1' alphanumc
      val alphanumId = Text.letter |-- many' alphanumc
      val symc = void $ Text.oneOf "-!%&$+/:<=>?@~`^|#*\\"
      val symId = many1' symc
      val id = alphanumId ||| symId
      fun longId ? = (id --| maybe (Symb.dot |-- longId)) ?

      val num = many1' Text.digit
      val frac = Symb.dot |-- num
      val exp = Text.oneOf "eE" |-- maybe Symb.tilde |-- num

      val sign = void $ maybe Symb.tilde

      val realint = num |-- maybe frac |-- void $ maybe frac

      val hexdigit = void $ Text.oneOf "1234567890abcdefABCDEF"
      val hexnum = many1' hexdigit

      val hex = Text.string "0x" |-- hexnum
      val word = Text.string "0w" |-- (num ||| Text.char #"x" |-- hexnum)

      val number = (sign --| try realint ||| hex) ||| word

      val special =
          void $ Text.oneOf "_,{}[];()" ||| void $ Text.string "..."

      val tyvar = Symb.pling |-- alphanum

      val string =
          let
            val escape =
                void $ Text.oneOf "abfnrtv\\\"" |||
                void $ count 3 Text.digit |||
                Text.whitespace --| Symb.backslash
            fun loop ? =
                do c <- any
                 ; case c of
                     #"\"" => return ()
                   | #"\\" => escape |-- loop
                   | _     => loop
                end ?
          in
            Symb.quote |-- loop
          end

      val char =
          Symb.hash |-- string

      val comment =
          let
            fun loop 0 ? = return () ?
              | loop n ? =
                (try $ (Text.string "(*" |-- loop (n + 1)) |||
                 try $ (Text.string "*)" |-- loop (n - 1)) |||
                 any |-- loop n
                ) ?
          in
            try $ Text.string "(*" |-- loop 1
          end

      fun debug s p = p --> (fn x => (println (s ^ ": " ^ x) ; return x))
    in
      choice
        [ longId
        , number
        , tyvar
        , string
        , char
        , try comment
        , special
        ]
    end ?
end
