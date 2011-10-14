structure PreMLB :> PreMLB =
struct

fun run loop pathI pathO =
    let open Path Parser Rewrite infix |-- -->
      val paths = ref []
      fun rule ? =
          (token "%pre" |-- any --> (fn t =>
           let
             val file = tokenToString t
             val path = Path.new' (Path.dir pathI) file
           in
             paths := path :: !paths
           ; return [new $ Path.toString $ loop path]
           end
                                    )
          ) ?

      val src = Source.fromString $ TextIO.readFile $ toString pathI
      val (n, src') = Rewrite.run Token.mlb src rule
    in
      TextIO.writeFile (toString pathO) $ Source.toString src'
    ; (n, !paths)
    end
end
