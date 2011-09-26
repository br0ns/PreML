structure PreML :> PreML =
struct
fun run pathI pathO =
    let open Path
      val src = Source.fromString $ TextIO.readFile $ toString pathI
      val (n, src') = Rewrite.run Token.sml src Rules.doBlock
    in
      TextIO.writeFile (toString pathO) $ Source.toString src'
    ; n
    end
end
