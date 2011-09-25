structure PreML :> PreML =
struct
fun run pathI =
    let open Path
      val pathO = new $ base pathI ^ ".pre.sml"
      val src = Source.fromString $ TextIO.readFile $ toString pathI
      val (n, src') = Rewrite.run Token.sml src Rules.doBlock
    in
      TextIO.writeFile (toString pathO) $ Source.toString src'
    ; (n, pathO)
    end
end
