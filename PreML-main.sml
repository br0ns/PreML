fun die e = (println e ; OS.Process.exit OS.Process.failure)

val args = CommandLine.arguments ()
val arg1 = case args of
             [arg] => arg
           | _ => die $ "usage: " ^ CommandLine.name () ^ " [file.sml]"

val path = Path.relative arg1
    handle _ => die $ "Can't create path from '" ^ arg1  ^ "'"

val (n, path') = PreML.run path

;println $ "Applied " ^ Int.toString n ^ " rules.";
;println $ "Output written to '" ^ Path.toString path' ^ "'";
