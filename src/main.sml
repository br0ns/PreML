structure Main =
struct
val srcdir = ref $ Path.relative "."
fun setSrcDir path = srcdir := path
fun shortest path =
    let
      val rel = Path.path' (!srcdir) path
      val abs = Path.path path
    in
      if size rel > size abs then
        abs
      else
        rel
    end

fun new path =
    case Path.extension path of
      NONE   => Path.new (Path.toString path ^ ".preml")
    | SOME e => Path.new (Path.base path ^ ".preml." ^ e)

fun extOneOf p es =
    case Path.extension p of
      SOME e => List.exists (e \< op=) es
    | NONE   => false

fun isProj file = extOneOf file ["mlb", "cm"]

fun failIO desc name =
    raise Fail (desc ^ ": '" ^ shortest (Path.new name) ^ "'")

fun run force filei proj fileo =
    let open Time OS.FileSys
      val fileo = case fileo of
                    SOME fileo => fileo
                  | NONE =>
                    let
                      val fileo = new filei
                    in
                      Log.verbose ("No target for '" ^ shortest filei ^
                                   "', using '" ^ shortest fileo ^ "'")
                    ; fileo
                    end
      val proj = case proj of
                   SOME proj => proj
                 | NONE =>
                   let
                     val proj = isProj filei
                     val typ = if proj
                               then "project"
                               else "code"
                   in
                     Log.verbose
                       ("No file type specified, automagically choosing '" ^
                        typ ^ "'")
                   ; proj
                   end
      val doRun =
          proj orelse force orelse
          modTime (Path.toString filei) > modTime (Path.toString fileo)
          handle _ => true
      val prep = if proj
                 then PreMLProject.run $ runDefault force
                 else PreML.run
    in
      Log.normal $ shortest filei
    ; Log.indent 2
    ; (if not doRun
       then Log.chatty "Hasn't changed, skipping"
       else
         let
           val n = prep filei fileo
           fun changes sone smany =
               Log.normal (shortest fileo ^ ": " ^
                           (if n = 1
                            then "1 " ^ sone
                            else Int.toString n ^ " " ^ smany)
                          )
           val _ =
               if proj
               then changes "file path changed" "file paths changed"
               else changes "change" "changes"
           val _ = if n = 0
                   then Log.warning ("No changes to '" ^ shortest filei ^
                                     "', consider turning off preprocessing")
                   else ()
         in
           ()
         end)
    ; Log.indent ~2
    ; fileo
    end handle IO.Io {cause = OS.SysErr (desc, _), name, ...} =>
               failIO desc name

(* and runDefault filei = run filei NONE NONE *)
and runDefault force filei =
    run force filei (SOME $ isProj filei) (SOME $ new filei)

fun clean filei fileo =
    let
      val fileo = getOpt (fileo, new filei)
      fun remove file =
          if File.exists file
          then (Log.normal ("Removing '" ^ shortest file ^ "'")
              ; OS.FileSys.remove (Path.toString file)
                handle OS.SysErr _ => Log.warning "  Couldn't remove"
               )
          else ()
    in
      if isProj filei
      then (PreMLProject.walk (fn f => clean f NONE) filei
          ; remove fileo)
      else remove fileo
    end
end
