fun die e = (println e ; OS.Process.exit OS.Process.failure)

val args = CommandLine.arguments ()
val arg1 = case args of
             [arg] => arg
           | _ => die $ "usage: " ^ CommandLine.name () ^ " [file]"

val path = Path.relative arg1
    handle _ => die $ "Can't create path from '" ^ arg1  ^ "'"
val dir = Path.dir path

(* ;OS.FileSys.mkDir "/tmp/preml" handle OS.SysErr _ => (); *)
(* fun new path = *)
(*     Path.new $ "/tmp/preml/" ^ UniqId.next () ^ ":::" ^ Path.file path *)

fun shortest path =
    let
      val rel = Path.path' dir path
      val abs = Path.path path
    in
      if size rel > size abs then
        abs
      else
        rel
    end

local
fun mk exts path =
    case Path.extension path of
      SOME ext => List.exists (ext \< op=) exts
    | NONE => die $ "No file extension: " ^ shortest path
in
val isSML = mk ["sml", "fun", "sig"]
val isMLB = mk ["mlb"]
end

local
  val pathmap = ref Path.Map.empty
in
fun check p = Path.Map.lookup (!pathmap) p
fun record p p' = Path.Map.update (!pathmap) (p, p')
end

fun printChanged (n, path, path') =
    println $ shortest path ^ " -> " ^
              shortest path' ^ ": " ^ Int.toString n

fun run path =
    case check path of
      SOME path' => path'
    | NONE =>
      if isSML path then
        let
          val path' = Path.append path ".premlfile.sml"
          (* val path' = new path *)
          val n = PreML.run path path'
        in
          record path path'
        ; printChanged (n, path, path')
        ; path'
        end
      else if isMLB path then
        let
          val path' = Path.append path ".premlfile.mlb"
          (* val path' = new path *)
          val (n, paths) = PreMLB.run run path path'
        in
          record path path'
        ; printChanged (n, path, path')
        ; path'
        end
      else
        die $ "Unknown file extension: " ^ Path.toString path

val main = run path
