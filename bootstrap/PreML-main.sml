fun die e = (println e ; OS.Process.exit OS.Process.failure)

val args = CommandLine.arguments ()
val target =
    case args of
      [target] => target
    | _ => die $ "usage: " ^ CommandLine.name () ^ "file"
(* ;OS.FileSys.mkDir tmpdir handle OS.SysErr _ => (); *)

val target = Path.relative target
    handle _ => die $ "Can't create path from '" ^ target  ^ "'"
val targetdir = Path.dir target

fun shortest path =
    let
      val rel = Path.path' targetdir path
      val abs = Path.path path
    in
      if size rel > size abs then
        abs
      else
        rel
    end

fun extension path =
    case Path.extension path of
      SOME ext => ext
    | NONE     => die $ "No file extension: " ^ shortest path

local
fun mk exts path = List.exists (extension path \< op=) exts
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
    println $ shortest path ^ " -> " ^ Int.toString n ^ " -> " ^
              shortest path'

fun new path = Path.new $ Path.base path ^ ".preml." ^ extension path
    (* Path.new' tmpdir $ UniqId.next () ^ "-" ^ Path.file path *)

fun run path =
    case check path of
      SOME path' => path'
    | NONE =>
      if isSML path then
        let
          val path' = new path
          val n = PreML.run path path'
        in
          record path path'
        ; printChanged (n, path, path')
        ; path'
        end
      else if isMLB path then
        let
          val path' = new path
          val (n, paths) = PreMLB.run run path path'
        in
          record path path'
        ; printChanged (n, path, path')
        ; path'
        end
      else
        die $ "Unknown file extension: " ^ Path.toString path

;run target;
