fun usage name =
    map println
        ["Usage: " ^ name ^ " [option] <file>"
       , ""
       , "Options:"
       , "  -o <file>"
       , "     Output to file <file>."
       , "  -t [project, code]"
       , "     Set type of input file. Occurences of '%pre <file>' in a project\
         \ are"
       , "     replaced by <file'>, where <file'> is the result of\
         \ preprocessing <file>."
       , "     The type of <file> and the path of <file'> is chosen\
         \ automatically."
       , "  -f"
       , "     Force preprocessing, even thoug a file hasn't changed. Useful\
         \ when if a"
       , "     includes other files that might change."
       , "  -c"
       , "     Clean up preprocessed files. If the -o option was used during"
       , "     preprocessing, it must be used again with the same target."
       , "  -q"
       , "     Print only warnings."
       , "  -v"
       , "     More output."
       , "  -vv"
       , "     Even more output."
       , "  -vvv"
       , "     Print debug information."
       , "  --version"
       , "     Print version number and exit."
       , "  -?, -h, -help, --help"
       , "     You've just read it."
        ]
fun printVersionAndExt () = (println ("preml version " ^ version)
                           ; OS.Process.exit OS.Process.success)
fun printUsageAndExit () = (usage $ CommandLine.name ()
                          ; OS.Process.exit OS.Process.success)
fun die e = (Log.warning e ; OS.Process.exit OS.Process.failure)

local
  val logLevel = ref Log.Normal
  val isProject = ref NONE
  val src = ref NONE
  val dst = ref NONE
  val doClean = ref false
  val doForce = ref false

  fun setLogLevel x = logLevel := x
  fun setIsProject x = isProject := SOME x
  fun setSrc x = case !src of
                   SOME s => raise Fail ("Only one source file allowed \
                                         \(you already told me '" ^ s ^ "')")
                 | NONE => src := SOME x
  fun setDst x = dst := SOME x
  fun setDoClean x = doClean := x
  fun setDoForce x = doForce := x
in
fun parseArgs args =
    let open Parser val op >>= = op --> infix >>> ||| --|
      val fileType = let infix 0 >>= in ( 
             token "project" ) >>= (fn _ => 
             return $ setIsProject true ) end 
              ||| let infix 0 >>= in ( 
             token "code" ) >>= (fn _ => 
             return $ setIsProject false ) end 
              ||| let infix 0 >>= in ( 
                  any ) >>= (fn  t => 
             raise Fail ("Unknown file type: " ^ t) ) end 

      val one = let infix 0 >>= in ( 
                    any ) >>= (fn  tok => 
             case tok of
               "-o"        => any >>> setDst
             | "-t"        => fileType
             | "-c"        => return $ setDoClean true
             | "-f"        => return $ setDoForce true
             | "-q"        => return $ setLogLevel Log.Quiet
             | "-v"        => return $ setLogLevel Log.Chatty
             | "-vv"       => return $ setLogLevel Log.Verbose
             | "-vvv"      => return $ setLogLevel Log.Debug
             | "-?"        => printUsageAndExit ()
             | "-h"        => printUsageAndExit ()
             | "-help"     => printUsageAndExit ()
             | "--help"    => printUsageAndExit ()
             | "--version" => printVersionAndExt ()
             |  _          => return $ setSrc tok ) end 

      val parse = let infix 0 >>= in ( 
             many' one ) >>= (fn _ => 
             eof ) end 

    in
      if null args
      then printUsageAndExit ()
      else
        case Parse.list parse args of
          Right _ => (!logLevel, !isProject, !src, !dst, !doClean, !doForce)
        | Left ({token = SOME x, ...} :: _) =>
          raise Fail ("Could not parse command line arguments\
                      \ (failed on '" ^ x ^ "')")
        | Left _ =>
          raise Fail ("Could not parse command line arguments")
    end
end

fun go () =
    let
      val (logLevel, isProject, src, dst, doClean, doForce) =
          parseArgs $ CommandLine.arguments ()
      val src =
          case src of
            NONE => raise Fail "No source file specified"
          | SOME src => Path.relative src
      val dst = Option.map Path.relative dst
      val _ = Main.setSrcDir $ Path.dir src
      val _ = Log.setLevel logLevel
    in
      if doClean
      then
        (Log.normal $ Main.shortest src
       ; Log.indent 2
       ; Main.clean src dst
       ; Log.indent ~2
        )
      else
        let
          val dst' = Main.run doForce src isProject dst
          fun log f = f ("Output written to '" ^ Main.shortest dst' ^ "'")
        in
          case dst of
            NONE   => log Log.normal
          | SOME _ => log Log.chatty
        end
    end
    handle Fail s => die s

val _ = go ()