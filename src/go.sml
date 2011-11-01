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
  val opt = ref
              {logLevel  = Log.Normal
             , isProject = NONE
             , src       = NONE
             , dst       = NONE
             , doClean   = false
              }

  fun setLogLevel l =
      case !opt of
        {logLevel, isProject, src, dst, doClean} =>
        opt := {logLevel = l
              , isProject = isProject
              , src = src
              , dst = dst
              , doClean = doClean}

  fun setIsProject p =
      case !opt of
        {logLevel, isProject, src, dst, doClean} =>
        opt := {logLevel = logLevel
              , isProject = SOME p
              , src = src
              , dst = dst
              , doClean = doClean}

  fun setDst d =
      case !opt of
        {logLevel, isProject, src, dst, doClean} =>
        opt := {logLevel = logLevel
              , isProject = isProject
              , src = src
              , dst = SOME d
              , doClean = doClean}

  fun setSrc s =
      case !opt of
        {logLevel, isProject, src, dst, doClean} =>
        case src of
          SOME s => raise Fail ("Only one source file allowed \
                                \(you already told me '" ^ s ^ "')")
        | _ =>
          opt := {logLevel = logLevel
                , isProject = isProject
                , src = SOME s
                , dst = dst
                , doClean = doClean}

  fun setDoClean c =
      case !opt of
        {logLevel, isProject, src, dst, doClean} =>
        opt := {logLevel = logLevel
              , isProject = isProject
              , src = src
              , dst = dst
              , doClean = c}
in
fun parseArgs args =
    let open Parser val op >>= = op --> infix >>> ||| --|
      val fileType =
          do token "project"
           ; return $ setIsProject true
          end |||
          do token "code"
           ; return $ setIsProject false
          end |||
          do t <- any
           ; raise Fail ("Unknown file type: " ^ t)
          end
      val one =
          do tok <- any
           ; case tok of
               "-o"        => any >>> setDst
             | "-t"        => fileType
             | "-c"        => return $ setDoClean true
             | "-q"        => return $ setLogLevel Log.Quiet
             | "-v"        => return $ setLogLevel Log.Chatty
             | "-vv"       => return $ setLogLevel Log.Verbose
             | "-vvv"      => return $ setLogLevel Log.Debug
             | "-?"        => printUsageAndExit ()
             | "-h"        => printUsageAndExit ()
             | "-help"     => printUsageAndExit ()
             | "--help"    => printUsageAndExit ()
             | "--version" => printVersionAndExt ()
             |  _          => return $ setSrc tok
          end
      val parse =
          do many' one
           ; eof
          end
    in
      if null args
      then printUsageAndExit ()
      else
        case Parse.list parse args of
          Right _ => !opt
        | Left ({token = SOME x, ...} :: _) =>
          raise Fail ("Could not parse command line arguments\
                      \ (failed on '" ^ x ^ "')")
        | Left _ =>
          raise Fail ("Could not parse command line arguments")
    end
end

fun go () =
    let
      val {logLevel, isProject, src, dst, doClean} =
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
          val dst' = Main.run src isProject dst
          fun log f = f ("Output written to '" ^ Main.shortest dst' ^ "'")
        in
          case dst of
            NONE   => log Log.normal
          | SOME _ => log Log.chatty
        end
    end
    handle Fail s => die s

val _ = go ()
