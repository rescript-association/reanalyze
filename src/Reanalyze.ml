open Common

let loadCmtFile ~(runConfig : RunConfig.t) cmtFilePath =
  let cmt_infos = CL.Cmt_format.read_cmt cmtFilePath in
  let excludePath sourceFile =
    !Cli.excludePaths
    |> List.exists (fun prefix_ ->
           let prefix =
             match Filename.is_relative sourceFile with
             | true -> prefix_
             | false -> Filename.concat (Sys.getcwd ()) prefix_
           in
           String.length prefix <= String.length sourceFile
           &&
           try String.sub sourceFile 0 (String.length prefix) = prefix
           with Invalid_argument _ -> false)
  in
  match cmt_infos.cmt_annots |> FindSourceFile.cmt with
  | Some sourceFile when not (excludePath sourceFile) ->
    if !Cli.debug then
      Log_.item "Scanning %s Source:%s@."
        (match !Cli.ci && not (Filename.is_relative cmtFilePath) with
        | true -> Filename.basename cmtFilePath
        | false -> cmtFilePath)
        (match !Cli.ci && not (Filename.is_relative sourceFile) with
        | true -> sourceFile |> Filename.basename
        | false -> sourceFile);
    FileReferences.addFile sourceFile;
    currentSrc := sourceFile;
    currentModule := Paths.getModuleName sourceFile;
    currentModuleName :=
      !currentModule
      |> Name.create ~isInterface:(Filename.check_suffix !currentSrc "i");
    if runConfig.dce then cmt_infos |> DeadCode.processCmt ~cmtFilePath;
    if runConfig.exception_ then cmt_infos |> Exception.processCmt;
    if runConfig.noalloc then cmt_infos |> Noalloc.processCmt;
    if runConfig.termination then cmt_infos |> Arnold.processCmt
  | _ -> ()

let processCmtFiles ~cmtRoot ~runConfig =
  let ( +++ ) = Filename.concat in
  match cmtRoot with
  | Some root ->
    Cli.cmtCommand := true;
    let rec walkSubDirs dir =
      let absDir = match dir = "" with true -> root | false -> root +++ dir in
      let skipDir =
        let base = Filename.basename dir in
        base = "node_modules" || base = "_esy"
      in
      if (not skipDir) && Sys.file_exists absDir then
        if Sys.is_directory absDir then
          absDir |> Sys.readdir |> Array.iter (fun d -> walkSubDirs (dir +++ d))
        else if
          Filename.check_suffix absDir ".cmt"
          || Filename.check_suffix absDir ".cmti"
        then absDir |> loadCmtFile ~runConfig
    in
    walkSubDirs ""
  | None ->
    Paths.setProjectRoot ();
    let lib_bs = !Suppress.projectRoot +++ ("lib" +++ "bs") in
    let sourceDirs =
      Paths.readSourceDirs ~configSources:None |> List.sort String.compare
    in
    sourceDirs
    |> List.iter (fun sourceDir ->
           let libBsSourceDir = Filename.concat lib_bs sourceDir in
           let files =
             match Sys.readdir libBsSourceDir |> Array.to_list with
             | files -> files
             | exception Sys_error _ -> []
           in
           let cmtFiles =
             files
             |> List.filter (fun x ->
                    Filename.check_suffix x ".cmt"
                    || Filename.check_suffix x ".cmti")
           in
           cmtFiles |> List.sort String.compare
           |> List.iter (fun cmtFile ->
                  let cmtFilePath = Filename.concat libBsSourceDir cmtFile in
                  cmtFilePath |> loadCmtFile ~runConfig))

let runAnalysis ~runConfig ~cmtRoot ~ppf =
  Log_.Color.setup ();
  processCmtFiles ~cmtRoot ~runConfig;
  if runConfig.dce then (
    DeadException.forceDelayedItems ();
    DeadOptionalArgs.forceDelayedItems ();
    DeadCommon.reportDead ~checkOptionalArg:DeadOptionalArgs.check ppf;
    DeadCommon.WriteDeadAnnotations.write ());
  if runConfig.exception_ then Exception.reportResults ~ppf;
  if runConfig.noalloc then Noalloc.reportResults ~ppf;
  if runConfig.termination then Arnold.reportResults ~ppf;
  Log_.Stats.report ();
  Log_.Stats.clear ()

type cliCommand =
  | All of string option
  | Config
  | Exception of string option
  | DCE of string option
  | Noalloc
  | NoOp
  | Termination of string option

let cli () =
  let cliCommand = ref NoOp in
  let usage = "reanalyze version " ^ Version.version in
  let versionAndExit () =
    print_endline usage;
    exit 0
    [@@raises exit]
  in
  let rec printUsageAndExit () =
    Arg.usage speclist usage;
    exit 0
    [@@raises exit]
  and setCliCommand command =
    if !cliCommand <> NoOp then printUsageAndExit ();
    cliCommand := command
    [@@raises exit]
  and setAll cmtRoot = All cmtRoot |> setCliCommand [@@raises exit]
  and setConfig () = Config |> setCliCommand
  and setDCE cmtRoot = DCE cmtRoot |> setCliCommand [@@raises exit]
  and setDebug () = Cli.debug := true
  and setException cmtRoot = Exception cmtRoot |> setCliCommand [@@raises exit]
  and setExperimental () = Common.Cli.experimental := true
  and setNoalloc () = Noalloc |> setCliCommand [@@raises exit]
  and setSuppress s =
    let names = s |> String.split_on_char ',' in
    Suppress.suppress := names @ !Suppress.suppress
  and setUnsuppress s =
    let names = s |> String.split_on_char ',' in
    Suppress.unsuppress := names @ !Suppress.unsuppress
  and setWrite () = Common.Cli.write := true
  and setTermination cmtRoot =
    Termination cmtRoot |> setCliCommand
    [@@raises exit]
  and setLiveNames s =
    let names = s |> String.split_on_char ',' in
    Common.Cli.liveNames := names @ Common.Cli.liveNames.contents
  and setExcludePaths s =
    let paths = s |> String.split_on_char ',' in
    Common.Cli.excludePaths := paths @ Common.Cli.excludePaths.contents
  and setLivePaths s =
    let paths = s |> String.split_on_char ',' in
    Common.Cli.livePaths := paths @ Common.Cli.livePaths.contents
  and speclist =
    [
      ("-all", Arg.Unit (fun () -> setAll None), "Run all the analyses.");
      ( "-all-cmt",
        Arg.String (fun s -> setAll (Some s)),
        "root_path Run all the analyses for all the .cmt files under the root \
         path" );
      ("-ci", Arg.Unit (fun () -> Cli.ci := true), "Internal flag for use in CI");
      ("-config", Arg.Unit setConfig, "Read the analysis mode from bsconfig.json");
      ("-dce", Arg.Unit (fun () -> setDCE None), "Eperimental DCE");
      ("-debug", Arg.Unit setDebug, "Print debug information");
      ( "-dce-cmt",
        Arg.String (fun s -> setDCE (Some s)),
        "root_path Experimental DCE for all the .cmt files under the root path"
      );
      ( "-exception",
        Arg.Unit (fun () -> setException None),
        "Experimental exception analysis" );
      ( "-exception-cmt",
        Arg.String (fun s -> setException (Some s)),
        "root_path Experimental exception analysis for all the .cmt files \
         under the root path" );
      ( "-exclude-paths",
        Arg.String (fun s -> setExcludePaths s),
        "comma-separated-path-prefixes Exclude from analysis files whose path \
         has a prefix in the list" );
      ( "-experimental",
        Arg.Unit setExperimental,
        "Turn on experimental analyses (this option is currently unused)" );
      ( "-externals",
        Arg.Unit (fun () -> DeadCommon.Config.analyzeExternals := true),
        "Report on externals in dead code analysis" );
      ( "-live-names",
        Arg.String (fun s -> setLiveNames s),
        "comma-separated-names Consider all values with the given names as live"
      );
      ( "-live-paths",
        Arg.String (fun s -> setLivePaths s),
        "comma-separated-path-prefixes Consider all values whose path has a \
         prefix in the list as live" );
      ("-noalloc", Arg.Unit setNoalloc, "");
      ( "-suppress",
        Arg.String setSuppress,
        "comma-separated-path-prefixes Don't report on files whose path has a \
         prefix in the list" );
      ( "-termination",
        Arg.Unit (fun () -> setTermination None),
        "Experimental termination analysis" );
      ( "-termination-cmt",
        Arg.String (fun s -> setTermination (Some s)),
        "root_path Experimental termination analysis for all the .cmt files \
         under the root path" );
      ( "-unsuppress",
        Arg.String setUnsuppress,
        "comma-separated-path-prefixes Report on files whose path has a prefix \
         in the list, overriding -suppress (no-op if -suppress is not \
         specified)" );
      ("-version", Arg.Unit versionAndExit, "Show version information and exit");
      ("--version", Arg.Unit versionAndExit, "Show version information and exit");
      ( "-write",
        Arg.Unit setWrite,
        "Write @dead annotations directly in the source files" );
    ]
  in
  let ppf = Format.std_formatter in
  let executeCliCommand cliCommand =
    match cliCommand with
    | NoOp -> printUsageAndExit ()
    | All cmtRoot -> runAnalysis ~runConfig:RunConfig.all ~cmtRoot ~ppf
    | Config ->
      runAnalysis ~runConfig:RunConfig.fromBsconfig ~cmtRoot:None ~ppf
    | DCE cmtRoot -> runAnalysis ~runConfig:RunConfig.dce ~cmtRoot ~ppf
    | Exception cmtRoot ->
      runAnalysis ~runConfig:RunConfig.exception_ ~cmtRoot ~ppf
    | Noalloc -> runAnalysis ~runConfig:RunConfig.noalloc ~cmtRoot:None ~ppf
    | Termination cmtRoot ->
      runAnalysis ~runConfig:RunConfig.termination ~cmtRoot ~ppf
    [@@raises exit]
  in
  Arg.parse speclist print_endline usage;
  executeCliCommand !cliCommand
  [@@raises exit]
;;

cli () [@@raises exit]
