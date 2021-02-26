open Common;

type analysisType =
  | All
  | Dce
  | Exception
  | Noalloc
  | Termination;

let loadCmtFile = (~analysis, cmtFilePath) => {
  let cmt_infos = Cmt_format.read_cmt(cmtFilePath);

  let excludePath = sourceFile =>
    Cli.excludePaths^
    |> List.exists(prefix_ => {
         let prefix =
           Filename.is_relative(sourceFile)
             ? prefix_ : Filename.concat(Sys.getcwd(), prefix_);
         String.length(prefix) <= String.length(sourceFile)
         && (
           try(String.sub(sourceFile, 0, String.length(prefix)) == prefix) {
           | Invalid_argument(_) => false
           }
         );
       });
  switch (cmt_infos.cmt_annots |> FindSourceFile.cmt) {
  | Some(sourceFile) when !excludePath(sourceFile) =>
    if (Cli.debug^) {
      Log_.item(
        "Scanning %s Source:%s@.",
        Cli.ci^ && !Filename.is_relative(cmtFilePath)
          ? Filename.basename(cmtFilePath) : cmtFilePath,
        Cli.ci^ && !Filename.is_relative(sourceFile)
          ? sourceFile |> Filename.basename : sourceFile,
      );
    };
    FileReferences.addFile(sourceFile);
    currentSrc := sourceFile;
    currentModule := Paths.getModuleName(sourceFile);
    currentModuleName :=
      currentModule^
      |> Name.create(~isInterface=Filename.check_suffix(currentSrc^, "i"));

    switch (analysis) {
    | All =>
      cmt_infos |> DeadCode.processCmt(~cmtFilePath);
      cmt_infos |> Exception.processCmt;
      cmt_infos |> Arnold.processCmt;
    | Dce => cmt_infos |> DeadCode.processCmt(~cmtFilePath)
    | Exception => cmt_infos |> Exception.processCmt
    | Noalloc => cmt_infos |> Noalloc.processCmt
    | Termination => cmt_infos |> Arnold.processCmt
    };

  | _ => ()
  };
};

let runAnalysis = (~analysis, ~cmtRoot, ~ppf) => {
  Log_.Color.setup();
  let (+++) = Filename.concat;
  switch (cmtRoot) {
  | Some(root) =>
    let rec walkSubDirs = dir => {
      let absDir = dir == "" ? root : root +++ dir;
      let skipDir = {
        let base = Filename.basename(dir);
        base == "node_modules" || base == "_esy";
      };
      if (!skipDir && Sys.file_exists(absDir)) {
        if (Sys.is_directory(absDir)) {
          absDir |> Sys.readdir |> Array.iter(d => walkSubDirs(dir +++ d));
        } else if (Filename.check_suffix(absDir, ".cmt")
                   || Filename.check_suffix(absDir, ".cmti")) {
          absDir |> loadCmtFile(~analysis);
        };
      };
    };
    walkSubDirs("");

  | None =>
    Paths.setProjectRoot();
    let lib_bs = Suppress.projectRoot^ +++ "lib" +++ "bs";

    let sourceDirs =
      Paths.readSourceDirs(~configSources=None) |> List.sort(String.compare);
    sourceDirs
    |> List.iter(sourceDir => {
         let libBsSourceDir = Filename.concat(lib_bs, sourceDir);
         let files =
           switch (Sys.readdir(libBsSourceDir) |> Array.to_list) {
           | files => files
           | exception (Sys_error(_)) => []
           };
         let cmtFiles =
           files
           |> List.filter(x =>
                Filename.check_suffix(x, ".cmt")
                || Filename.check_suffix(x, ".cmti")
              );
         cmtFiles
         |> List.sort(String.compare)
         |> List.iter(cmtFile => {
              let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
              cmtFilePath |> loadCmtFile(~analysis);
            });
       });
  };
  let dce = () => {
    DeadException.forceDelayedItems();
    DeadOptionalArgs.forceDelayedItems();
    DeadCommon.reportDead(~checkOptionalArg=DeadOptionalArgs.check, ppf);
    DeadCommon.WriteDeadAnnotations.write();
  };
  switch (analysis) {
  | All =>
    dce();
    Exception.reportResults(~ppf);
    Arnold.reportResults(~ppf);
  | Dce => dce()
  | Exception => Exception.reportResults(~ppf)
  | Noalloc => Noalloc.reportResults(~ppf)
  | Termination => Arnold.reportResults(~ppf)
  };
  Log_.Stats.report();
  Log_.Stats.clear();
};

type cliCommand =
  | All(option(string))
  | Exception(option(string))
  | DCE(option(string))
  | Noalloc
  | NoOp
  | Termination(option(string));


[@raises exit]
let cli = () => {
  let cliCommand = ref(NoOp);
  let usage = "reanalyze version " ++ Version.version;
  [@raises exit]
  let versionAndExit = () => {
    print_endline(usage);
    exit(0);
  };
  [@raises exit]
  let rec printUsageAndExit = () => {
    Arg.usage(speclist, usage);
    exit(0);
  }
  [@raises exit]
  and setCliCommand = command => {
    if (cliCommand^ != NoOp) {
      printUsageAndExit();
    };
    cliCommand := command;
  }
  [@raises exit]
  and setAll = cmtRoot => {
    All(cmtRoot) |> setCliCommand;
  }
  [@raises exit]
  and setDCE = cmtRoot => {
    DCE(cmtRoot) |> setCliCommand;
  }
  and setDebug = () => {
    Cli.debug := true;
  }
  [@raises exit]
  and setException = cmtRoot => {
    Exception(cmtRoot) |> setCliCommand;
  }
  and setExperimental = () => {
    Common.Cli.experimental := true;
  }
  [@raises exit]
  and setNoalloc = () => {
    Noalloc |> setCliCommand;
  }
  and setSuppress = s => {
    let names = s |> String.split_on_char(',');
    Suppress.suppress := names @ Suppress.suppress^;
  }
  and setUnsuppress = s => {
    let names = s |> String.split_on_char(',');
    Suppress.unsuppress := names @ Suppress.unsuppress^;
  }
  and setWrite = () => {
    Common.Cli.write := true;
  }
  [@raises exit]
  and setTermination = cmtRoot => {
    Termination(cmtRoot) |> setCliCommand;
  }
  and setLiveNames = s => {
    let names = s |> String.split_on_char(',');
    Common.Cli.liveNames := names @ Common.Cli.liveNames.contents;
  }
  and setExcludePaths = s => {
    let paths = s |> String.split_on_char(',');
    Common.Cli.excludePaths := paths @ Common.Cli.excludePaths.contents;
  }
  and setLivePaths = s => {
    let paths = s |> String.split_on_char(',');
    Common.Cli.livePaths := paths @ Common.Cli.livePaths.contents;
  }
  and speclist = [
    ("-all", Arg.Unit(() => setAll(None)), "Run all the analyses."),
    (
      "-all-cmt",
      Arg.String(s => setAll(Some(s))),
      "root_path Run all the analyses for all the .cmt files under the root path",
    ),
    ("-ci", Arg.Unit(() => Cli.ci := true), "Internal flag for use in CI"),
    ("-dce", Arg.Unit(() => setDCE(None)), "Eperimental DCE"),
    ("-debug", Arg.Unit(setDebug), "Print debug information"),
    (
      "-dce-cmt",
      Arg.String(s => setDCE(Some(s))),
      "root_path Experimental DCE for all the .cmt files under the root path",
    ),
    (
      "-exception",
      Arg.Unit(() => setException(None)),
      "Experimental exception analysis",
    ),
    (
      "-exception-cmt",
      Arg.String(s => setException(Some(s))),
      "root_path Experimental exception analysis for all the .cmt files under the root path",
    ),
    (
      "-exclude-paths",
      Arg.String(s => setExcludePaths(s)),
      "comma-separated-path-prefixes Exclude from analysis files whose path has a prefix in the list",
    ),
    (
      "-experimental",
      Arg.Unit(setExperimental),
      "Turn on experimental analyses (this option is currently unused)",
    ),
    (
      "-live-names",
      Arg.String(s => setLiveNames(s)),
      "comma-separated-names Consider all values with the give names as live",
    ),
    (
      "-live-paths",
      Arg.String(s => setLivePaths(s)),
      "comma-separated-path-prefixes Consider all values whose path has a prefix in the list as live",
    ),
    ("-noalloc", Arg.Unit(setNoalloc), ""),
    (
      "-suppress",
      Arg.String(setSuppress),
      "comma-separated-path-prefixes Don't report on files whose path has a prefix in the list",
    ),
    (
      "-termination",
      Arg.Unit(() => setTermination(None)),
      "Experimental termination analysis",
    ),
    (
      "-termination-cmt",
      Arg.String(s => setTermination(Some(s))),
      "root_path Experimental termination analysis for all the .cmt files under the root path",
    ),
    (
      "-unsuppress",
      Arg.String(setUnsuppress),
      "comma-separated-path-prefixes Report on files whose path a prefix in the list, overriding -suppress (no-op if -suppress is not specified)",
    ),
    (
      "-version",
      Arg.Unit(versionAndExit),
      "Show version information and exit",
    ),
    (
      "--version",
      Arg.Unit(versionAndExit),
      "Show version information and exit",
    ),
    (
      "-write",
      Arg.Unit(setWrite),
      "Write @dead annotations directly in the source files",
    ),
  ];

  let ppf = Format.std_formatter;
  [@raises exit]
  let executeCliCommand = cliCommand =>
    switch (cliCommand) {
    | NoOp => printUsageAndExit()
    | All(cmtRoot) => runAnalysis(~analysis=All, ~cmtRoot, ~ppf)
    | DCE(cmtRoot) => runAnalysis(~analysis=Dce, ~cmtRoot, ~ppf)
    | Exception(cmtRoot) => runAnalysis(~analysis=Exception, ~cmtRoot, ~ppf)
    | Noalloc => runAnalysis(~analysis=Noalloc, ~cmtRoot=None, ~ppf)
    | Termination(cmtRoot) =>
      runAnalysis(~analysis=Termination, ~cmtRoot, ~ppf)
    };

  Arg.parse(speclist, print_endline, usage);

  executeCliCommand(cliCommand^);
};

[@raises exit]
cli();
