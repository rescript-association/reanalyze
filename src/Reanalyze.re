open Common;

type analysisType =
  | All
  | Dce
  | Exception
  | Termination;

let loadCmtFile = (~analysis, cmtFilePath) => {
  if (debug^) {
    Log_.item(
      "Scanning %s@.",
      ci^ ? Filename.basename(cmtFilePath) : cmtFilePath,
    );
  };

  let cmt_infos = Cmt_format.read_cmt(cmtFilePath);

  switch (cmt_infos.cmt_annots |> FindSourceFile.cmt) {
  | None => ()

  | Some(sourceFile) =>
    FileHash.addFile(fileReferences, sourceFile);
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
    | Termination => cmt_infos |> Arnold.processCmt
    };
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

    let sourceDirs = Paths.readSourceDirs(~configSources=None);
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
         |> List.iter(cmtFile => {
              let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
              cmtFilePath |> loadCmtFile(~analysis);
            });
       });
  };
  switch (analysis) {
  | All =>
    DeadException.forceDelayedItems();
    DeadCommon.reportDead(ppf);
    DeadCommon.WriteDeadAnnotations.write();
    Exception.reportResults(~ppf);
    Arnold.reportResults(~ppf);
  | Dce =>
    DeadException.forceDelayedItems();
    DeadCommon.reportDead(ppf);
    DeadCommon.WriteDeadAnnotations.write();
  | Exception => Exception.reportResults(~ppf)
  | Termination => Arnold.reportResults(~ppf)
  };
  Log_.Stats.report();
  Log_.Stats.clear();
};

type cliCommand =
  | All(option(string))
  | Exception(option(string))
  | DCE(option(string))
  | NoOp
  | Termination(option(string));

let cli = () => {
  let cliCommand = ref(NoOp);
  let usage = "reanalyze version " ++ Version.version;
  let versionAndExit = () => {
    print_endline(usage);
    exit(0);
  };
  let rec printUsageAndExit = () => {
    Arg.usage(speclist, usage);
    exit(0);
  }
  and setCliCommand = command => {
    if (cliCommand^ != NoOp) {
      printUsageAndExit();
    };
    cliCommand := command;
  }
  and setAll = cmtRoot => {
    All(cmtRoot) |> setCliCommand;
  }
  and setDCE = cmtRoot => {
    DCE(cmtRoot) |> setCliCommand;
  }
  and setException = cmtRoot => {
    Exception(cmtRoot) |> setCliCommand;
  }
  and setDebug = () => {
    debug := true;
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
    DeadCommon.Cli.write := true;
  }
  and setTermination = cmtRoot => {
    Termination(cmtRoot) |> setCliCommand;
  }
  and setLiveNames = s => {
    let names = s |> String.split_on_char(',');
    DeadCommon.Cli.liveNames := names @ DeadCommon.Cli.liveNames.contents;
  }
  and setLivePaths = s => {
    let paths = s |> String.split_on_char(',');
    DeadCommon.Cli.livePaths := paths @ DeadCommon.Cli.livePaths.contents;
  }
  and speclist = [
    (
      "-all",
      Arg.Unit(() => setAll(None)),
      "Run all the analyses.",
    ),
    (
      "-all-cmt",
      Arg.String(s => setAll(Some(s))),
      "root_path Run all the analyses for all the .cmt files under the root path",
    ),
    (
      "-ci",
      Arg.Unit(() => Common.ci := true),
      "Internal flag for use in CI",
    ),
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
      "-live-names",
      Arg.String(s => setLiveNames(s)),
      "comma-separated-names Consider all values with the give names as live",
    ),
    (
      "-live-paths",
      Arg.String(s => setLivePaths(s)),
      "comma-separated-path-prefixes Consider all values whose path has a prefix in the list as live",
    ),
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
  let executeCliCommand = cliCommand =>
    switch (cliCommand) {
    | NoOp => printUsageAndExit()
    | All(cmtRoot) => runAnalysis(~analysis=All, ~cmtRoot, ~ppf)
    | DCE(cmtRoot) => runAnalysis(~analysis=Dce, ~cmtRoot, ~ppf)
    | Exception(cmtRoot) => runAnalysis(~analysis=Exception, ~cmtRoot, ~ppf)
    | Termination(cmtRoot) =>
      runAnalysis(~analysis=Termination, ~cmtRoot, ~ppf)
    };

  Arg.parse(speclist, print_endline, usage);

  executeCliCommand(cliCommand^);
};

cli();