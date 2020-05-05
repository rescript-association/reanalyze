open DeadCommon;

let version = Version.version;

type analysisType =
  | Dce
  | Exception
  | Termination;

let loadCmtFile = (~analysis, cmtFilePath) => {
  if (debug^) {
    Log_.item("Scanning %s@.", cmtFilePath);
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
    let lib_bs = Paths.projectRoot^ +++ "lib" +++ "bs";

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
  | Dce =>
    reportDead(ppf);
    WriteDeadAnnotations.write();
  | Exception => Exception.reportResults(~ppf)
  | Termination => Arnold.reportResults(~ppf)
  };
};

type cliCommand =
  | Exception(option(string))
  | DCE(option(string))
  | NoOp
  | Termination(option(string));

let cli = () => {
  let cliCommand = ref(NoOp);
  let usage = "reanalyze version " ++ version;
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
  and setDCE = cmtRoot => {
    DCE(cmtRoot) |> setCliCommand;
  }
  and setException = cmtRoot => {
    Exception(cmtRoot) |> setCliCommand;
  }
  and setDebug = () => {
    DeadCommon.debug := true;
  }
  and setBlacklist = (prefix) => {
    DeadCommon.blacklist := Some(prefix);
  }
  and setWhitelist = (prefix) => {
    DeadCommon.whitelist := Some(prefix);
  }
  and setWrite = () => {
    DeadCommon.write := true;
  }
  and setTermination = cmtRoot => {
    Termination(cmtRoot) |> setCliCommand;
  }
  and setLiveNames = s => {
    let names = s |> String.split_on_char(',');
    DeadCommon.liveNames := names @ DeadCommon.liveNames.contents;
  }
  and setLivePaths = s => {
    let paths = s |> String.split_on_char(',');
    DeadCommon.livePaths := paths @ DeadCommon.livePaths.contents;
  }
  and speclist = [
    (
      "-blacklist",
      Arg.String(setBlacklist),
      "prefix Don't report on files whose path has the given prefix",
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
      "Experimental eexception analysis",
    ),
    (
      "-exception-cmt",
      Arg.String(s => setException(Some(s))),
      "root_path Experimental exception analysis for all the .cmt files under the root path",
    ),
    (
      "-live-names",
      Arg.String(s => setLiveNames(s)),
      "comma-separated-names Consider all values with the give names as lives",
    ),
    (
      "-live-paths",
      Arg.String(s => setLivePaths(s)),
      "comma-separated-path-prefixes Consider all values whose path has a prefix in the list",
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
      "-whitelist",
      Arg.String(setWhitelist),
      "prefix Report on blacklisted files whose path has the given prefix",
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
    | DCE(cmtRoot) => runAnalysis(~analysis=Dce, ~cmtRoot, ~ppf)
    | Exception(cmtRoot) => runAnalysis(~analysis=Exception, ~cmtRoot, ~ppf)
    | Termination(cmtRoot) =>
      runAnalysis(~analysis=Termination, ~cmtRoot, ~ppf)
    };

  Arg.parse(speclist, print_endline, usage);

  executeCliCommand(cliCommand^);
};

cli();