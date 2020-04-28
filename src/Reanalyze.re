open DeadCommon;

let version = Version.version;

type cliCommand =
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
  and setDebug = () => {
    DeadCommon.debug := true;
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
    ("-dce", Arg.Unit(() => setDCE(None)), "Eperimental DCE"),
    ("-debug", Arg.Unit(setDebug), "Print debug information"),
    (
      "-dce-cmt",
      Arg.String(s => setDCE(Some(s))),
      "root_path Experimental DCE for all the .cmt files under the root path",
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
    | DCE(cmtRoot) =>
      DeadCode.runAnalysis(~analysis=Dce, ~cmtRoot, ~ppf);
      DeadCode.reportResults(ppf);
    | Termination(cmtRoot) =>
      DeadCode.runAnalysis(~analysis=Termination, ~cmtRoot, ~ppf);
      Arnold.reportResults(~ppf);
    };

  Arg.parse(speclist, print_endline, usage);

  executeCliCommand(cliCommand^);
};

cli();