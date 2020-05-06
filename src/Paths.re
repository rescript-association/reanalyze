module StringMap = Map.Make(String);

let bsconfig = "bsconfig.json";

let rec findProjectRoot = (~dir) =>
  if (Sys.file_exists(Filename.concat(dir, bsconfig))) {
    dir;
  } else {
    let parent = dir |> Filename.dirname;
    if (parent == dir) {
      prerr_endline(
        "Error: cannot find project root containing " ++ bsconfig ++ ".",
      );
      assert(false);
    } else {
      findProjectRoot(~dir=parent);
    };
  };
let setProjectRoot = () => {
  Blacklist.projectRoot := findProjectRoot(~dir=Sys.getcwd());
  Blacklist.bsbProjectRoot :=
    (
      switch (Sys.getenv_opt("BSB_PROJECT_ROOT")) {
      | None => Blacklist.projectRoot^
      | Some(s) => s
      }
    );
};

/*
 * Handle namespaces in cmt files.
 * E.g. src/Module-Project.cmt becomes src/Module
 */
let handleNamespace = cmt => {
  let cutAfterDash = s =>
    switch (String.index(s, '-')) {
    | n =>
      try(String.sub(s, 0, n)) {
      | Invalid_argument(_) => s
      }
    | exception Not_found => s
    };
  let noDir = Filename.basename(cmt) == cmt;
  if (noDir) {
    cmt |> Filename.remove_extension |> cutAfterDash;
  } else {
    let dir = cmt |> Filename.dirname;
    let base =
      cmt |> Filename.basename |> Filename.remove_extension |> cutAfterDash;
    Filename.concat(dir, base);
  };
};

let getModuleName = cmt => cmt |> handleNamespace |> Filename.basename;

let readDirsFromConfig = (~configSources) => {
  let dirs = ref([]);
  let root = Blacklist.projectRoot^;

  let rec processDir = (~subdirs, dir) => {
    let absDir = dir == "" ? root : Filename.concat(root, dir);
    if (Sys.file_exists(absDir) && Sys.is_directory(absDir)) {
      dirs := [dir, ...dirs^];
      if (subdirs) {
        absDir
        |> Sys.readdir
        |> Array.iter(d => processDir(~subdirs, Filename.concat(dir, d)));
      };
    };
  };

  let rec processSourceItem = (sourceItem: Ext_json_types.t) =>
    switch (sourceItem) {
    | Str(str) => str |> processDir(~subdirs=false)
    | Obj(map) =>
      switch (map |> StringMap.find_opt("dir")) {
      | Some(Str(str)) =>
        let subdirs =
          switch (map |> StringMap.find_opt("subdirs")) {
          | Some(True(_)) => true
          | Some(False(_)) => false
          | _ => false
          };
        str |> processDir(~subdirs);
      | _ => ()
      }
    | Arr(arr) => arr |> Array.iter(processSourceItem)
    | _ => ()
    };

  switch (configSources) {
  | Some(sourceItem) => processSourceItem(sourceItem)
  | None => ()
  };
  dirs^;
};

let readSourceDirs = (~configSources) => {
  let sourceDirs =
    ["lib", "bs", ".sourcedirs.json"]
    |> List.fold_left(Filename.concat, Blacklist.bsbProjectRoot^);
  let dirs = ref([]);

  let readDirs = json => {
    switch (json) {
    | Ext_json_types.Obj(map) =>
      switch (map |> StringMap.find_opt("dirs")) {
      | Some(Arr(arr)) =>
        arr
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Str(str) => dirs := [str, ...dirs^]
             | _ => ()
             }
           );
        ();
      | _ => ()
      }
    | _ => ()
    };
  };

  if (sourceDirs |> Sys.file_exists) {
    let jsonOpt = sourceDirs |> Ext_json_parse.parse_json_from_file;
    switch (jsonOpt) {
    | Some(json) =>
      if (Blacklist.bsbProjectRoot^ != Blacklist.projectRoot^) {
        readDirs(json);
        dirs := readDirsFromConfig(~configSources);
      } else {
        readDirs(json);
      }
    | None => ()
    };
  } else {
    Log_.item("Warning: can't find source dirs: %s\n", sourceDirs);
    Log_.item("Types for cross-references will not be found by genType.\n");
    dirs := readDirsFromConfig(~configSources);
  };
  dirs^;
};