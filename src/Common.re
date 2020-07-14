let currentSrc = ref("");
let currentModule = ref("");
let currentModuleName = ref("" |> Name.create);

/* Location printer: `filename:line: ' */
let posToString = (pos: Lexing.position) => {
  let file = pos.Lexing.pos_fname;
  let line = pos.Lexing.pos_lnum;
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
  (file |> Filename.basename)
  ++ ":"
  ++ string_of_int(line)
  ++ ":"
  ++ string_of_int(col);
};

module Cli = {
  let debug = ref(false);

  let ci = ref(false);

  let experimental = ref(false);

  let write = ref(false);

  let liveNames = ref([]: list(string)); // names to be considered live values

  let livePaths = ref([]: list(string)); // paths of files where all values are considered live

  let excludePaths = ref([]: list(string)); // paths of files to exclude from analysis
};

module StringSet = Set.Make(String);

module LocSet =
  Set.Make({
    include Location;
    let compare = compare;
  });

module FileSet = Set.Make(String);
module FileHash = {
  include Hashtbl.Make({
    type t = string;

    let hash = (x: t) => Hashtbl.hash(x);

    let equal = (x: t, y) => x == y;
  });
};

module FileReferences = {
  let table: FileHash.t(FileSet.t) = FileHash.create(256); /* references across files */

  let findSet = (table, key) =>
    try(FileHash.find(table, key)) {
    | Not_found => FileSet.empty
    };

  let add = (locFrom: Location.t, locTo: Location.t) => {
    let key = locFrom.loc_start.pos_fname;
    let set = findSet(table, key);
    FileHash.replace(
      table,
      key,
      FileSet.add(locTo.loc_start.pos_fname, set),
    );
  };

  let addFile = fileName => {
    let set = findSet(table, fileName);
    FileHash.replace(table, fileName, set);
  };

  let exists = fileName => FileHash.mem(table, fileName);

  let find = fileName =>
    switch (FileHash.find_opt(table, fileName)) {
    | Some(set) => set
    | None => FileSet.empty
    };

  let iter = f => FileHash.iter(f, table);
};

module Path = {
  type t = list(Name.t);

  let toString = (path: t) =>
    path |> List.rev_map(Name.toString) |> String.concat(".");

  let withoutHead = path => {
    switch (path |> List.rev_map(Name.toString)) {
    | [_, ...tl] => tl |> String.concat(".")
    | [] => ""
    };
  };

  let onOkPath = (~whenContainsApply, ~f, path) => {
    switch (path |> Path.flatten) {
    | `Ok(id, mods) => f([Ident.name(id), ...mods] |> String.concat("."))
    | `Contains_apply => whenContainsApply
    };
  };

  let fromPathT = path => {
    switch (path |> Path.flatten) {
    | `Ok(id, mods) =>
      [Ident.name(id), ...mods] |> List.rev_map(Name.create)
    | `Contains_apply => []
    };
  };

  let moduleToImplementation = path =>
    switch (path |> List.rev) {
    | [moduleName, ...rest] =>
      [moduleName |> Name.toImplementation, ...rest] |> List.rev
    | [] => path
    };

  let moduleToInterface = path =>
    switch (path |> List.rev) {
    | [moduleName, ...rest] =>
      [moduleName |> Name.toInterface, ...rest] |> List.rev
    | [] => path
    };

  let toModuleName = (~isValue, path) => {
    switch (path) {
    | [_, ...tl] when isValue => tl |> toString
    | [_, _, ...tl] when !isValue => tl |> toString
    | _ => ""
    };
  };

  let typeToInterface = path =>
    switch (path) {
    | [typeName, ...rest] => [typeName |> Name.toInterface, ...rest]
    | [] => path
    };
};
