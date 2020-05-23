let currentSrc = ref("");
let currentModule = ref("");
let currentModuleName = ref("" |> Name.create);

let debug = ref(false);

let ci = ref(false);

/* Location printer: `filename:line: ' */
let posToString = (~printCol=true, pos: Lexing.position) => {
  let file = pos.Lexing.pos_fname;
  let line = pos.Lexing.pos_lnum;
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
  (file |> Filename.basename)
  ++ ":"
  ++ string_of_int(line)
  ++ (printCol ? ":" ++ string_of_int(col) : ": ");
};

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