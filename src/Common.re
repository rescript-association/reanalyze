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

  let findSet = (table, key) =>
    try(find(table, key)) {
    | Not_found => FileSet.empty
    };

  let addFile = (table, key) => {
    let set = findSet(table, key);
    replace(table, key, set);
  };

  let addSet = (table, key, value) => {
    let set = findSet(table, key);
    replace(table, key, FileSet.add(value, set));
  };
};

let fileReferences: FileHash.t(FileSet.t) = FileHash.create(256); /* references across files */