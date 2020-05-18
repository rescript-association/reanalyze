let suppress: ref(list(string)) = ref([]);
let unsuppress: ref(list(string)) = ref([]);

let projectRoot = ref("");
let bsbProjectRoot = ref("");

let checkPrefix = prefix_ => {
  let prefix =
    projectRoot^ == "" ? prefix_ : Filename.concat(projectRoot^, prefix_);
  let prefixLen = prefix |> String.length;
  sourceDir =>
    try(String.sub(sourceDir, 0, prefixLen) == prefix) {
    | Invalid_argument(_) => false
    };
};

let suppressSourceDir =
  lazy(
    sourceDir =>
      suppress^ |> List.exists(prefix => checkPrefix(prefix, sourceDir))
  );

let unsuppressSourceDir =
  lazy(
    sourceDir =>
      unsuppress^ |> List.exists(prefix => checkPrefix(prefix, sourceDir))
  );

let posInSuppress = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(suppressSourceDir);
};

let posInUnsuppress = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(unsuppressSourceDir);
};

// First suppress list, then override with unsuppress list
let filter = pos => !posInSuppress(pos) || posInUnsuppress(pos);