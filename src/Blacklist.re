let blacklist: ref(list(string)) = ref([]);
let whitelist: ref(list(string)) = ref([]);

let projectRoot = ref("");
let bsbProjectRoot = ref("");

let checkPrefix = prefix_ => {
  let prefix =
    projectRoot^ == ""
      ? prefix_ : Filename.concat(projectRoot^, prefix_);
  let prefixLen = prefix |> String.length;
  sourceDir =>
    String.length(sourceDir) >= prefixLen
    && String.sub(sourceDir, 0, prefixLen) == prefix;
};

let blacklistSourceDir =
  lazy(
    sourceDir =>
      blacklist^ |> List.exists(prefix => checkPrefix(prefix, sourceDir))
  );

let whitelistSourceDir =
  lazy(
    sourceDir =>
      whitelist^ |> List.exists(prefix => checkPrefix(prefix, sourceDir))
  );

let posInBlacklist = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(blacklistSourceDir);
};

let posInWhitelist = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(whitelistSourceDir);
};

// First blacklist, then override with whitelist
let filter = pos => !posInBlacklist(pos) || posInWhitelist(pos);