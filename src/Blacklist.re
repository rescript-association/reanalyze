let blacklist: ref(option(string)) = ref(None);
let whitelist: ref(option(string)) = ref(None);

let checkPrefix = prefix_ => {
  let prefix =
    Paths.projectRoot^ == ""
      ? prefix_ : Filename.concat(Paths.projectRoot^, prefix_);
  let prefixLen = prefix |> String.length;
  sourceDir =>
    String.length(sourceDir) >= prefixLen
    && String.sub(sourceDir, 0, prefixLen) == prefix;
};

let blacklistSourceDir =
  lazy(
    {
      switch (blacklist^) {
      | None => (_sourceDir => false)
      | Some(prefix) => checkPrefix(prefix)
      };
    }
  );

let whitelistSourceDir =
  lazy(
    {
      switch (whitelist^) {
      | None => (_sourceDir => true)
      | Some(prefix) => checkPrefix(prefix)
      };
    }
  );

let posInBlacklist = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(blacklistSourceDir);
};

let posInWhitelist = (pos: Lexing.position) => {
  pos.pos_fname |> Lazy.force(whitelistSourceDir);
};


// First blacklist, then override with whitelist
let filter = pos => posInWhitelist(pos) && !posInBlacklist(pos);