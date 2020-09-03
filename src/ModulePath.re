open Common;

module NameMap = Map.Make(Name);

/* Keep track of the module path while traversing with Tast_mapper */
type t = {
  aliases: NameMap.t(Path.t),
  loc: Location.t,
  path: Path.t,
};

let initial: t = {aliases: NameMap.empty, loc: Location.none, path: []};
let current: ref(t) = ref(initial);

let init = () => current := initial;

let normalizePath = (~aliases, path) => {
  switch (path |> List.rev) {
  | [name, ...restRev] when restRev != [] =>
    switch (aliases |> NameMap.find_opt(name)) {
    | None => path
    | Some(path1) =>
      let newPath = List.rev(path1 @ restRev);
      if (Common.Cli.debug^) {
        Log_.item(
          "Resolve Alias: %s to %s@.",
          path |> Common.Path.toString,
          newPath |> Common.Path.toString,
        );
      };
      newPath;
    }
  | _ => path
  };
};

let addAlias = (~name, ~path) => {
  let aliases = current^.aliases;
  let pathNormalized = path |> normalizePath(~aliases);
  if (Common.Cli.debug^) {
    Log_.item(
      "Module Alias: %s = %s@.",
      name |> Name.toString,
      Path.toString(pathNormalized),
    );
  };
  current :=
    {...current^, aliases: NameMap.add(name, pathNormalized, aliases)};
};

let resolveAlias = path => path |> normalizePath(~aliases=current^.aliases);

let getCurrent = () => current^;

let setCurrent = p => current := p;
