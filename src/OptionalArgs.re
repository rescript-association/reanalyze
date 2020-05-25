open DeadCommon;
open Common;

let active = () => Cli.experimental^;

type item = {
  locTo: Location.t,
  argName: string,
};

let delayedItems: ref(list(item)) = ref([]);
let references: ref(list((Location.t, Location.t))) = ref([]);

let addFunctionReference = (~locFrom: Location.t, ~locTo: Location.t) =>
  if (active()) {
    let shouldAdd =
      switch (PosHash.find_opt(decls, locTo.loc_start)) {
      | Some({declKind: Value({optionalArgs})}) =>
        !StringSet.is_empty(optionalArgs)
      | _ => false
      };
    if (shouldAdd) {
      Log_.item(
        "OptionalArgs.addFunctionReference %s %s@.",
        locFrom.loc_start |> posToString,
        locTo.loc_start |> posToString,
      );
      references := [(locFrom, locTo), ...references^];
    };
  };

let rec fromTypeExpr = (texpr: Types.type_expr) =>
  switch (texpr.desc) {
  | _ when !active() => StringSet.empty
  | Tarrow(Optional(s), _tFrom, tTo, _) =>
    StringSet.add(s, fromTypeExpr(tTo))
  | Tarrow(_, _tFrom, tTo, _) => fromTypeExpr(tTo)
  | Tlink(t)
  | Tsubst(t) => fromTypeExpr(t)
  | _ => StringSet.empty
  };

let addReference = (~locFrom: Location.t, ~locTo: Location.t, ~path, argName) =>
  if (active()) {
    delayedItems := [{locTo, argName}, ...delayedItems^];
    if (Common.debug^) {
      Log_.item(
        "OptionalArgs.addReference %s called with optional arg %s %s@.",
        path |> Path.fromPathT |> Path.toString,
        argName,
        locFrom.loc_start |> posToString,
      );
    };
  };

let forceDelayedItems = () => {
  let items = delayedItems^ |> List.rev;
  delayedItems := [];
  items
  |> List.iter(({locTo, argName}) =>
       switch (PosHash.find_opt(decls, locTo.loc_start)) {
       | Some({declKind: Value(r)}) =>
         r.optionalArgs = r.optionalArgs |> StringSet.remove(argName)
       | _ => ()
       }
     );
};

let check = decl =>
  switch (decl) {
  | {declKind: Value({optionalArgs})} when active() =>
    optionalArgs
    |> StringSet.iter(s => {
         Log_.info(
           ~loc=decl |> declGetLoc, ~name="Warning Unused Argument", (ppf, ()) =>
           Format.fprintf(
             ppf,
             "optional argument @{<info>%s@} of function @{<info>%s@} is never used",
             s,
             decl.path |> Path.withoutHead,
           )
         )
       })
  | _ => ()
  };