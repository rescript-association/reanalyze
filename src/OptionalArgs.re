open DeadCommon;
open Common;

let active = () => Cli.experimental^;

type item = {
  posTo: Lexing.position,
  argName: string,
};

let delayedItems: ref(list(item)) = ref([]);
let functionReferences: ref(list((Lexing.position, Lexing.position))) =
  ref([]);

let addFunctionReference = (~locFrom: Location.t, ~locTo: Location.t) =>
  if (active()) {
    let posTo = locTo.loc_start;
    let posFrom = locFrom.loc_start;
    let shouldAdd =
      switch (PosHash.find_opt(decls, posTo)) {
      | Some({declKind: Value({optionalArgs})}) =>
        !StringSet.is_empty(optionalArgs)
      | _ => false
      };
    if (shouldAdd) {
      if (Common.debug^) {
        Log_.item(
          "OptionalArgs.addFunctionReference %s %s@.",
          posFrom |> posToString,
          posTo |> posToString,
        );
      };
      functionReferences := [(posFrom, posTo), ...functionReferences^];
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
    let posTo = locTo.loc_start;
    let posFrom = locFrom.loc_start;
    delayedItems := [{posTo, argName}, ...delayedItems^];
    if (Common.debug^) {
      Log_.item(
        "OptionalArgs.addReference %s called with optional arg %s %s@.",
        path |> Path.fromPathT |> Path.toString,
        argName,
        posFrom |> posToString,
      );
    };
  };

let forceDelayedItems = () => {
  let items = delayedItems^ |> List.rev;
  delayedItems := [];
  items
  |> List.iter(({posTo, argName}) =>
       switch (PosHash.find_opt(decls, posTo)) {
       | Some({declKind: Value(r)}) =>
         r.optionalArgs = r.optionalArgs |> StringSet.remove(argName)
       | _ => ()
       }
     );
  let fRefs = functionReferences^ |> List.rev;
  functionReferences := [];
  fRefs
  |> List.iter(((posFrom, posTo)) =>
       switch (
         PosHash.find_opt(decls, posFrom),
         PosHash.find_opt(decls, posTo),
       ) {
       | (Some({declKind: Value(rFrom)}), Some({declKind: Value(rTo)})) =>
         let intersection =
           StringSet.inter(rFrom.optionalArgs, rTo.optionalArgs);
         rFrom.optionalArgs = intersection;
         rTo.optionalArgs = intersection;
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