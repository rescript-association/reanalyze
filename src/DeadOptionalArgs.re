open DeadCommon;
open Common;

let active = () => Cli.experimental^;

type item = {
  posTo: Lexing.position,
  argNames: list(string),
  argNamesMaybe: list(string),
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
        !OptionalArgs.isEmpty(optionalArgs)
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

let rec hasOptionalArgs = (texpr: Types.type_expr) =>
  switch (texpr.desc) {
  | _ when !active() => false
  | Tarrow(Optional(_), _tFrom, _tTo, _) => true
  | Tarrow(_, _tFrom, tTo, _) => hasOptionalArgs(tTo)
  | Tlink(t)
  | Tsubst(t) => hasOptionalArgs(t)
  | _ => false
  };

let rec fromTypeExpr = (texpr: Types.type_expr) =>
  switch (texpr.desc) {
  | _ when !active() => []
  | Tarrow(Optional(s), _tFrom, tTo, _) => [s, ...fromTypeExpr(tTo)]
  | Tarrow(_, _tFrom, tTo, _) => fromTypeExpr(tTo)
  | Tlink(t)
  | Tsubst(t) => fromTypeExpr(t)
  | _ => []
  };

let addReferences =
    (
      ~locFrom: Location.t,
      ~locTo: Location.t,
      ~path,
      (argNames, argNamesMaybe),
    ) =>
  if (active()) {
    let posTo = locTo.loc_start;
    let posFrom = locFrom.loc_start;
    delayedItems := [{posTo, argNames, argNamesMaybe}, ...delayedItems^];
    if (Common.debug^) {
      Log_.item(
        "DeadOptionalArgs.addReferences %s called with optional argNames:%s argNamesMaybe:%s %s@.",
        path |> Path.fromPathT |> Path.toString,
        argNames |> String.concat(", "),
        argNamesMaybe |> String.concat(", "),
        posFrom |> posToString,
      );
    };
  };

let forceDelayedItems = () => {
  let items = delayedItems^ |> List.rev;
  delayedItems := [];
  items
  |> List.iter(({posTo, argNames, argNamesMaybe}) =>
       switch (PosHash.find_opt(decls, posTo)) {
       | Some({declKind: Value(r)}) =>
         r.optionalArgs |> OptionalArgs.call(~argNames, ~argNamesMaybe)
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
         OptionalArgs.combine(rFrom.optionalArgs, rTo.optionalArgs)
       | _ => ()
       }
     );
};

let check = decl =>
  switch (decl) {
  | {declKind: Value({optionalArgs})} when active() =>
    optionalArgs
    |> OptionalArgs.iterUnused(s => {
         Log_.info(
           ~loc=decl |> declGetLoc, ~name="Warning Unused Argument", (ppf, ()) =>
           Format.fprintf(
             ppf,
             "optional argument @{<info>%s@} of function @{<info>%s@} is never used",
             s,
             decl.path |> Path.withoutHead,
           )
         )
       });
    optionalArgs
    |> OptionalArgs.iterAlwaysUsed((s, nCalls) => {
         Log_.info(
           ~loc=decl |> declGetLoc,
           ~name="Warning Redundant Optional Argument",
           (ppf, ()) =>
           Format.fprintf(
             ppf,
             "optional argument @{<info>%s@} of function @{<info>%s@} is always supplied (%d calls)",
             s,
             decl.path |> Path.withoutHead,
             nCalls,
           )
         )
       });

  | _ => ()
  };