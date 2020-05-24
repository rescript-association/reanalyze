open DeadCommon;

let rec fromTypeExpr = (texpr: Types.type_expr) =>
  switch (texpr.desc) {
  | Tarrow(Optional(s), _tFrom, tTo, _) => [s, ...fromTypeExpr(tTo)]
  | Tarrow(_, _tFrom, tTo, _) => fromTypeExpr(tTo)
  | Tlink(t)
  | Tsubst(t) => fromTypeExpr(t)
  | _ => []
  };

let addReference = (~locFrom: Location.t, ~locTo: Location.t, ~path, s) => {
  let (declFound, argFound) =
    switch (PosHash.find_opt(decls, locTo.loc_start)) {
    | Some({declKind: Value({optionalArgs})}) => (
        true,
        List.mem(s, optionalArgs),
      )
    | _ => (false, false)
    };
  Log_.item(
    "XXX %s-%s %s called with optional arg %s declFound:%b%s@.",
    locFrom.loc_start |> posToString,
    locFrom.loc_end |> posToString,
    path |> Path.fromPathT |> Path.toString,
    s,
    declFound,
    declFound ? " argFound" ++ (argFound ? "true" : "false") : "",
  );
};