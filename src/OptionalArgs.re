open DeadCommon;

let rec fromTypeExpr = (texpr: Types.type_expr) =>
  switch (texpr.desc) {
  | Tarrow(Optional(s), _tFrom, tTo, _) => [s, ...fromTypeExpr(tTo)]
  | Tarrow(_, _tFrom, tTo, _) => fromTypeExpr(tTo)
  | Tlink(t)
  | Tsubst(t) => fromTypeExpr(t)
  | _ => []
  };

let addReference = (~locFrom: Location.t, ~path, s) => {
  Log_.item(
    "XXX %s-%s %s called with optional arg %s@.",
    locFrom.loc_start |> posToString,
    locFrom.loc_end |> posToString,
    path |> Path.fromPathT |> Path.toString,
    s,
  );
};