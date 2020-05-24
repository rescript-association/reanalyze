open DeadCommon;

let active = true;

let rec fromTypeExpr = (texpr: Types.type_expr) =>
  switch (texpr.desc) {
  | _ when !active => []
  | Tarrow(Optional(s), _tFrom, tTo, _) => [s, ...fromTypeExpr(tTo)]
  | Tarrow(_, _tFrom, tTo, _) => fromTypeExpr(tTo)
  | Tlink(t)
  | Tsubst(t) => fromTypeExpr(t)
  | _ => []
  };

let addReference = (~locFrom: Location.t, ~locTo: Location.t, ~path, s) =>
  if (active) {
    let (declFound, argFound) =
      switch (PosHash.find_opt(decls, locTo.loc_start)) {
      | Some({declKind: Value({optionalArgs} as r)}) =>
        let argFound = List.mem(s, optionalArgs);
        if (argFound) {
          r.optionalArgs = r.optionalArgs |> List.filter(a => a != s);
        };
        (true, argFound);
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

let findUnusedArgs = () =>
  if (active) {
    decls
    |> PosHash.iter((pos, decl) =>
         switch (decl) {
         | {declKind: Value({optionalArgs})} =>
           optionalArgs
           |> List.iter(s =>
                Log_.item("XXX %s dead arg:%s@.", pos |> posToString, s)
              )
         | _ => ()
         }
       );
  };