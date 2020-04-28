type event =
  | Raises(Location.t)
  | Catches(Location.t);

let traverseAst = {
  let super = Tast_mapper.default;

  let currentId = ref("");
  let currentEvents = ref([]);

  let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
    switch (e.exp_desc) {
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, _) =>
      let functionName = Path.name(callee);
      if (functionName == "Pervasives.raise") {
        currentEvents := [Raises(e.exp_loc), ...currentEvents^];
        Log_.item(
          "XXX %s %s this raises #currentEvents:%d@.",
          currentId^,
          e.exp_loc.loc_start |> DeadCommon.posToString,
          List.length(currentEvents^),
        );
      };
    | Texp_match(_) when e.exp_desc |> Compat.texpMatchHasExceptions =>
      currentEvents := [Catches(e.exp_loc), ...currentEvents^];
      Log_.item(
        "XXX %s %s this catches #currentEvents:%d@.",
        currentId^,
        e.exp_loc.loc_start |> DeadCommon.posToString,
        List.length(currentEvents^),
      );
    | Texp_try(_) =>
      currentEvents := [Catches(e.exp_loc), ...currentEvents^];
      Log_.item(
        "XXX %s %s this catches #currentEvents:%d@.",
        currentId^,
        e.exp_loc.loc_start |> DeadCommon.posToString,
        List.length(currentEvents^),
      );

    | _ => ()
    };
    super.expr(self, e);
  };

  let value_binding = (self: Tast_mapper.mapper, vb: Typedtree.value_binding) => {
    let oldId = currentId^;
    let oldEvents = currentEvents^;
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, _) =>
      currentId := Ident.name(id);
      ();
    | _ => ()
    };
    let res = super.value_binding(self, vb);
    currentId := oldId;
    currentEvents := oldEvents;
    res;
  };

  Tast_mapper.{...super, expr, value_binding};
};

let processStructure = (structure: Typedtree.structure) => {
  structure |> traverseAst.structure(traverseAst) |> ignore;
};

let processCmt = (cmt_infos: Cmt_format.cmt_infos) =>
  switch (cmt_infos.cmt_annots) {
  | Interface(_) => ()
  | Implementation(structure) => processStructure(structure)
  | _ => ()
  };

let reportResults = (~ppf) =>
  Format.fprintf(ppf, "Report Exception results@.");