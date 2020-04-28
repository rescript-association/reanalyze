let traverseAst = {
  let super = Tast_mapper.default;

  let currentId = ref("");

  let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
    switch (e.exp_desc) {
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, _) =>
      let functionName = Path.name(callee);
      if (functionName == "Pervasives.raise") {
        Log_.item(
          "XXX %s %s this raises@.",
          currentId^,
          e.exp_loc.loc_start |> DeadCommon.posToString,
        );
      };
    | Texp_match(_) when e.exp_desc |> Compat.texpMatchHasExceptions =>
      Log_.item(
        "XXX %s %s this catches@.",
        currentId^,
        e.exp_loc.loc_start |> DeadCommon.posToString,
      )
    | Texp_try(_) =>
      Log_.item(
        "XXX %s %s this catches@.",
        currentId^,
        e.exp_loc.loc_start |> DeadCommon.posToString,
      )

    | _ => ()
    };
    super.expr(self, e);
  };

  let value_bindings = (self: Tast_mapper.mapper, (recFlag, valueBindings)) => {
    valueBindings
    |> List.iter((vb: Typedtree.value_binding) => {
         let oldId = currentId^;
         switch (vb.vb_pat.pat_desc) {
         | Tpat_var(id, _) =>
           currentId := Ident.name(id);
           ();
         | _ => ()
         };
         super.value_binding(self, vb) |> ignore;
         currentId := oldId;
       });

    (recFlag, valueBindings);
  };

  Tast_mapper.{...super, expr, value_bindings};
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