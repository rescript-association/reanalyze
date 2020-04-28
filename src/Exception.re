type event =
  | Raises(Location.t)
  | Catches(Location.t);

let valueBindingsTable = Hashtbl.create(15);

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
      };
    | Texp_match(_) when e.exp_desc |> Compat.texpMatchHasExceptions =>
      currentEvents := [Catches(e.exp_loc), ...currentEvents^]
    | Texp_try(_) => currentEvents := [Catches(e.exp_loc), ...currentEvents^]
    | _ => ()
    };
    super.expr(self, e);
  };

  let value_binding = (self: Tast_mapper.mapper, vb: Typedtree.value_binding) => {
    let oldId = currentId^;
    let oldEvents = currentEvents^;
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, {loc: {loc_start: pos}}) =>
      currentId := Ident.name(id);
      let hasRaisesAnnotation =
        vb.vb_attributes |> Annotation.getAttributePayload((==)("raises"));
      Hashtbl.replace(
        valueBindingsTable,
        Ident.name(id),
        (pos, hasRaisesAnnotation),
      );
    | _ => ()
    };
    let res = super.value_binding(self, vb);
    let hasRaise =
      currentEvents^
      |> List.exists(event =>
           switch (event) {
           | Raises(_) => true
           | Catches(_) => false
           }
         );
    let hasCatch =
      currentEvents^
      |> List.exists(event =>
           switch (event) {
           | Raises(_) => false
           | Catches(_) => true
           }
         );
    let hasRaisesAnnotation =
      switch (Hashtbl.find_opt(valueBindingsTable, currentId^)) {
      | Some((_loc, Some(_))) => true
      | _ => false
      };
    let shouldReport = hasRaise && !hasCatch && !hasRaisesAnnotation;
    if (shouldReport) {
      Log_.info(~loc=vb.vb_pat.pat_loc, ~name="Exception Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "%s might raise an exception and is not annotated with @raises",
          currentId^,
        )
      );
    };
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