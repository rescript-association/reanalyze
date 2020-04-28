module Event = {
  type kind =
    | Raises
    | Catches
    | Calls
    | Lib;

  type t = {
    kind,
    loc: Location.t,
  };
};

let valueBindingsTable = Hashtbl.create(15);

type exceptions =
  | Failure
  | Invalid_argument
  | Not_found;

let raisesLibTable = {
  let table = Hashtbl.create(15);
  [
    (
      "List",
      [
        ("hd", [Failure]),
        ("tl", [Failure]),
        ("nth", [Failure, Invalid_argument]),
        ("nth_opt", [Invalid_argument]),
        ("init", [Invalid_argument]),
        ("iter2", [Invalid_argument]),
        ("map2", [Invalid_argument]),
        ("fold_left2", [Invalid_argument]),
        ("fold_right2", [Invalid_argument]),
        ("for_all2", [Invalid_argument]),
        ("exists2", [Invalid_argument]),
        ("find", [Not_found]),
        ("assoc", [Not_found]),
        ("combine", [Invalid_argument]),
      ],
    ),
  ]
  |> List.iter(((name, group)) =>
       group
       |> List.iter(((s, e)) => Hashtbl.add(table, name ++ "." ++ s, e))
     );

  table;
};

let traverseAst = {
  let super = Tast_mapper.default;

  let currentId = ref("");
  let currentEvents = ref([]);

  let expr = (self: Tast_mapper.mapper, e: Typedtree.expression) => {
    switch (e.exp_desc) {
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, _) =>
      let functionName = Path.name(callee);
      if (functionName == "Pervasives.raise") {
        currentEvents :=
          [{Event.kind: Raises, loc: e.exp_loc}, ...currentEvents^];
      } else {
        switch (Hashtbl.find_opt(valueBindingsTable, functionName)) {
        | Some((loc, Some(_))) =>
          currentEvents := [{Event.kind: Calls, loc}, ...currentEvents^]
        | _ =>
          if (Hashtbl.mem(raisesLibTable, functionName)) {
            currentEvents :=
              [{Event.kind: Lib, loc: e.exp_loc}, ...currentEvents^];
          }
        };
      };
    | Texp_match(_) when e.exp_desc |> Compat.texpMatchHasExceptions =>
      currentEvents :=
        [{Event.kind: Catches, loc: e.exp_loc}, ...currentEvents^]
    | Texp_try(_) =>
      currentEvents :=
        [{Event.kind: Catches, loc: e.exp_loc}, ...currentEvents^]
    | _ => ()
    };
    super.expr(self, e);
  };

  let nested = false;

  let value_binding = (self: Tast_mapper.mapper, vb: Typedtree.value_binding) => {
    let oldId = currentId^;
    let oldEvents = currentEvents^;
    let shouldUpdateCurrent = nested || currentId^ == "";
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, {loc}) =>
      if (shouldUpdateCurrent) {
        currentId := Ident.name(id);
      };
      let hasRaisesAnnotation =
        vb.vb_attributes |> Annotation.getAttributePayload((==)("raises"));
      Hashtbl.replace(
        valueBindingsTable,
        Ident.name(id),
        (loc, hasRaisesAnnotation),
      );
    | _ => ()
    };
    let res = super.value_binding(self, vb);
    let eventIsCatches = (event: Event.t) => event.kind == Catches;
    let (eventsCatches, eventsNotCatches) =
      currentEvents^ |> List.partition(event => eventIsCatches(event));
    let hasRaisesAnnotation =
      switch (Hashtbl.find_opt(valueBindingsTable, currentId^)) {
      | Some((_loc, Some(_))) => true
      | _ => false
      };
    let shouldReport =
      eventsNotCatches != [] && eventsCatches == [] && !hasRaisesAnnotation;
    if (shouldReport) {
      Log_.info(
        ~loc=(eventsNotCatches |> List.hd).loc,
        ~name="Exception Analysis",
        (ppf, ()) =>
        Format.fprintf(
          ppf,
          "%s might raise an exception and is not annotated with @raises",
          currentId^,
        )
      );
    };
    if (shouldUpdateCurrent) {
      currentId := oldId;
      currentEvents := oldEvents;
    };
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