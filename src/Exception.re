module Exn: {
  type t;
  let failure: t;
  let fromString: string => t;
  let invalidArgument: t;
  let notFound: t;
  let toString: t => string;
} = {
  type t = string;
  let failure = "Failure";
  let invalidArgument = "Invalid_argument";
  let notFound = "Not_found";
  let fromString = s => s;
  let toString = s => s;
};

module Event = {
  type kind =
    | Raises
    | Catches
    | Calls
    | Lib;

  type t = {
    exceptions: list(Exn.t),
    kind,
    loc: Location.t,
  };

  let exceptionsToString = exceptions =>
    exceptions |> List.map(Exn.toString) |> String.concat(" ");
};

let valueBindingsTable = Hashtbl.create(15);

let raisesLibTable = {
  let table = Hashtbl.create(15);
  open Exn;
  [
    (
      "List",
      [
        ("hd", [failure]),
        ("tl", [failure]),
        ("nth", [failure, invalidArgument]),
        ("nth_opt", [invalidArgument]),
        ("init", [invalidArgument]),
        ("iter2", [invalidArgument]),
        ("map2", [invalidArgument]),
        ("fold_left2", [invalidArgument]),
        ("fold_right2", [invalidArgument]),
        ("for_all2", [invalidArgument]),
        ("exists2", [invalidArgument]),
        ("find", [notFound]),
        ("assoc", [notFound]),
        ("combine", [invalidArgument]),
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
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args) =>
      let calleeName = Path.name(callee);
      if (calleeName == "Pervasives.raise") {
        let exceptions =
          switch (args) {
          | [(_, Some({exp_desc: Texp_construct(lid, _, _)}))] => [
              Exn.fromString(
                lid.txt |> Longident.flatten |> String.concat("."),
              ),
            ]
          | _ => [Exn.fromString("TODO_from_raise")]
          };
        currentEvents :=
          [
            {Event.kind: Raises, loc: e.exp_loc, exceptions},
            ...currentEvents^,
          ];
      } else {
        switch (Hashtbl.find_opt(valueBindingsTable, calleeName)) {
        | Some(Some(payload)) =>
          let exceptions =
            switch (payload) {
            | Annotation.StringPayload(s)
            | Annotation.ConstructPayload(s) => [Exn.fromString(s)]
            | _ => [Exn.fromString("TODO_from_call")]
            };
          currentEvents :=
            [
              {Event.kind: Calls, loc: e.exp_loc, exceptions},
              ...currentEvents^,
            ];
        | _ =>
          switch (Hashtbl.find_opt(raisesLibTable, calleeName)) {
          | Some(exceptions) =>
            currentEvents :=
              [
                {Event.kind: Lib, loc: e.exp_loc, exceptions},
                ...currentEvents^,
              ]
          | None => ()
          }
        };
      };
    | Texp_match(_) when e.exp_desc |> Compat.texpMatchHasExceptions =>
      currentEvents :=
        [
          {Event.kind: Catches, loc: e.exp_loc, exceptions: []},
          ...currentEvents^,
        ]
    | Texp_try(_) =>
      currentEvents :=
        [
          {Event.kind: Catches, loc: e.exp_loc, exceptions: []},
          ...currentEvents^,
        ]
    | _ => ()
    };
    super.expr(self, e);
  };

  let nested = true;

  let value_binding = (self: Tast_mapper.mapper, vb: Typedtree.value_binding) => {
    let oldId = currentId^;
    let oldEvents = currentEvents^;
    let shouldUpdateCurrent = nested || currentId^ == "";
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, _) =>
      let name = Ident.name(id);
      if (shouldUpdateCurrent) {
        currentId := name;
        currentEvents := [];
      };
      let raisesAnnotationPayload =
        vb.vb_attributes |> Annotation.getAttributePayload((==)("raises"));
      Hashtbl.replace(
        valueBindingsTable,
        Ident.name(id),
        raisesAnnotationPayload,
      );
      let res = super.value_binding(self, vb);
      let eventIsCatches = (event: Event.t) => event.kind == Catches;
      let (eventsCatches, eventsNotCatches) =
        currentEvents^ |> List.partition(event => eventIsCatches(event));
      let hasRaisesAnnotation =
        switch (Hashtbl.find_opt(valueBindingsTable, name)) {
        | Some(Some(_)) => true
        | _ => false
        };

      let shouldReport =
        eventsNotCatches != [] && eventsCatches == [] && !hasRaisesAnnotation;
      if (shouldReport) {
        let event = eventsNotCatches |> List.hd;
        Log_.info(~loc=event.loc, ~name="Exception Analysis", (ppf, ()) =>
          Format.fprintf(
            ppf,
            "@{<info>%s@} might raise an exception @{<info>%s@} and is not annotated with @raises",
            name,
            event.exceptions |> Event.exceptionsToString,
          )
        );
      };

      if (shouldUpdateCurrent) {
        currentId := oldId;
        currentEvents := oldEvents;
      };

      res;

    | _ => super.value_binding(self, vb)
    };
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

let reportResults = (~ppf as _) => ();