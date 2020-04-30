let debug = DeadCommon.debug;
let posToString = DeadCommon.posToString;

module Exn: {
  type t;
  let compare: (t, t) => int;
  let failure: t;
  let fromLid: Asttypes.loc(Longident.t) => t;
  let fromString: string => t;
  let matchFailure: t;
  let invalidArgument: t;
  let notFound: t;
  let toString: t => string;
} = {
  type t = string;
  let compare = String.compare;
  let failure = "Failure";
  let invalidArgument = "Invalid_argument";
  let matchFailure = "Match_failure";
  let notFound = "Not_found";
  let fromLid = lid =>
    lid.Asttypes.txt |> Longident.flatten |> String.concat(".");
  let fromString = s => s;
  let toString = s => s;
};

module ExnSet = Set.Make(Exn);

let exceptionsToString = exceptions =>
  exceptions |> List.map(Exn.toString) |> String.concat(" ");

module Event = {
  type kind =
    | Catches(list(t)) // with | E => ...
    | CallRaises // foo() when foo is annotated @raises
    | LibFunRaises // List.hd when it's modeled as raising exceptions
    | Raises // raise E

  and t = {
    exceptions: list(Exn.t),
    kind,
    loc: Location.t,
  };

  let isCatches = event =>
    switch (event.kind) {
    | Catches(_) => true
    | _ => false
    };

  let rec print = (ppf, event) =>
    switch (event) {
    | {kind: Raises | CallRaises | LibFunRaises, exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Events combine.loop: raises %s@.",
        loc.loc_start |> posToString,
        exceptions |> exceptionsToString,
      )
    | {kind: Catches(nestedEvents), exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Events combine.loop: Catches exceptions:%s nestedEvents:%a@.",
        loc.loc_start |> posToString,
        exceptions |> exceptionsToString,
        (ppf, ()) => {
          nestedEvents
          |> List.iter(e => {Format.fprintf(ppf, "%a ", print, e)})
        },
        (),
      )
    };

  let combine = events => {
    if (debug^) {
      Log_.item("@.");
      Log_.item("Events combine: #events %d@.", events |> List.length);
    };
    let rec loop = (acc, events) =>
      switch (events) {
      | [
          {kind: Raises | CallRaises | LibFunRaises, exceptions} as ev,
          ...rest,
        ] =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        loop(ExnSet.union(acc, exceptions |> ExnSet.of_list), rest);
      | [{kind: Catches(_), exceptions: [] /* catch-all */} as ev, ...rest] =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        loop(acc, rest);
      | [{kind: Catches(nestedEvents), exceptions} as ev, ...rest] =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        let nestedExnSet = loop(ExnSet.empty, nestedEvents);
        let newRaises =
          ExnSet.diff(nestedExnSet, exceptions |> ExnSet.of_list);
        loop(ExnSet.union(acc, newRaises), rest);
      | [] => acc
      };
    loop(ExnSet.empty, events);
  };
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

  let exceptionsOfPatterns = patterns =>
    patterns
    |> List.fold_left(
         (acc, desc) =>
           switch (desc) {
           | Typedtree.Tpat_construct(lid, _, _) => [
               Exn.fromLid(lid),
               ...acc,
             ]
           | _ => acc
           },
         [],
       );

  let iterExpr = (self, e) => self.Tast_mapper.expr(self, e) |> ignore;
  let iterExprOpt = (self, eo) =>
    switch (eo) {
    | None => ()
    | Some(e) => e |> iterExpr(self)
    };
  let iterPat = (self, p) => self.Tast_mapper.pat(self, p) |> ignore;
  let iterCases = (self, cases) =>
    cases
    |> List.iter((case: Typedtree.case) => {
         case.c_lhs |> iterPat(self);
         case.c_guard |> iterExprOpt(self);
         case.c_rhs |> iterExpr(self);
       });

  let expr = (self: Tast_mapper.mapper, expr: Typedtree.expression) => {
    let loc = expr.exp_loc;
    switch (expr.exp_desc) {
    | Texp_apply({exp_desc: Texp_ident(callee, _, _)}, args) =>
      let calleeName = Path.name(callee);
      if (calleeName == "Pervasives.raise") {
        let exceptions =
          switch (args) {
          | [(_, Some({exp_desc: Texp_construct(lid, _, _)}))] => [
              Exn.fromLid(lid),
            ]
          | _ => [Exn.fromString("TODO_from_raise")]
          };
        currentEvents :=
          [{Event.kind: Raises, loc, exceptions}, ...currentEvents^];
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
            [{Event.kind: CallRaises, loc, exceptions}, ...currentEvents^];
        | _ =>
          switch (Hashtbl.find_opt(raisesLibTable, calleeName)) {
          | Some(exceptions) =>
            currentEvents :=
              [
                {Event.kind: LibFunRaises, loc, exceptions},
                ...currentEvents^,
              ]
          | None => ()
          }
        };
      };

      args |> List.iter(((_, eOpt)) => eOpt |> iterExprOpt(self));

    | Texp_match(_) =>
      let (e, cases, partial) = Compat.getTexpMatch(expr.exp_desc);
      let exceptions =
        expr.exp_desc |> Compat.texpMatchGetExceptions |> exceptionsOfPatterns;
      if (exceptions != []) {
        let oldEvents = currentEvents^;
        currentEvents := [];
        e |> iterExpr(self);
        cases |> iterCases(self);
        currentEvents :=
          [
            {Event.kind: Catches(currentEvents^), loc, exceptions},
            ...oldEvents,
          ];
      } else {
        e |> iterExpr(self);
        cases |> iterCases(self);
      };
      if (partial == Partial) {
        currentEvents :=
          [
            {Event.kind: Raises, loc, exceptions: [Exn.matchFailure]},
            ...currentEvents^,
          ];
      };

    | Texp_try(e, cases) =>
      let exceptions =
        cases
        |> List.map((case: Typedtree.case) => case.c_lhs.pat_desc)
        |> exceptionsOfPatterns;
      let oldEvents = currentEvents^;
      currentEvents := [];
      e |> iterExpr(self);
      cases |> iterCases(self);
      currentEvents :=
        [
          {Event.kind: Catches(currentEvents^), loc, exceptions},
          ...oldEvents,
        ];

    | _ => super.expr(self, expr) |> ignore
    };
    expr;
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
      let raiseSet = currentEvents^ |> Event.combine;
      let hasRaisesAnnotation =
        switch (Hashtbl.find_opt(valueBindingsTable, name)) {
        | Some(Some(_)) => true
        | _ => false
        };

      let shouldReport = !ExnSet.is_empty(raiseSet) && !hasRaisesAnnotation;
      if (shouldReport) {
        Log_.info(
          ~loc=vb.vb_pat.pat_loc, ~name="Exception Analysis", (ppf, ()) =>
          Format.fprintf(
            ppf,
            "@{<info>%s@} might raise @{<info>%s@} and is not annotated with @raises",
            name,
            raiseSet
            |> ExnSet.elements
            |> List.map(Exn.toString)
            |> String.concat(" "),
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