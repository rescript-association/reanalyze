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
       |> List.iter(((s, e)) =>
            Hashtbl.add(table, name ++ "." ++ s, e |> ExnSet.of_list)
          )
     );

  table;
};

module Exceptions = {
  type t = ExnSet.t;
  let toString = exceptions =>
    exceptions
    |> ExnSet.elements
    |> List.map(Exn.toString)
    |> String.concat(" ");
};

module Values = {
  let valueBindingsTable: Hashtbl.t(string, Hashtbl.t(string, Exceptions.t)) =
    Hashtbl.create(15);
  let currentFileTable = ref(Hashtbl.create(1));
  let add = (~id, exceptions) =>
    Hashtbl.replace(currentFileTable^, Ident.name(id), exceptions);
  let findId = id => Hashtbl.find_opt(currentFileTable^, id |> Ident.name);
  let findPath = path => {
    let name = Path.name(path);
    switch (Hashtbl.find_opt(currentFileTable^, name)) {
    | Some(exceptions) => Some(exceptions)
    | None =>
      switch (path) {
      | Pdot(_) =>
        let (moduleName, valuePath) =
          switch (path |> Path.flatten) {
          | `Ok(id, mods) => (Ident.name(id), mods |> String.concat("."))
          | `Contains_apply => ("", "")
          };
        switch (Hashtbl.find_opt(valueBindingsTable, moduleName)) {
        | Some(tbl) =>
          let res = Hashtbl.find_opt(tbl, valuePath);
          Log_.item(
            "XXX module:%s value:%s found:%b@.",
            moduleName,
            valuePath,
            res != None,
          );
          res;
        | None => None
        };
      | _ => None
      }
    };
  };

  let newCmt = () => {
    currentFileTable := Hashtbl.create(15);
    Hashtbl.replace(
      valueBindingsTable,
      DeadCommon.currentModule^,
      currentFileTable^,
    );
  };
};

module Event = {
  type kind =
    | Catches(list(t)) // with | E => ...
    | Call(Path.t) // foo()
    | Raises // raise E

  and t = {
    exceptions: ExnSet.t,
    kind,
    loc: Location.t,
  };

  let rec print = (ppf, event) =>
    switch (event) {
    | {kind: Call(path), exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Call(%s) %s@.",
        loc.loc_start |> posToString,
        path |> Path.name,
        exceptions |> Exceptions.toString,
      )
    | {kind: Raises, exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s raises %s@.",
        loc.loc_start |> posToString,
        exceptions |> Exceptions.toString,
      )
    | {kind: Catches(nestedEvents), exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Catches exceptions:%s nestedEvents:%a@.",
        loc.loc_start |> posToString,
        exceptions |> Exceptions.toString,
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
      | [{kind: Raises, exceptions} as ev, ...rest] =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        loop(ExnSet.union(acc, exceptions), rest);
      | [{kind: Call(path)} as ev, ...rest] =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        switch (path |> Values.findPath) {
        | Some(exceptions) when !ExnSet.is_empty(exceptions) =>
          loop(ExnSet.union(acc, exceptions), rest)
        | _ =>
          switch (Hashtbl.find_opt(raisesLibTable, path |> Path.name)) {
          | Some(exceptions) => loop(ExnSet.union(acc, exceptions), rest)
          | None => loop(acc, rest)
          }
        };
      | [{kind: Catches(_), exceptions} as ev, ...rest]
          when ExnSet.is_empty(exceptions) /* catch-all */ =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        loop(acc, rest);
      | [{kind: Catches(nestedEvents), exceptions} as ev, ...rest] =>
        if (debug^) {
          Log_.item("%a@.", print, ev);
        };
        let nestedExnSet = loop(ExnSet.empty, nestedEvents);
        let newRaises = ExnSet.diff(nestedExnSet, exceptions);
        loop(ExnSet.union(acc, newRaises), rest);
      | [] => acc
      };
    loop(ExnSet.empty, events);
  };
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
           | Typedtree.Tpat_construct(lid, _, _) =>
             ExnSet.add(Exn.fromLid(lid), acc)
           | _ => acc
           },
         ExnSet.empty,
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
          | [(_, Some({exp_desc: Texp_construct(lid, _, _)}))] =>
            Exn.fromLid(lid) |> ExnSet.singleton
          | _ => Exn.fromString("TODO_from_raise") |> ExnSet.singleton
          };
        currentEvents :=
          [{Event.kind: Raises, loc, exceptions}, ...currentEvents^];
      } else {
        currentEvents :=
          [
            {Event.kind: Call(callee), loc, exceptions: ExnSet.empty},
            ...currentEvents^,
          ];
      };

      args |> List.iter(((_, eOpt)) => eOpt |> iterExprOpt(self));

    | Texp_match(_) =>
      let (e, cases, partial) = Compat.getTexpMatch(expr.exp_desc);
      let exceptionPatterns = expr.exp_desc |> Compat.texpMatchGetExceptions;
      let exceptions = exceptionPatterns |> exceptionsOfPatterns;
      if (exceptionPatterns != []) {
        let oldEvents = currentEvents^;
        currentEvents := [];
        e |> iterExpr(self);
        currentEvents :=
          [
            {Event.kind: Catches(currentEvents^), loc, exceptions},
            ...oldEvents,
          ];
      } else {
        e |> iterExpr(self);
      };
      cases |> iterCases(self);
      if (partial == Partial) {
        currentEvents :=
          [
            {
              Event.kind: Raises,
              loc,
              exceptions: Exn.matchFailure |> ExnSet.singleton,
            },
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
      currentEvents :=
        [
          {Event.kind: Catches(currentEvents^), loc, exceptions},
          ...oldEvents,
        ];
      cases |> iterCases(self);

    | _ => super.expr(self, expr) |> ignore
    };
    expr;
  };

  let nested = true;

  let report = (~id, ~loc) => {
    let raisesAnnotations =
      switch (id |> Values.findId) {
      | Some(exceptions) => exceptions
      | _ => ExnSet.empty
      };
    let raiseSet = currentEvents^ |> Event.combine;
    let missingAnnotations = ExnSet.diff(raiseSet, raisesAnnotations);
    let redundantAnnotations = ExnSet.diff(raisesAnnotations, raiseSet);
    if (!ExnSet.is_empty(missingAnnotations)) {
      Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "@{<info>%s@} might raise @{<info>%s@} and is not annotated with @raises %s",
          id |> Ident.name,
          raiseSet |> Exceptions.toString,
          missingAnnotations |> Exceptions.toString,
        )
      );
    };
    if (!ExnSet.is_empty(redundantAnnotations)) {
      Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "@{<info>%s@} might raise @{<info>%s@} and is annotated with redundant @raises %s",
          id |> Ident.name,
          raiseSet |> Exceptions.toString,
          redundantAnnotations |> Exceptions.toString,
        )
      );
    };
  };

  let value_binding = (self: Tast_mapper.mapper, vb: Typedtree.value_binding) => {
    let oldId = currentId^;
    let oldEvents = currentEvents^;
    let shouldUpdateCurrent = nested || currentId^ == "";
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, {loc: {loc_ghost}})
        when !loc_ghost && !vb.vb_loc.loc_ghost =>
      let name = Ident.name(id);
      if (shouldUpdateCurrent) {
        currentId := name;
        currentEvents := [];
      };
      let raisesAnnotationPayload =
        vb.vb_attributes |> Annotation.getAttributePayload((==)("raises"));
      let rec getExceptions = payload =>
        switch (payload) {
        | Annotation.StringPayload(s) => Exn.fromString(s) |> ExnSet.singleton
        | Annotation.ConstructPayload(s) =>
          Exn.fromString(s) |> ExnSet.singleton
        | Annotation.TuplePayload(tuple) =>
          tuple
          |> List.map(payload => payload |> getExceptions |> ExnSet.elements)
          |> List.concat
          |> ExnSet.of_list
        | _ => ExnSet.empty
        };
      let exceptions =
        switch (raisesAnnotationPayload) {
        | None => ExnSet.empty
        | Some(payload) => payload |> getExceptions
        };
      exceptions |> Values.add(~id);
      let res = super.value_binding(self, vb);

      report(~id, ~loc=vb.vb_pat.pat_loc);

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
  | Implementation(structure) =>
    Values.newCmt();
    structure |> processStructure;
  | _ => ()
  };

let reportResults = (~ppf as _) => ();