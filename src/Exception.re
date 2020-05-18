let posToString = Common.posToString;

module LocSet = Common.LocSet;

module Values = {
  let valueBindingsTable: Hashtbl.t(string, Hashtbl.t(string, Exceptions.t)) =
    Hashtbl.create(15);
  let currentFileTable = ref(Hashtbl.create(1));

  let add = (~id, exceptions) =>
    Hashtbl.replace(currentFileTable^, Ident.name(id), exceptions);

  let getFromModule = (~moduleName, name) => {
    switch (
      Hashtbl.find_opt(
        valueBindingsTable,
        String.capitalize_ascii(moduleName),
      )
    ) {
    | Some(tbl) => Hashtbl.find_opt(tbl, name)
    | None =>
      switch (
        Hashtbl.find_opt(
          valueBindingsTable,
          String.uncapitalize_ascii(moduleName),
        )
      ) {
      | Some(tbl) => Hashtbl.find_opt(tbl, name)
      | None => None
      }
    };
  };

  let findId = (~moduleName, id) =>
    id |> Ident.name |> getFromModule(~moduleName);

  let findPath = (~moduleName, path) => {
    switch (path |> Path.name |> getFromModule(~moduleName)) {
    | Some(exceptions) => Some(exceptions)
    | None =>
      switch (path) {
      | Pdot(_) =>
        let (moduleName, valuePath) =
          switch (path |> Path.flatten) {
          | `Ok(id, mods) => (Ident.name(id), mods |> String.concat("."))
          | `Contains_apply => ("", "")
          };
        valuePath |> getFromModule(~moduleName);
      | _ => None
      }
    };
  };

  let newCmt = () => {
    currentFileTable := Hashtbl.create(15);
    Hashtbl.replace(
      valueBindingsTable,
      Common.currentModule^,
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
    exceptions: Exceptions.t,
    kind,
    loc: Location.t,
  };

  let rec print = (ppf, event) =>
    switch (event) {
    | {kind: Call(path), exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Call(%s) %a@.",
        loc.loc_start |> posToString,
        path |> Path.name,
        Exceptions.pp(~exnTable=None),
        exceptions,
      )
    | {kind: Raises, exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s raises %a@.",
        loc.loc_start |> posToString,
        Exceptions.pp(~exnTable=None),
        exceptions,
      )
    | {kind: Catches(nestedEvents), exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Catches exceptions:%a nestedEvents:%a@.",
        loc.loc_start |> posToString,
        Exceptions.pp(~exnTable=None),
        exceptions,
        (ppf, ()) => {
          nestedEvents
          |> List.iter(e => {Format.fprintf(ppf, "%a ", print, e)})
        },
        (),
      )
    };

  let combine = (~moduleName, events) => {
    if (Common.debug^) {
      Log_.item("@.");
      Log_.item("Events combine: #events %d@.", events |> List.length);
    };
    let exnTable = Hashtbl.create(1);
    let extendExnTable = (exn, loc) =>
      switch (Hashtbl.find_opt(exnTable, exn)) {
      | Some(locSet) =>
        Hashtbl.replace(exnTable, exn, LocSet.add(loc, locSet))
      | None => Hashtbl.replace(exnTable, exn, LocSet.add(loc, LocSet.empty))
      };

    let rec loop = (exnSet, events) =>
      switch (events) {
      | [{kind: Raises, exceptions, loc} as ev, ...rest] =>
        if (Common.debug^) {
          Log_.item("%a@.", print, ev);
        };
        exceptions |> Exceptions.iter(exn => extendExnTable(exn, loc));
        loop(Exceptions.union(exnSet, exceptions), rest);
      | [{kind: Call(path), loc} as ev, ...rest] =>
        if (Common.debug^) {
          Log_.item("%a@.", print, ev);
        };
        switch (path |> Values.findPath(~moduleName)) {
        | Some(exceptions) when !Exceptions.isEmpty(exceptions) =>
          exceptions |> Exceptions.iter(exn => extendExnTable(exn, loc));
          loop(Exceptions.union(exnSet, exceptions), rest);
        | _ =>
          switch (ExnLib.find(path)) {
          | Some(exceptions) =>
            exceptions |> Exceptions.iter(exn => extendExnTable(exn, loc));
            loop(Exceptions.union(exnSet, exceptions), rest);
          | None => loop(exnSet, rest)
          }
        };
      | [{kind: Catches(nestedEvents), exceptions} as ev, ...rest] =>
        if (Common.debug^) {
          Log_.item("%a@.", print, ev);
        };
        if (Exceptions.isEmpty(exceptions /* catch-all */)) {
          loop(exnSet, rest);
        } else {
          let nestedExnSet = loop(Exceptions.empty, nestedEvents);
          let newRaises = Exceptions.diff(nestedExnSet, exceptions);
          loop(Exceptions.union(exnSet, newRaises), rest);
        };
      | [] => exnSet
      };
    let exnSet = loop(Exceptions.empty, events);
    (exnSet, exnTable);
  };
};

module Checks = {
  type check = {
    events: list(Event.t),
    id: Ident.t,
    loc: Location.t,
    moduleName: string,
    exceptions: Exceptions.t,
  };
  type t = list(check);

  let checks: ref(t) = ref([]);

  let add = (~events, ~exceptions, ~id, ~loc, ~moduleName) =>
    checks := [{events, exceptions, id, loc, moduleName}, ...checks^];

  let doCheck = ({events, exceptions, id, loc, moduleName}) => {
    let (raiseSet, exnTable) = events |> Event.combine(~moduleName);
    let missingAnnotations = Exceptions.diff(raiseSet, exceptions);
    let redundantAnnotations = Exceptions.diff(exceptions, raiseSet);
    if (!Exceptions.isEmpty(missingAnnotations)) {
      Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "@{<info>%s@} might raise%a and is not annotated with @raises%a",
          id |> Ident.name,
          Exceptions.pp(~exnTable=Some(exnTable)),
          raiseSet,
          Exceptions.pp(~exnTable=None),
          missingAnnotations,
        )
      );
    };
    if (!Exceptions.isEmpty(redundantAnnotations)) {
      Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "@{<info>%s@} might raise%a and is annotated with redundant @raises%a",
          id |> Ident.name,
          Exceptions.pp(~exnTable=Some(exnTable)),
          raiseSet,
          Exceptions.pp(~exnTable=None),
          redundantAnnotations,
        )
      );
    };
  };

  let doChecks = () => {
    checks^ |> List.rev |> List.iter(doCheck);
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
             Exceptions.add(Exn.fromLid(lid), acc)
           | _ => acc
           },
         Exceptions.empty,
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

  let isRaise = s => {
    s == "Pervasives.raise"
    || s == "Pervasives.raise_notracee"
    || s == "Stdlib.raise"
    || s == "Stdlib.raise_notracee";
  };
  let raiseArgs = args =>
    switch (args) {
    | [(_, Some({Typedtree.exp_desc: Texp_construct(lid, _, _)}))] =>
      [Exn.fromLid(lid)] |> Exceptions.fromList
    | [(_, Some({Typedtree.exp_desc: Texp_ident(_)}))] =>
      [Exn.fromString("genericException")] |> Exceptions.fromList
    | _ => [Exn.fromString("TODO_from_raise1")] |> Exceptions.fromList
    };

  let doesNotRaise = attributes =>
    attributes
    |> Annotation.getAttributePayload(s =>
         s == "doesNotRaise"
         || s == "doesnotraise"
         || s == "DoesNoRaise"
         || s == "doesNotraise"
         || s == "doNotRaise"
         || s == "donotraise"
         || s == "DoNoRaise"
         || s == "doNotraise"
       )
    != None;

  let expr = (self: Tast_mapper.mapper, expr: Typedtree.expression) => {
    let loc = expr.exp_loc;
    switch (expr.exp_desc) {
    | _ when expr.exp_attributes |> doesNotRaise => ()

    | Texp_ident(callee, _, _) =>
      let calleeName = callee |> Path.name;
      if (calleeName |> isRaise) {
        Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
          Format.fprintf(
            ppf,
            "@{<info>%s@} can be analyzed only if called direclty",
            calleeName,
          )
        );
      };
      currentEvents :=
        [
          {Event.kind: Call(callee), loc, exceptions: Exceptions.empty},
          ...currentEvents^,
        ];

    | Texp_apply(
        {exp_desc: Texp_ident(atat, _, _)},
        [(_lbl1, Some({exp_desc: Texp_ident(callee, _, _)})), arg],
      )
        // raise @@ Exn(...)
        when
          atat
          |> Path.name == "Pervasives.@@"
          && callee
          |> Path.name
          |> isRaise =>
      let exceptions = [arg] |> raiseArgs;
      currentEvents :=
        [{Event.kind: Raises, loc, exceptions}, ...currentEvents^];
      arg |> snd |> iterExprOpt(self);

    | Texp_apply(
        {exp_desc: Texp_ident(atat, _, _)},
        [arg, (_lbl1, Some({exp_desc: Texp_ident(callee, _, _)}))],
      )
        // Exn(...) |> raises
        when
          atat
          |> Path.name == "Pervasives.|>"
          && callee
          |> Path.name
          |> isRaise =>
      let exceptions = [arg] |> raiseArgs;
      currentEvents :=
        [{Event.kind: Raises, loc, exceptions}, ...currentEvents^];
      arg |> snd |> iterExprOpt(self);

    | Texp_apply({exp_desc: Texp_ident(callee, _, _)} as e, args) =>
      let calleeName = Path.name(callee);
      if (calleeName |> isRaise) {
        let exceptions = args |> raiseArgs;
        currentEvents :=
          [{Event.kind: Raises, loc, exceptions}, ...currentEvents^];
      } else {
        e |> iterExpr(self);
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
              exceptions: [Exn.matchFailure] |> Exceptions.fromList,
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

  let value_binding = (self: Tast_mapper.mapper, vb: Typedtree.value_binding) => {
    let oldId = currentId^;
    let oldEvents = currentEvents^;
    let isFunction =
      switch (vb.vb_expr.exp_desc) {
      | Texp_function(_) => true
      | _ => false
      };
    let isToplevel = currentId^ == "";
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, {loc: {loc_ghost}})
        when (isFunction || isToplevel) && !loc_ghost && !vb.vb_loc.loc_ghost =>
      let name = Ident.name(id);

      currentId := name;
      currentEvents := [];

      let raisesAnnotationPayload =
        vb.vb_attributes
        |> Annotation.getAttributePayload(s => s == "raises" || s == "raise");
      let rec getExceptions = payload =>
        switch (payload) {
        | Annotation.StringPayload(s) =>
          [Exn.fromString(s)] |> Exceptions.fromList
        | Annotation.ConstructPayload(s) =>
          [Exn.fromString(s)] |> Exceptions.fromList
        | Annotation.IdentPayload(s) =>
          [Exn.fromString(s |> Longident.flatten |> String.concat("."))]
          |> Exceptions.fromList
        | Annotation.TuplePayload(tuple) =>
          tuple
          |> List.map(payload =>
               payload |> getExceptions |> Exceptions.toList
             )
          |> List.concat
          |> Exceptions.fromList
        | _ => Exceptions.empty
        };
      let exceptionsFromAnnotations =
        switch (raisesAnnotationPayload) {
        | None => Exceptions.empty
        | Some(payload) => payload |> getExceptions
        };
      exceptionsFromAnnotations |> Values.add(~id);
      let res = super.value_binding(self, vb);

      let moduleName = Common.currentModule^;
      let exceptions =
        switch (id |> Values.findId(~moduleName)) {
        | Some(exceptions) => exceptions
        | _ => Exceptions.empty
        };
      Checks.add(
        ~events=currentEvents^,
        ~exceptions,
        ~id,
        ~loc=vb.vb_pat.pat_loc,
        ~moduleName,
      );

      currentId := oldId;
      currentEvents := oldEvents;

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

let reportResults = (~ppf as _) => Checks.doChecks();