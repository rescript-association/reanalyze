let posToString = Common.posToString;

module LocSet = Common.LocSet;

module Values = {
  let valueBindingsTable: Hashtbl.t(string, Hashtbl.t(string, Exceptions.t)) =
    Hashtbl.create(15);
  let currentFileTable = ref(Hashtbl.create(1));

  let add = (~name, exceptions) => {
    let path = [name |> Name.create, ...ModulePath.getCurrent().path];
    Hashtbl.replace(
      currentFileTable^,
      path |> Common.Path.toString,
      exceptions,
    );
  };

  let getFromModule = (~moduleName, ~modulePath, path_: Common.Path.t) => {
    let name = path_ @ modulePath |> Common.Path.toString;
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

  let rec findLocal = (~moduleName, ~modulePath, path) => {
    switch (path |> getFromModule(~moduleName, ~modulePath)) {
    | Some(exceptions) => Some(exceptions)
    | None =>
      switch (modulePath) {
      | [] => None
      | [_, ...restModulePath] =>
        path |> findLocal(~moduleName, ~modulePath=restModulePath)
      }
    };
  };

  let findPath = (~moduleName, ~modulePath, path) =>
    switch (path |> findLocal(~moduleName, ~modulePath)) {
    | None =>
      // Search in another file
      switch (path |> List.rev) {
      | [externalModuleName, ...pathRev] =>
        pathRev
        |> List.rev
        |> getFromModule(
             ~moduleName=externalModuleName |> Name.toString,
             ~modulePath=[],
           )
      | [] => None
      }
    | Some(exceptions) => Some(exceptions)
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
    | Call({
        callee: Common.Path.t,
        modulePath: Common.Path.t,
      }) // foo()
    | DoesNotRaise(list(t)) // DoesNotRaise(events) where events come from an expression
    | Raises // raise E

  and t = {
    exceptions: Exceptions.t,
    kind,
    loc: Location.t,
  };

  let rec print = (ppf, event) =>
    switch (event) {
    | {kind: Call({callee, modulePath}), exceptions, loc} =>
      Format.fprintf(
        ppf,
        "%s Call(%s, modulePath:%s) %a@.",
        loc.loc_start |> posToString,
        callee |> Common.Path.toString,
        modulePath |> Common.Path.toString,
        Exceptions.pp(~exnTable=None),
        exceptions,
      )
    | {kind: DoesNotRaise(nestedEvents), loc} =>
      Format.fprintf(
        ppf,
        "%s DoesNotRaise(%a)@.",
        loc.loc_start |> posToString,
        (ppf, ()) => {
          nestedEvents
          |> List.iter(e => {Format.fprintf(ppf, "%a ", print, e)})
        },
        (),
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
    if (Common.Cli.debug^) {
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
    let shrinkExnTable = (exn, loc) =>
      switch (Hashtbl.find_opt(exnTable, exn)) {
      | Some(locSet) =>
        Hashtbl.replace(exnTable, exn, LocSet.remove(loc, locSet))
      | None => ()
      };

    let rec loop = (exnSet, events) =>
      switch (events) {
      | [{kind: Raises, exceptions, loc} as ev, ...rest] =>
        if (Common.Cli.debug^) {
          Log_.item("%a@.", print, ev);
        };
        exceptions |> Exceptions.iter(exn => extendExnTable(exn, loc));
        loop(Exceptions.union(exnSet, exceptions), rest);

      | [{kind: Call({callee, modulePath}), loc} as ev, ...rest] =>
        if (Common.Cli.debug^) {
          Log_.item("%a@.", print, ev);
        };
        let exceptions =
          switch (callee |> Values.findPath(~moduleName, ~modulePath)) {
          | Some(exceptions) => exceptions
          | _ =>
            switch (ExnLib.find(callee)) {
            | Some(exceptions) => exceptions
            | None => Exceptions.empty
            }
          };
        exceptions |> Exceptions.iter(exn => extendExnTable(exn, loc));
        loop(Exceptions.union(exnSet, exceptions), rest);

      | [{kind: DoesNotRaise(nestedEvents), loc} as ev, ...rest] =>
        if (Common.Cli.debug^) {
          Log_.item("%a@.", print, ev);
        };
        let nestedExceptions = loop(Exceptions.empty, nestedEvents);
        if (Exceptions.isEmpty(nestedExceptions)) {
          let name =
            switch (nestedEvents) {
            | [{kind: Call({callee})}, ..._] =>
              callee |> Common.Path.toString
            | _ => "expression"
            };
          Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
            Format.fprintf(
              ppf,
              "@{<info>%s@} does not raise and is annotated with redundant @doesNotRaise",
              name,
            )
          );
        };
        loop(exnSet, rest);

      | [{kind: Catches(nestedEvents), exceptions} as ev, ...rest] =>
        if (Common.Cli.debug^) {
          Log_.item("%a@.", print, ev);
        };
        if (Exceptions.isEmpty(exceptions /* catch-all */)) {
          loop(exnSet, rest);
        } else {
          let nestedExceptions = loop(Exceptions.empty, nestedEvents);
          let newRaises = Exceptions.diff(nestedExceptions, exceptions);
          exceptions
          |> Exceptions.iter(exn =>
               nestedEvents
               |> List.iter(event => shrinkExnTable(exn, event.loc))
             );
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
    loc: Location.t,
    moduleName: string,
    name: string,
    exceptions: Exceptions.t,
  };
  type t = list(check);

  let checks: ref(t) = ref([]);

  let add = (~events, ~exceptions, ~loc, ~moduleName, ~name) =>
    checks := [{events, exceptions, loc, moduleName, name}, ...checks^];

  let doCheck = ({events, exceptions, loc, moduleName, name}) => {
    let (raiseSet, exnTable) = events |> Event.combine(~moduleName);
    let missingAnnotations = Exceptions.diff(raiseSet, exceptions);
    let redundantAnnotations = Exceptions.diff(exceptions, raiseSet);
    if (!Exceptions.isEmpty(missingAnnotations)) {
      Log_.info(~loc, ~name="Exception Analysis", (ppf, ()) =>
        Format.fprintf(
          ppf,
          "@{<info>%s@} might raise%a and is not annotated with @raises%a",
          name,
          Exceptions.pp(~exnTable=Some(exnTable)),
          raiseSet,
          Exceptions.pp(~exnTable=None),
          missingAnnotations,
        )
      );
    };
    if (!Exceptions.isEmpty(redundantAnnotations)) {
      Log_.info(
        ~loc,
        ~name="Exception Analysis",
        (ppf, ()) => {
          let raisesDescription = (ppf, ()) =>
            if (raiseSet |> Exceptions.isEmpty) {
              Format.fprintf(ppf, "raises nothing");
            } else {
              Format.fprintf(
                ppf,
                "might raise%a",
                Exceptions.pp(~exnTable=Some(exnTable)),
                raiseSet,
              );
            };
          Format.fprintf(
            ppf,
            "@{<info>%s@} %a and is annotated with redundant @raises%a",
            name,
            raisesDescription,
            (),
            Exceptions.pp(~exnTable=None),
            redundantAnnotations,
          );
        },
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

    let isDoesNoRaise = expr.exp_attributes |> doesNotRaise;
    let oldEvents = currentEvents^;
    if (isDoesNoRaise) {
      currentEvents := [];
    };

    switch (expr.exp_desc) {
    | Texp_ident(callee_, _, _) =>
      let callee = callee_ |> Common.Path.fromPathT;
      let calleeName = callee |> Common.Path.toString;
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
          {
            Event.exceptions: Exceptions.empty,
            loc,
            kind: Call({callee, modulePath: ModulePath.getCurrent().path}),
          },
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
        [{Event.exceptions, loc, kind: Raises}, ...currentEvents^];
      arg |> snd |> iterExprOpt(self);

    | Texp_apply(
        {exp_desc: Texp_ident(atat, _, _)},
        [arg, (_lbl1, Some({exp_desc: Texp_ident(callee, _, _)}))],
      )
        // Exn(...) |> raise
        when
          atat
          |> Path.name == "Pervasives.|>"
          && callee
          |> Path.name
          |> isRaise =>
      let exceptions = [arg] |> raiseArgs;
      currentEvents :=
        [{Event.exceptions, loc, kind: Raises}, ...currentEvents^];
      arg |> snd |> iterExprOpt(self);

    | Texp_apply({exp_desc: Texp_ident(callee, _, _)} as e, args) =>
      let calleeName = Path.name(callee);
      if (calleeName |> isRaise) {
        let exceptions = args |> raiseArgs;
        currentEvents :=
          [{Event.exceptions, loc, kind: Raises}, ...currentEvents^];
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
            {Event.exceptions, loc, kind: Catches(currentEvents^)},
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
              Event.exceptions: [Exn.matchFailure] |> Exceptions.fromList,
              loc,
              kind: Raises,
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
          {Event.exceptions, loc, kind: Catches(currentEvents^)},
          ...oldEvents,
        ];
      cases |> iterCases(self);

    | _ => super.expr(self, expr) |> ignore
    };

    if (isDoesNoRaise) {
      let nestedEvents = currentEvents^;
      currentEvents :=
        [
          {
            Event.exceptions: Exceptions.empty,
            loc,
            kind: DoesNotRaise(nestedEvents),
          },
          ...oldEvents,
        ];
    };

    expr;
  };

  let structure_item =
      (self: Tast_mapper.mapper, structureItem: Typedtree.structure_item) => {
    let oldModulePath = ModulePath.getCurrent();
    switch (structureItem.str_desc) {
    | Tstr_module({mb_id, mb_loc}) =>
      ModulePath.setCurrent({
        loc: mb_loc,
        path: [
          mb_id |> Compat.moduleIdName |> Name.create,
          ...oldModulePath.path,
        ],
      })
    | _ => ()
    };
    let result = super.structure_item(self, structureItem);
    ModulePath.setCurrent(oldModulePath);
    result;
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
    let processBinding = name => {
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
      exceptionsFromAnnotations |> Values.add(~name);
      let res = super.value_binding(self, vb);

      let moduleName = Common.currentModule^;

      let path = [name |> Name.create];
      let exceptions =
        switch (
          path
          |> Values.findPath(
               ~moduleName,
               ~modulePath=ModulePath.getCurrent().path,
             )
        ) {
        | Some(exceptions) => exceptions
        | _ => Exceptions.empty
        };
      Checks.add(
        ~events=currentEvents^,
        ~exceptions,
        ~loc=vb.vb_pat.pat_loc,
        ~moduleName,
        ~name,
      );

      currentId := oldId;
      currentEvents := oldEvents;

      res;
    };
    switch (vb.vb_pat.pat_desc) {
    | Tpat_any when isToplevel && !vb.vb_loc.loc_ghost => processBinding("_")
    | Tpat_construct({txt: Longident.Lident("()")}, _, _)
        when isToplevel && !vb.vb_loc.loc_ghost =>
      processBinding("()")
    | Tpat_var(id, {loc: {loc_ghost}})
        when (isFunction || isToplevel) && !loc_ghost && !vb.vb_loc.loc_ghost =>
      processBinding(id |> Ident.name)
    | _ => super.value_binding(self, vb)
    };
  };

  Tast_mapper.{...super, expr, value_binding, structure_item};
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
