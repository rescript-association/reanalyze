/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

let whiteListSideEffects = [
  "Pervasives./.",
  "Pervasives.ref",
  "Int64.mul",
  "Int64.neg",
  "Int64.sub",
  "Int64.shift_left",
  "Int64.one",
  "String.length",
];
let whiteTableSideEffects =
  lazy({
    let tbl = Hashtbl.create(11);

    whiteListSideEffects |> List.iter(s => Hashtbl.add(tbl, s, ()));
    tbl;
  });

let pathIsWhitelistedForSideEffects = path => {
  switch (path |> Path.flatten) {
  | `Ok(id, mods) =>
    Hashtbl.mem(
      Lazy.force(whiteTableSideEffects),
      [Ident.name(id), ...mods] |> String.concat("."),
    )
  | `Contains_apply => false
  };
};

let rec exprNoSideEffects = (expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_ident(_)
  | Texp_constant(_) => true
  | Texp_construct(_, _, el) => el |> List.for_all(exprNoSideEffects)
  | Texp_function(_) => true
  | Texp_apply({exp_desc: Texp_ident(path, _, _)}, args)
      when path |> pathIsWhitelistedForSideEffects =>
    args |> List.for_all(((_, eo)) => eo |> exprOptNoSideEffects)
  | Texp_apply(_) => false
  | Texp_sequence(e1, e2) =>
    e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
  | Texp_let(_, vbs, e) =>
    vbs
    |> List.for_all((vb: Typedtree.value_binding) =>
         vb.vb_expr |> exprNoSideEffects
       )
    && e
    |> exprNoSideEffects
  | Texp_record({fields, extended_expression}) =>
    fields
    |> Array.for_all(fieldNoSideEffects)
    && extended_expression
    |> exprOptNoSideEffects
  | Texp_assert(_) => false
  | Texp_match(_) =>
    let (e, cases, partial) = expr.exp_desc |> Compat.getTexpMatch;
    partial == Total
    && e
    |> exprNoSideEffects
    && cases
    |> List.for_all(caseNoSideEffects);
  | Texp_letmodule(_) => false
  | Texp_lazy(e) => e |> exprNoSideEffects
  | Texp_try(e, cases) =>
    e |> exprNoSideEffects && cases |> List.for_all(caseNoSideEffects)
  | Texp_tuple(el) => el |> List.for_all(exprNoSideEffects)
  | Texp_variant(_lbl, eo) => eo |> exprOptNoSideEffects
  | Texp_field(e, _lid, _ld) => e |> exprNoSideEffects
  | Texp_setfield(_) => false
  | Texp_array(el) => el |> List.for_all(exprNoSideEffects)
  | Texp_ifthenelse(e1, e2, eo) =>
    e1
    |> exprNoSideEffects
    && e2
    |> exprNoSideEffects
    && eo
    |> exprOptNoSideEffects
  | Texp_while(e1, e2) => e1 |> exprNoSideEffects && e2 |> exprNoSideEffects
  | Texp_for(_id, _pat, e1, e2, _dir, e3) =>
    e1
    |> exprNoSideEffects
    && e2
    |> exprNoSideEffects
    && e3
    |> exprNoSideEffects
  | Texp_send(_) => false
  | Texp_new(_) => true
  | Texp_instvar(_) => true
  | Texp_setinstvar(_) => false
  | Texp_override(_) => false
  | Texp_letexception(_ec, e) => e |> exprNoSideEffects
  | Texp_object(_) => true
  | Texp_pack(_) => false
  | Texp_unreachable => false
  | Texp_extension_constructor(_) when true => true
  | _ => true // on ocaml 4.08: Texp_letop | Texp_open
  }
and exprOptNoSideEffects = eo =>
  switch (eo) {
  | None => true
  | Some(e) => e |> exprNoSideEffects
  }
and fieldNoSideEffects =
    ((_ld, rld): (_, Typedtree.record_label_definition)) =>
  switch (rld) {
  | Kept(_typeExpr) => true
  | Overridden(_lid, e) => e |> exprNoSideEffects
  }
and caseNoSideEffects = ({c_guard, c_rhs}: Typedtree.case) => {
  c_guard |> exprOptNoSideEffects && c_rhs |> exprNoSideEffects;
};

let checkAnyValueBindingWithNoSideEffects =
    (
      {vb_pat: {pat_desc}, vb_expr: expr, vb_loc: loc}: Typedtree.value_binding,
    ) =>
  switch (pat_desc) {
  | Tpat_any when exprNoSideEffects(expr) && !loc.loc_ghost =>
    let name = "_" |> Name.create(~isInterface=false);
    let path = currentModulePath^ @ [currentModuleName^];
    addValueDeclaration(~path, ~loc, ~sideEffects=false, name);
  | _ => ()
  };

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  let oldCurrentBindings = currentBindings^;
  let oldLastBinding = lastBinding^;
  checkAnyValueBindingWithNoSideEffects(vb);
  let loc =
    switch (vb.vb_pat.pat_desc) {
    | Tpat_var(id, {loc: {loc_start, loc_ghost} as loc})
        when !loc_ghost && !vb.vb_loc.loc_ghost =>
      let name = Ident.name(id) |> Name.create(~isInterface=false);
      let exists =
        switch (PosHash.find_opt(decls, loc_start)) {
        | Some({declKind: Value}) => true
        | _ => false
        };
      let path = currentModulePath^ @ [currentModuleName^];
      if (!exists) {
        let sideEffects = !exprNoSideEffects(vb.vb_expr);
        addValueDeclaration(~path, ~loc, ~sideEffects, name);
      };
      switch (PosHash.find_opt(decls, loc_start)) {
      | None => ()
      | Some(decl) =>
        // Value bindings contain the correct location for the entire declaration: update final position.
        // The previous value was taken from the signature, which only has positions for the id.

        let sideEffects = !exprNoSideEffects(vb.vb_expr);
        PosHash.replace(
          decls,
          loc_start,
          {
            ...decl,
            posEnd: vb.vb_loc.loc_end,
            posStart: vb.vb_loc.loc_start,
            sideEffects,
          },
        );
      };
      loc;
    | _ => getLastBinding()
    };
  currentBindings := PosSet.add(loc.loc_start, currentBindings^);
  lastBinding := loc;
  let r = super.Tast_mapper.value_binding(self, vb);
  currentBindings := oldCurrentBindings;
  lastBinding := oldLastBinding;
  r;
};

let collectExpr = (super, self, e: Typedtree.expression) => {
  let locFrom = e.exp_loc;
  switch (e.exp_desc) {
  | Texp_ident(path, _, {Types.val_loc: {loc_ghost: true}}) =>
    // When the ppx uses a dummy location, find the original location.
    let moduleName =
      switch (path) {
      | Pident(_) => currentModuleName^
      | _ => path |> Path.head |> Ident.name |> Name.create
      };

    let valueName = path |> Path.last |> Name.create(~isInterface=false);
    switch (getPosOfValue(~moduleName, ~valueName)) {
    | Some(posName) =>
      addValueReference(
        ~addFileReference=true,
        ~locFrom,
        ~locTo={loc_start: posName, loc_end: posName, loc_ghost: false},
      )
    | None => ()
    };

  | Texp_ident(_path, _, {Types.val_loc: {loc_ghost: false, _} as locTo, _}) =>
    addValueReference(~addFileReference=true, ~locFrom, ~locTo)

  | Texp_apply(
      {exp_desc: Texp_ident(path, _, {Types.val_loc: locTo, _})},
      [(_lbl, Some({exp_desc: Texp_constant(Const_string(s, _))}))],
    )
      when
        path
        |> Path.name == "JSResource.jSResource"
        && Filename.check_suffix(s, ".bs") =>
    let moduleName =
      Filename.chop_extension(s) |> Name.create(~isInterface=false);
    switch (
      getPosOfValue(
        ~moduleName,
        ~valueName="make" |> Name.create(~isInterface=false),
      )
    ) {
    | None => ()
    | Some(posMake) =>
      if (verbose) {
        Log_.item(
          "lazyLoad %s(%s) %s defined in %s@.",
          path |> Path.name,
          moduleName |> Name.toString,
          locTo.loc_start |> posToString,
          posMake |> posToString,
        );
      };
      let locMake = {
        Location.loc_start: posMake,
        loc_end: posMake,
        loc_ghost: false,
      };
      addValueReference(~addFileReference=true, ~locFrom, ~locTo=locMake);
    };

  | Texp_apply(
      {exp_desc: Texp_ident(path, _, _)},
      [
        (_, Some({exp_desc: Texp_constant(Const_string(sTrue, _))})),
        (_, Some({exp_desc: Texp_constant(Const_string(sFalse, _))})),
      ],
    )
      when
        path
        |> Path.name == "J.unsafe_expr"
        && Filename.check_suffix(sTrue, ".bs")
        && Filename.check_suffix(sFalse, ".bs") =>
    let moduleTrue =
      Filename.chop_extension(sTrue) |> Name.create(~isInterface=false);
    let moduleFalse =
      Filename.chop_extension(sFalse) |> Name.create(~isInterface=false);

    let positionsTrue = getDeclPositions(~moduleName=moduleTrue);
    let positionsFalse = getDeclPositions(~moduleName=moduleFalse);
    let allPositions = PosSet.union(positionsTrue, positionsFalse);

    if (verbose) {
      Log_.item(
        "requireCond  true:%s false:%s allPositions:[%s]@.",
        moduleTrue |> Name.toString,
        moduleFalse |> Name.toString,
        allPositions
        |> PosSet.elements
        |> List.map(posToString)
        |> String.concat(", "),
      );
    };

    allPositions
    |> PosSet.iter(pos => {
         let posFrom = {...Lexing.dummy_pos, pos_fname: currentModule^};
         let locFrom = {
           Location.loc_start: posFrom,
           loc_end: posFrom,
           loc_ghost: false,
         };
         addValueReference(
           ~addFileReference=false,
           ~locTo={Location.loc_start: pos, loc_end: pos, loc_ghost: false},
           ~locFrom,
         );
       });

  | Texp_field(
      _,
      _,
      {lbl_loc: {Location.loc_start: posTo, loc_ghost: false}, _},
    )
  | Texp_construct(
      _,
      {cstr_loc: {Location.loc_start: posTo, loc_ghost: false}, _},
      _,
    ) =>
    if (analyzeTypes^) {
      DeadType.addTypeReference(~posTo, ~posFrom=locFrom.loc_start);
    }

  | _ => ()
  };
  super.Tast_mapper.expr(self, e);
};

let collectPattern = (super, self, pat: Typedtree.pattern) => {
  let posFrom = pat.pat_loc.loc_start;
  switch (pat.pat_desc) {
  | Tpat_record(cases, _clodsedFlag) =>
    cases
    |> List.iter(((_loc, {Types.lbl_loc: {loc_start: posTo}}, _pat)) =>
         if (analyzeTypes^) {
           DeadType.addTypeReference(~posFrom, ~posTo);
         }
       )
  | _ => ()
  };
  super.Tast_mapper.pat(self, pat);
};

let rec getSignature = (~isfunc=false, moduleType: Types.module_type) =>
  switch (moduleType) {
  | Mty_signature(signature) => signature
  | Mty_functor(_, tOpt, _) when isfunc =>
    switch (tOpt) {
    | None => []
    | Some(moduleType) => getSignature(moduleType)
    }
  | Mty_functor(_, _, moduleType) => getSignature(moduleType)
  | _ => []
  };

let rec processSignatureItem =
        (~doTypes, ~doValues, ~path, si: Types.signature_item) =>
  switch (si) {
  | Sig_type(_) when doTypes =>
    let (id, t) = si |> Compat.getSigType;
    if (analyzeTypes^) {
      t
      |> DeadType.addDeclaration(~isInterface=true, ~typId=id, ~typKind=None);
    };
  | Sig_value(_) when doValues =>
    let (id, loc, kind) = si |> Compat.getSigValue;
    if (!loc.Location.loc_ghost) {
      let isPrimitive =
        switch (kind) {
        | Val_prim(_) => true
        | _ => false
        };
      if (!isPrimitive || analyzeExternals) {
        addValueDeclaration(
          ~sideEffects=false,
          ~path,
          ~loc,
          Ident.name(id) |> Name.create(~isInterface=false),
        );
      };
    };
  | Sig_module(_)
  | Sig_modtype(_) =>
    switch (si |> Compat.getSigModuleModtype) {
    | Some((id, moduleType)) =>
      let collect =
        switch (si) {
        | Sig_modtype(_) => false
        | _ => true
        };
      if (collect) {
        getSignature(moduleType)
        |> List.iter(
             processSignatureItem(
               ~doTypes,
               ~doValues,
               ~path=[id |> Ident.name |> Name.create, ...path],
             ),
           );
      };
    | None => ()
    }
  | _ => ()
  };

/* Traverse the AST */
let traverseStructure = (~doTypes, ~doValues) => {
  /* Tast_mapper */
  let super = Tast_mapper.default;

  let expr = (self, e) => e |> collectExpr(super, self);
  let pat = (self, p) => p |> collectPattern(super, self);
  let value_binding = (self, vb) => vb |> collectValueBinding(super, self);
  let structure_item = (self, structureItem: Typedtree.structure_item) => {
    let oldModulePath = currentModulePath^;
    switch (structureItem.str_desc) {
    | Tstr_module({mb_expr, mb_name}) =>
      let hasInterface =
        switch (mb_expr.mod_desc) {
        | Tmod_constraint(_) => true
        | _ => false
        };
      currentModulePath := [mb_name.txt |> Name.create, ...currentModulePath^];
      if (hasInterface) {
        switch (mb_expr.mod_type) {
        | Mty_signature(signature) =>
          signature
          |> List.iter(
               processSignatureItem(
                 ~doTypes,
                 ~doValues=false,
                 ~path=currentModulePath^ @ [currentModuleName^],
               ),
             )
        | _ => ()
        };
      };
    | Tstr_primitive(vd) when doValues && analyzeExternals =>
      let path = currentModulePath^ @ [currentModuleName^];
      let exists =
        switch (PosHash.find_opt(decls, vd.val_loc.loc_start)) {
        | Some({declKind: Value}) => true
        | _ => false
        };
      if (!exists) {
        addValueDeclaration(
          ~path,
          ~loc=vd.val_loc,
          ~sideEffects=false,
          vd.val_id |> Ident.name |> Name.create(~isInterface=false),
        );
      };
    | Tstr_type(_recFlag, typeDeclarations) when doTypes =>
      if (analyzeTypes^) {
        typeDeclarations
        |> List.iter((typeDeclaration: Typedtree.type_declaration) => {
             typeDeclaration.typ_type
             |> DeadType.addDeclaration(
                  ~isInterface=false,
                  ~typId=typeDeclaration.typ_id,
                  ~typKind=Some(typeDeclaration.typ_type.type_kind),
                );
           });
      }
    | Tstr_include({incl_mod, incl_type, incl_loc, incl_attributes}) =>
      switch (incl_mod.mod_desc) {
      | Tmod_ident(path, lid) =>
        let pathName = {
          switch (path |> Path.flatten) {
          | `Ok(id, mods) =>
            [Ident.name(id), ...mods] |> String.concat(".")
          | `Contains_apply => "Apply!!!"
          };
        };
        Log_.item("XXX %s@.", pathName);

      | _ => ()
      };
      // TODO: anything special?
      ();
    | _ => ()
    };
    let result = super.structure_item(self, structureItem);
    currentModulePath := oldModulePath;
    result;
  };
  Tast_mapper.{...super, expr, pat, structure_item, value_binding};
};

/* Merge a location's references to another one's */
let processValueDependency =
    (
      (
        {
          val_loc:
            {loc_start: {pos_fname: fnTo} as posTo, loc_ghost: ghost1} as locTo,
        }: Types.value_description,
        {
          val_loc:
            {loc_start: {pos_fname: fnFrom} as posFrom, loc_ghost: ghost2} as locFrom,
        }: Types.value_description,
      ),
    ) =>
  if (!ghost1 && !ghost2 && posTo != posFrom) {
    let addFileReference = fileIsImplementationOf(fnTo, fnFrom);
    addValueReference(~addFileReference, ~locFrom, ~locTo);
  };

let processTypeDependency =
    (
      (
        {loc_start: posTo, loc_ghost: ghost1}: Location.t,
        {loc_start: posFrom, loc_ghost: ghost2}: Location.t,
      ),
    ) =>
  if (!ghost1 && !ghost2 && posTo != posFrom) {
    DeadType.addTypeReference(~posTo, ~posFrom);
  };

let processStructure =
    (
      ~cmt_value_dependencies,
      ~doTypes,
      ~doValues,
      structure: Typedtree.structure,
    ) => {
  let traverseStructure = traverseStructure(~doTypes, ~doValues);
  structure |> traverseStructure.structure(traverseStructure) |> ignore;

  let valueDependencies = cmt_value_dependencies |> List.rev;

  valueDependencies |> List.iter(processValueDependency);

  DeadType.typeDependencies^ |> List.iter(processTypeDependency);

  DeadType.typeDependencies := [];
};