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
  path
  |> Path.onOkPath(~whenContainsApply=false, ~f=s =>
       Hashtbl.mem(Lazy.force(whiteTableSideEffects), s)
     );
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
    let path = Current.modulePath^ @ [Common.currentModuleName^];
    name |> addValueDeclaration(~path, ~loc, ~sideEffects=false);
  | _ => ()
  };

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  let oldCurrentBindings = Current.bindings^;
  let oldLastBinding = Current.lastBinding^;
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
      let path = Current.modulePath^ @ [Common.currentModuleName^];
      if (!exists) {
        // This is never toplevel currently
        let isToplevel = oldLastBinding == Location.none;
        let sideEffects = !exprNoSideEffects(vb.vb_expr);
        name |> addValueDeclaration(~isToplevel, ~loc, ~path, ~sideEffects);
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
    | _ => Current.lastBinding^
    };
  Current.bindings := PosSet.add(loc.loc_start, Current.bindings^);
  Current.lastBinding := loc;
  let r = super.Tast_mapper.value_binding(self, vb);
  Current.bindings := oldCurrentBindings;
  Current.lastBinding := oldLastBinding;
  r;
};

let collectExpr = (super, self, e: Typedtree.expression) => {
  let locFrom = e.exp_loc;
  switch (e.exp_desc) {
  | Texp_ident(_path, _, {Types.val_loc: {loc_ghost: false, _} as locTo, _}) =>
    addValueReference(~addFileReference=true, ~locFrom, ~locTo)

  | Texp_field(
      _,
      _,
      {lbl_loc: {Location.loc_start: posTo, loc_ghost: false}, _},
    ) =>
    if (Config.analyzeTypes^) {
      DeadType.addTypeReference(~posTo, ~posFrom=locFrom.loc_start);
    }

  | Texp_construct(
      _,
      {cstr_loc: {Location.loc_start: posTo, loc_ghost} as locTo, cstr_tag},
      _,
    ) =>
    switch (cstr_tag) {
    | Cstr_extension(path, _) =>
      path |> DeadException.markAsUsed(~locFrom, ~locTo)
    | _ => ()
    };
    if (Config.analyzeTypes^ && !loc_ghost) {
      DeadType.addTypeReference(~posTo, ~posFrom=locFrom.loc_start);
    };

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
         if (Config.analyzeTypes^) {
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
  | Mty_functor(_) when isfunc =>
    switch (moduleType |> Compat.getMtyFunctorModuleType) {
    | Some((Some(mtParam), _)) => getSignature(mtParam)
    | _ => []
    }
  | Mty_functor(_) =>
    switch (moduleType |> Compat.getMtyFunctorModuleType) {
    | Some((_, mt)) => getSignature(mt)
    | _ => []
    }
  | _ => []
  };

let rec processSignatureItem =
        (~doTypes, ~doValues, ~path, si: Types.signature_item) =>
  switch (si) {
  | Sig_type(_) when doTypes =>
    let (id, t) = si |> Compat.getSigType;
    if (Config.analyzeTypes^) {
      DeadType.addDeclaration(~typeId=id, ~typeKind=t.type_kind);
    };
  | Sig_value(_) when doValues =>
    let (id, loc, kind) = si |> Compat.getSigValue;
    if (!loc.Location.loc_ghost) {
      let isPrimitive =
        switch (kind) {
        | Val_prim(_) => true
        | _ => false
        };
      if (!isPrimitive || Config.analyzeExternals) {
        Ident.name(id)
        |> Name.create(~isInterface=false)
        |> addValueDeclaration(~sideEffects=false, ~path, ~loc);
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
    let oldModulePath = Current.modulePath^;
    switch (structureItem.str_desc) {
    | Tstr_module({mb_expr, mb_name}) =>
      let hasInterface =
        switch (mb_expr.mod_desc) {
        | Tmod_constraint(_) => true
        | _ => false
        };
      Current.modulePath :=
        [mb_name |> Compat.locGetTxt |> Name.create, ...Current.modulePath^];
      if (hasInterface) {
        switch (mb_expr.mod_type) {
        | Mty_signature(signature) =>
          signature
          |> List.iter(
               processSignatureItem(
                 ~doTypes,
                 ~doValues=false,
                 ~path=Current.modulePath^ @ [Common.currentModuleName^],
               ),
             )
        | _ => ()
        };
      };

    | Tstr_primitive(vd) when doValues && Config.analyzeExternals =>
      let path = Current.modulePath^ @ [Common.currentModuleName^];
      let exists =
        switch (PosHash.find_opt(decls, vd.val_loc.loc_start)) {
        | Some({declKind: Value}) => true
        | _ => false
        };
      if (!exists) {
        vd.val_id
        |> Ident.name
        |> Name.create(~isInterface=false)
        |> addValueDeclaration(~path, ~loc=vd.val_loc, ~sideEffects=false);
      };

    | Tstr_type(_recFlag, typeDeclarations) when doTypes =>
      if (Config.analyzeTypes^) {
        typeDeclarations
        |> List.iter((typeDeclaration: Typedtree.type_declaration) => {
             DeadType.addDeclaration(
               ~typeId=typeDeclaration.typ_id,
               ~typeKind=typeDeclaration.typ_type.type_kind,
             )
           });
      }

    | Tstr_include({incl_mod, incl_type}) =>
      switch (incl_mod.mod_desc) {
      | Tmod_ident(_path, _lid) =>
        let currentPath = Current.modulePath^ @ [Common.currentModuleName^];
        incl_type
        |> List.iter(
             processSignatureItem(
               ~doTypes,
               ~doValues=false, // TODO: also values?
               ~path=currentPath,
             ),
           );
      | _ => ()
      }

    | Tstr_exception(_) =>
      switch (structureItem.str_desc |> Compat.tstrExceptionGet) {
      | Some((id, loc)) =>
        let path = Current.modulePath^ @ [Common.currentModuleName^];
        let name = id |> Ident.name |> Name.create;
        name |> DeadException.add(~path, ~loc, ~strLoc=structureItem.str_loc);
      | None => ()
      }
    | _ => ()
    };
    let result = super.structure_item(self, structureItem);
    Current.modulePath := oldModulePath;
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

  DeadType.TypeDependencies.forceDelayedItems();
  DeadType.TypeDependencies.clear();
};