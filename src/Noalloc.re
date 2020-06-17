let processCallee = (~def, ~loc, callee) =>
  switch (callee) {
  | Path.Pident(id) =>
    let id = Ident.name(id);
    switch (Il.findDef(~id)) {
    | Some(defCallee) => def |> Il.Def.emit(~instr=Il.Call(defCallee.id))
    | None =>
      Log_.info(~count=false, ~loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Callee not recognized: %s", id)
      );
      assert(false);
    };
  | _ =>
    switch (callee |> Path.name) {
    | "Pervasives.+"
    | "Stdlib.+" => def |> Il.Def.emit(~instr=Il.I32Add)
    | name =>
      Log_.info(~count=false, ~loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Callee not recognized: %s", name)
      );
      assert(false);
    }
  };

let rec processFunPatId = (~env, ~def, ~id, ~offset, ~typ: Types.type_expr) => {
  switch (typ.desc) {
  | Ttuple(ts) =>
    let (newEnv, newOffset, scopesRev) =
      ts
      |> List.fold_left(
           ((e, o, scopes), t) => {
             let (newEnv, newOffset, newScope) =
               processFunPatId(~env=e, ~def, ~id, ~offset=o, ~typ=t);
             (newEnv, newOffset, [newScope, ...scopes]);
           },
           (env, offset, []),
         );
    let scope = Il.Env.Tuple(scopesRev |> List.rev);
    (
      newEnv |> Il.Env.addFunctionParameter(~id=id |> Ident.name, ~scope),
      newOffset,
      scope,
    );
  | _ =>
    def |> Il.Def.emit(~instr=Il.Param(offset));
    let scope = Il.Env.Local(offset);
    let newEnv =
      env |> Il.Env.addFunctionParameter(~id=id |> Ident.name, ~scope);
    (newEnv, offset + 1, scope);
  };
};

let rec processFunPat = (~def, ~env, ~offset, pat: Typedtree.pattern) =>
  switch (pat.pat_desc) {
  | Tpat_var(id, _)
  | Tpat_alias({pat_desc: Tpat_any}, id, _) =>
    processFunPatId(~env, ~def, ~id, ~offset, ~typ=pat.pat_type)

  | Tpat_tuple(pats) =>
    let (newEnv, newOffset, scopes) =
      pats
      |> List.fold_left(
           ((e, o, scopes), p) => {
             let (newEnv, newOffset, scope) =
               p |> processFunPat(~def, ~env=e, ~offset=o);
             (newEnv, newOffset, [scope, ...scopes]);
           },
           (env, offset, []),
         );
    (newEnv, newOffset, Il.Env.Tuple(scopes));

  | _ =>
    Log_.info(~count=false, ~loc=pat.pat_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Argument pattern not supported")
    );
    assert(false);
  };

let rec processFunDef =
        (~def, ~env, ~offset, ~params, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_function({
      arg_label: Nolabel,
      param,
      cases: [{c_lhs, c_guard: None, c_rhs}],
      partial: Total,
    }) =>
    let (newEnv, newOffset, typ) =
      c_lhs |> processFunPat(~def, ~env, ~offset);
    c_rhs
    |> processFunDef(
         ~def,
         ~env=newEnv,
         ~offset=newOffset,
         ~params=[(param, typ), ...params],
       );

  | _ => (env, expr, params)
  };

let rec processExpr = (~def, ~env, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_constant(Const_int(n)) =>
    def |> Il.Def.emit(~instr=Il.Const(Il.I32(n |> Int32.of_int)))

  | Texp_ident(id, _, _) =>
    let id = Path.name(id);
    let rec emitScope = (scope: Il.Env.scope) =>
      switch (scope) {
      | Local(offset) => def |> Il.Def.emit(~instr=Il.LocalGet(offset))
      | Tuple(scopes) => scopes |> List.iter(emitScope)
      };
    switch (env |> Il.Env.find(~id)) {
    | Some(scope) => emitScope(scope)

    | None =>
      Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Id not found: %s", id)
      );

      assert(false);
    };

  | Texp_apply(
      {exp_desc: Texp_ident(callee, _, _), exp_loc: callee_loc},
      args,
    ) =>
    args
    |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
         switch (argLabel, argOpt) {
         | (Nolabel, Some(arg)) => arg |> processExpr(~def, ~env)
         | _ =>
           Log_.info(
             ~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
             Format.fprintf(ppf, "Argument not supported")
           )
         }
       );
    callee |> processCallee(~def, ~loc=callee_loc);

  | Texp_function(_) =>
    let (env, body, params) =
      expr |> processFunDef(~def, ~env, ~offset=0, ~params=[]);
    if (params == []) {
      Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Cannot decode function parameters")
      );
      assert(false);
    };
    def.params = params;
    body |> processExpr(~def, ~env);

  | Texp_tuple(l) => l |> List.iter(processExpr(~def, ~env))

  | _ =>
    Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Expression not supported")
    );
    assert(false);
  };

let processValueBinding = (~loc, ~id, ~expr: Typedtree.expression) => {
  Log_.item("no-alloc binding for %s@.", id |> Ident.name);
  let def = Il.createDef(~loc, ~id);
  let env = Il.Env.create();
  expr |> processExpr(~def, ~env);
};

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  switch (vb.vb_pat.pat_desc) {
  | Tpat_var(id, _)
      when vb.vb_attributes |> Annotation.hasAttribute((==)("noalloc")) =>
    processValueBinding(~loc=vb.vb_loc, ~id, ~expr=vb.Typedtree.vb_expr)
  | _ => ()
  };
  let r = super.Tast_mapper.value_binding(self, vb);
  r;
};

let traverseStructure = {
  /* Tast_mapper */
  let super = Tast_mapper.default;

  let value_binding = (self, vb) => vb |> collectValueBinding(super, self);
  Tast_mapper.{...super, value_binding};
};

let processCmt = (cmt_infos: Cmt_format.cmt_infos) =>
  switch (cmt_infos.cmt_annots) {
  | Interface(_) => ()
  | Implementation(structure) =>
    structure |> traverseStructure.structure(traverseStructure) |> ignore
  | _ => ()
  };

let reportResults = (~ppf) => Il.dumpDefs(~ppf);
