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

type instKind =
  | Param
  | Decl
  | Set;
let rec processFunPatId =
        (~env, ~def: Il.Def.t, ~idOpt, ~instrKind, ~typ: Types.type_expr) => {
  switch (typ.desc) {
  | Ttuple(ts) =>
    let (newEnv, scopesRev) =
      ts
      |> List.fold_left(
           ((e, scopes), t) => {
             let (newEnv, newScope) =
               processFunPatId(~env=e, ~def, ~idOpt=None, ~instrKind, ~typ=t);
             (newEnv, [newScope, ...scopes]);
           },
           (env, []),
         );
    let scope = Il.Env.Tuple(scopesRev |> List.rev);
    let newEnv2 =
      switch (idOpt) {
      | None => newEnv
      | Some(id) => newEnv |> Il.Env.addFunctionParameter(~id, ~scope)
      };
    (newEnv2, scope);
  | _ =>
    let offset = def.nextOffset;
    let instr =
      switch (instrKind) {
      | Param => Il.Param(offset)
      | Decl => Il.LocalDecl(offset)
      | Set => Il.LocalSet(offset)
      };
    def |> Il.Def.emit(~instr);
    def.nextOffset = offset + 1;
    let scope = Il.Env.Local(offset);
    let newEnv =
      switch (idOpt) {
      | None => env
      | Some(id) => env |> Il.Env.addFunctionParameter(~id, ~scope)
      };
    (newEnv, scope);
  };
};

let rec processFunPat = (~def, ~env, pat: Typedtree.pattern) =>
  switch (pat.pat_desc) {
  | Tpat_var(id, _)
  | Tpat_alias({pat_desc: Tpat_any}, id, _) =>
    processFunPatId(
      ~env,
      ~def,
      ~idOpt=Some(id |> Ident.name),
      ~instrKind=Param,
      ~typ=pat.pat_type,
    )

  | Tpat_tuple(pats) =>
    let (newEnv, scopes) =
      pats
      |> List.fold_left(
           ((e, scopes), p) => {
             let (newEnv, scope) = p |> processFunPat(~def, ~env=e);
             (newEnv, [scope, ...scopes]);
           },
           (env, []),
         );
    (newEnv, Il.Env.Tuple(scopes));

  | _ =>
    Log_.info(~count=false, ~loc=pat.pat_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Argument pattern not supported")
    );
    assert(false);
  };

let rec processFunDef = (~def, ~env, ~params, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_function({
      arg_label: Nolabel,
      param,
      cases: [{c_lhs, c_guard: None, c_rhs}],
      partial: Total,
    }) =>
    let (newEnv, typ) = c_lhs |> processFunPat(~def, ~env);
    c_rhs
    |> processFunDef(~def, ~env=newEnv, ~params=[(param, typ), ...params]);

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
    let (env, body, params) = expr |> processFunDef(~def, ~env, ~params=[]);
    if (params == []) {
      Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Cannot decode function parameters")
      );
      assert(false);
    };
    def.params = params;
    body |> processExpr(~def, ~env);

  | Texp_tuple(l) => l |> List.iter(processExpr(~def, ~env))

  | Texp_let(
      Nonrecursive,
      [{vb_pat: {pat_desc: Tpat_var(id, _)}, vb_expr}],
      inExpr,
    ) =>
    let offset = def.nextOffset;
    let (newEnv, _scope) =
      processFunPatId(
        ~env,
        ~def,
        ~idOpt=Some(id |> Ident.name),
        ~instrKind=Decl,
        ~typ=vb_expr.exp_type,
      );

    vb_expr |> processExpr(~def, ~env);

    def.nextOffset = offset; // TODO: resetting as processing twicce
    let (newEnv2, scope) =
      processFunPatId(
        ~env=newEnv,
        ~def,
        ~idOpt=Some(id |> Ident.name),
        ~instrKind=Set,
        ~typ=vb_expr.exp_type,
      );

    let newEnv3 =
      newEnv2 |> Il.Env.addFunctionParameter(~id=id |> Ident.name, ~scope);
    inExpr |> processExpr(~def, ~env=newEnv3);

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
