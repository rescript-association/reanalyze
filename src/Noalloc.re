module StringMap = Map.Make(String);

module Env = {
  type offset = int;
  type scope =
    | Local(offset);
  type id = string;
  type t = StringMap.t(scope);

  let addFunctionParameter = (~id, ~offset, env: t) =>
    env |> StringMap.add(id, Local(offset));

  let find = (~id, env: t) => env |> StringMap.find_opt(id);

  let create = (): t => StringMap.empty;
};

let processCallee = (~def, ~loc, callee) =>
  switch (callee |> Path.name) {
  | "Pervasives.+"
  | "Stdlib.+" => def |> Il.Def.emit(~instr=Il.I32Add)
  | name =>
    Log_.info(~count=false, ~loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Callee not recognized: %s", name)
    );
    assert(false);
  };

let rec processFunDef =
        (~def, ~env, ~offset, ~params, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_function({
      arg_label: Nolabel,
      param,
      cases: [
        {
          c_lhs: {pat_desc: Tpat_var(_id, _), pat_type},
          c_guard: None,
          c_rhs,
        },
      ],
      partial: Total,
    }) =>
    def |> Il.Def.emit(~instr=Il.Param(offset));
    c_rhs
    |> processFunDef(
         ~def,
         ~env=
           env |> Env.addFunctionParameter(~id=param |> Ident.name, ~offset),
         ~offset=offset + 1,
         ~params=[(param, pat_type), ...params],
       );
  | _ => (env, expr, params)
  };

let rec processExpr = (~def, ~env, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_constant(Const_int(n)) =>
    def |> Il.Def.emit(~instr=Il.Const(Il.I32(n |> Int32.of_int)))

  | Texp_ident(id, _, _) =>
    let id = Path.name(id);
    switch (env |> Env.find(~id)) {
    | Some(Local(offset)) => def |> Il.Def.emit(~instr=Il.LocalGet(offset))

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
    def.params = params;
    body |> processExpr(~def, ~env);

  | _ =>
    Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Expression not supported")
    );
    assert(false);
  };

let processValueBinding = (~id, ~expr: Typedtree.expression) => {
  Log_.item("no-alloc binding for %s@.", id |> Ident.name);
  let def = Il.createDef(~id);
  let env = Env.create();
  expr |> processExpr(~def, ~env);
};

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  switch (vb.vb_pat.pat_desc) {
  | Tpat_var(id, _)
      when vb.vb_attributes |> Annotation.hasAttribute((==)("noalloc")) =>
    processValueBinding(~id, ~expr=vb.Typedtree.vb_expr)
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
