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

let rec processExpr = (~def, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_constant(Const_int(n)) =>
    def |> Il.Def.emit(~instr=Il.Const(Il.I32(n |> Int32.of_int)))
  | Texp_apply(
      {exp_desc: Texp_ident(callee, _, _), exp_loc: callee_loc},
      args,
    ) =>
    args
    |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
         switch (argLabel, argOpt) {
         | (Nolabel, Some(arg)) => arg |> processExpr(~def)
         | _ =>
           Log_.info(
             ~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
             Format.fprintf(ppf, "Argument not supported")
           )
         }
       );
    callee |> processCallee(~def, ~loc=callee_loc);

  | _ =>
    Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Expression not supported")
    );
    assert(false);
  };

let processValueBinding = (~id, ~expr: Typedtree.expression) => {
  Log_.item("no-alloc binding for %s@.", id |> Ident.name);
  let def = Il.newDef(~id);
  expr |> processExpr(~def);
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
