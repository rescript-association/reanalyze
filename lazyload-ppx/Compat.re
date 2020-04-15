#if OCAML_MINOR >= 8
let attributeTxt = (x: Parsetree.attribute) => x.attr_name.txt;
#else
let attributeTxt = (x: Parsetree.attribute) => fst(x).txt;
#endif

#if OCAML_MINOR >= 8
let mkAttribute = (~loc, ~txt) => {
  Parsetree.attr_loc: loc,
  attr_name: Location.{loc, txt},
  attr_payload: Parsetree.PStr([Ast_helper.Str.eval(Ast_helper.Exp.constant(Pconst_string("-3", None)))]),
};
#else
let mkAttribute = (~loc, ~txt) => (
  Location.{loc, txt},
  Parsetree.PStr([Ast_helper.Str.eval(Ast_helper.Exp.constant(Pconst_string("-3", None)))]),
);
#endif
