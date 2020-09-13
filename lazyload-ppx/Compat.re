let attributeTxt = (x: Parsetree.attribute) => fst(x).txt;

let mkAttribute = (~loc, ~txt) => (
  Location.{loc, txt},
  Parsetree.PStr([Ast_helper.Str.eval(Ast_helper.Exp.constant(Pconst_string("-3", None)))]),
);

let makeLoc = (~loc, ~txt) => {
  {Location.loc, txt};
};
