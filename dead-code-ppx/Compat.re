#if OCAML_MINOR >= 8
let attributeTxt = (x: Parsetree.attribute) => x.attr_name.txt;
#else
let attributeTxt = (x: Parsetree.attribute) => fst(x).txt;
#endif
