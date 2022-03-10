#if OCAML_VERSION >= (4, 08, 0)
let attributeTxt (x: Parsetree.attribute) = x.attr_name.txt
#else
let attributeTxt (x: Parsetree.attribute) = (fst x).txt
#endif

#if OCAML_VERSION >= (4, 08, 0)
let exceptionAttributes ({Parsetree.ptyexn_attributes}) = ptyexn_attributes
#else
let exceptionAttributes ({Parsetree.pext_attributes}) = pext_attributes
#endif
