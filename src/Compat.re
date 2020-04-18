#if OCAML_MINOR >= 8
let getStringTag = s => switch (s) {
  | Format.String_tag(s) => s
  | _ => ""
}
#else
let getStringTag = s => s
#endif


#if OCAML_MINOR >= 8
let setOpenCloseTag = (openTag, closeTag) => {
  Format.mark_open_stag: openTag,
  mark_close_stag: closeTag,
  print_open_stag: _ => (),
  print_close_stag: _ => (),
}
#else
let setOpenCloseTag = (openTag, closeTag) => {
  Format.mark_open_tag: openTag,
  mark_close_tag: closeTag,
  print_open_tag: _ => (),
  print_close_tag: _ => (),
}
#endif

let pp_set_formatter_tag_functions =
#if OCAML_MINOR >= 8
    Format.pp_set_formatter_stag_functions;
#else
    Format.pp_set_formatter_tag_functions;
#endif

let getSigValue = si => switch si {
#if OCAML_MINOR >= 8
  | Types.Sig_value(id, {Types.val_loc, val_kind}, _) =>
    (id, val_loc, val_kind)
#else
  | Types.Sig_value(id, {Types.val_loc, val_kind}) =>
    (id, val_loc, val_kind)
#endif
  | _ => assert false
}

let getSigType = si => switch si {
#if OCAML_MINOR >= 8
  | Types.Sig_type(id, t, _, _) =>
    (id, t)
#else
  | Types.Sig_type(id, t, _) =>
    (id, t)
#endif
  | _ => assert false
}

let getSigModuleModtype = si => switch si {
#if OCAML_MINOR >= 8
  | Types.Sig_module(id, _, {Types.md_type: moduleType}, _, _)
  | Types.Sig_modtype(id, {Types.mtd_type: Some(moduleType)}, _) =>
    Some((id, moduleType))
#else
  | Types.Sig_module(id, {Types.md_type: moduleType}, _)
  | Types.Sig_modtype(id, {Types.mtd_type: Some(moduleType)}) =>
    Some((id, moduleType))
#endif
  | _ => None
}

let getTexpMatch = desc => switch desc {
#if OCAML_MINOR >= 8
  | Typedtree.Texp_match(e, cases, partial) =>
    (e, casesOK @ casesExn, partial)
#else
  | Typedtree.Texp_match(e, casesOK, casesExn, partial) =>
    (e, casesOK @ casesExn, partial)
#endif
  | _ => assert false
}

let texpMatchHasExceptions = desc => switch desc {
#if OCAML_MINOR >= 8
  | Typedtree.Texp_match(_, cases, _) =>
    cases
    |> List.for_all(({c_lhs: pat}: Typedtree.case) =>
          switch (pat.pat_desc) {
          | Tpat_exception(_) => true
          | _ => false
          })
#else
  | Typedtree.Texp_match(_, _, casesExn, _) =>
    casesExn != []
#endif
  | _ => assert false
}



let getPayload = x => {
#if OCAML_MINOR >= 8
 let {attr_name: {txt}, attr_payload: payload} = x;
#else
 let ({Asttypes.txt}, payload) = x;
#endif
 (txt, payload)
}

module Ident = {
  include Ident;
#if OCAML_MINOR >= 8
  let create = Ident.create_local
#endif
}
