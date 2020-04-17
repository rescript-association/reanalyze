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

