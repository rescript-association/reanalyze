open CL

#if OCAML_VERSION >= (4, 08, 0)
let getStringTag s = match s with
  | Format.String_tag(s) -> s
  | _ -> ""
#else
let getStringTag s = s
#endif

#if OCAML_VERSION >= (4, 08, 0)
let filter_map = List.filter_map
#else
(* https://github.com/ocaml/ocaml/blob/9a31c888b177f3aa603bbbe17852cbb57f047df4/stdlib/list.ml#L254-L262 passed though refmt *)
let filter_map f =
  let rec aux accu =
    function
    | [] -> List.rev accu
    | x :: l ->
      match f x with
      | None -> aux accu l
      | Some(v) -> aux (v::accu) l
       in
  aux []
#endif

let getStringValue const = match const with
#if OCAML_VERSION >= (4, 11, 0)
  | Parsetree.Pconst_string(s, _, _) -> s
#else
  | Parsetree.Pconst_string(s, _) -> s
#endif
  | _ -> assert false


let getConstString const = match const with
#if OCAML_VERSION >= (4, 11, 0)
  | Asttypes.Const_string(s, _, _) -> s
#else
  | Asttypes.Const_string(s, _) -> s
#endif
  | _ -> assert false


#if OCAML_VERSION >= (4, 11, 0)
type 'a typedtreeCase = 'a Typedtree.case
#else
type 'a typedtreeCase = Typedtree.case
#endif

#if OCAML_VERSION >= (4, 11, 0)
type 'a generalPattern = 'a Typedtree.general_pattern
#else
type 'a generalPattern = Typedtree.pattern
#endif

#if OCAML_VERSION >= (4, 13, 0)
type ('a, 'b) type_kind = ('a, 'b) Types.type_kind
#else
type ('a, 'b) type_kind = Types.type_kind
#endif

let unboxPatCstrName pat =
#if OCAML_VERSION >= (4, 11, 0)
  match pat.Typedtree.pat_desc with
  | Typedtree.Tpat_value v -> (
    match
      (v :> Typedtree.value Typedtree.pattern_desc Typedtree.pattern_data)
        .pat_desc
    with
#if OCAML_VERSION >= (4, 13, 0)
    | Tpat_construct (_, {cstr_name}, _, _) -> Some cstr_name
#else
    | Tpat_construct (_, {cstr_name}, _) -> Some cstr_name
#endif
    | _ -> None)
  | _ -> None
#else
  match pat.Typedtree.pat_desc with
    | Tpat_construct(_, {cstr_name}, _) -> Some(cstr_name)
    | _ -> None
#endif

let unboxPatCstrTxt pat = match pat with
#if OCAML_VERSION >= (4, 13, 0)
  | Typedtree.Tpat_construct ({txt}, _, _, _) -> txt
#else
  | Typedtree.Tpat_construct ({txt}, _, _) -> txt
#endif
  | _ -> assert false


#if OCAML_VERSION >= (4, 08, 0)
let setOpenCloseTag openTag closeTag =
  {
    Format.mark_open_stag = openTag;
    mark_close_stag = closeTag;
    print_open_stag = (fun _ -> ());
    print_close_stag = fun _ -> ()
  }
#else
let setOpenCloseTag openTag closeTag =
  {
    Format.mark_open_tag = openTag;
    mark_close_tag = closeTag;
    print_open_tag = (fun _ -> ());
    print_close_tag = (fun _ -> ())
  }
#endif

let pp_set_formatter_tag_functions =
#if OCAML_VERSION >= (4, 08, 0)
    Format.pp_set_formatter_stag_functions
#else
    Format.pp_set_formatter_tag_functions [@warning "-3"]
#endif

let getSigValue si = match si with
#if OCAML_VERSION >= (4, 08, 0)
  | Types.Sig_value(id, {Types.val_loc; val_kind; val_type}, _) ->
    (id, val_loc, val_kind, val_type)
#else
  | Types.Sig_value(id, {Types.val_loc; val_kind; val_type}) ->
    (id, val_loc, val_kind, val_type)
#endif
  | _ -> assert false

let getSigType si = match si with
#if OCAML_VERSION >= (4, 08, 0)
  | Types.Sig_type(id, t, _, _) ->
    (id, t)
#else
  | Types.Sig_type(id, t, _) ->
    (id, t)
#endif
  | _ -> assert false

let getTSubst td = match td with
#if OCAML_VERSION >= (4, 13, 0)
  | Types.Tsubst (t, _) -> t
#else
  | Types.Tsubst t -> t
#endif
  | _ -> assert false

let getTypeVariant (tk: ('a, 'b) type_kind) = match tk with
#if OCAML_VERSION >= (4, 13, 0)
  | Type_variant (l, _) -> l
#else
  | Type_variant l -> l
#endif
  | _ -> assert false

let getSigModuleModtype si = match si with
#if OCAML_VERSION >= (4, 08, 0)
  | Types.Sig_module(id, _, {Types.md_type=moduleType; md_loc=loc}, _, _)
  | Types.Sig_modtype(id, {Types.mtd_type=Some(moduleType); mtd_loc=loc}, _) ->
    Some((id, moduleType, loc))
#else
  | Types.Sig_module(id, {Types.md_type= moduleType; md_loc=loc}, _)
  | Types.Sig_modtype(id, {Types.mtd_type=Some(moduleType); mtd_loc=loc}) ->
    Some((id, moduleType, loc))
#endif
  | _ -> None


let getMtyFunctorModuleType  (moduleType: Types.module_type) = match moduleType with
#if OCAML_VERSION >= (4, 10, 0)
  | Mty_functor(Named(_, mtParam), mt) -> Some((Some(mtParam), mt))
  | Mty_functor(Unit, mt) -> Some((None, mt))
#else
  | Mty_functor(_, mtParam, mt) -> Some((mtParam, mt))
#endif
  | _ -> None

let getTexpMatch desc = match desc with
#if OCAML_VERSION >= (4, 08, 0)
  | Typedtree.Texp_match(e, cases, partial) ->
    (e, cases, partial)
#else
  | Typedtree.Texp_match(e, casesOK, casesExn, partial) ->
    (e, casesOK @ casesExn, partial)
#endif
  | _ -> assert false

let texpMatchGetExceptions desc = match desc with
#if OCAML_VERSION >= (4, 08, 0)
  | Typedtree.Texp_match(_, cases, _) ->
    cases
    |> List.filter_map(fun ({Typedtree.c_lhs= pat}) ->
          match pat.pat_desc with
          | Tpat_exception({pat_desc}) -> Some(pat_desc)
          | _ -> None
          )
#else
  | Typedtree.Texp_match(_, _, casesExn, _) ->
    casesExn |> List.map (fun (case: Typedtree.case) -> case.c_lhs.pat_desc)
#endif
  | _ -> assert false


let texpMatchHasExceptions desc = texpMatchGetExceptions(desc) != []



let getPayload x = 
#if OCAML_VERSION >= (4, 08, 0)
 let {Parsetree.attr_name= {txt}; attr_payload= payload} = x in
#else
 let ({Asttypes.txt}, payload) = x in
#endif
 (txt, payload)

module Ident = struct
  include Ident
#if OCAML_VERSION >= (4, 08, 0)
  let create = Ident.create_local
#endif
end

let tstrExceptionGet (x : Typedtree.structure_item_desc) = match x with
#if OCAML_VERSION >= (4, 08, 0)
  | Tstr_exception({tyexn_constructor= {ext_id}; tyexn_loc}) ->
    Some((ext_id, tyexn_loc))
#else
  | Tstr_exception({ext_id; ext_loc}) ->
    Some((ext_id, ext_loc))
#endif
  | _ -> None

#if OCAML_VERSION >= (4, 10, 0)
let moduleIdName nameOpt = match nameOpt with
  | None -> "UnnamedModule"
  | Some(name) -> name |> Ident.name
#else
let moduleIdName name = name |> Ident.name
#endif

#if OCAML_VERSION >= (4, 14, 0)
let get_desc = Types.get_desc
#else
let get_desc x = x.Types.desc
#endif
