open Compilerlibs406
open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let attributeTxt (x : Parsetree.attribute) = (fst x).txt

let mkAttribute ~loc ~txt =
  ( (let open Location in
    {loc; txt}),
    Parsetree.PStr
      [
        Ast_helper.Str.eval
          (Ast_helper.Exp.constant (Pconst_string ("-3", None)));
      ] )

let makeLoc ~loc ~txt = {Location.loc; txt}
let hasMappedStructure = ref false

module Resource = struct
  type t = {name : string; loc : Location.t}

  let compare {name = n1} {name = n2} = compare n1 n2
end

module ResourceSet = Set.Make (Resource)
module SM = Map.Make (String)

let jsResources = ref ResourceSet.empty
let topLevelExprs = ref SM.empty

let addTopLevelExpr bindingName expr =
  let count = ref 0 in
  let makeBindingName bindingName count =
    match count = 0 with
    | true -> bindingName
    | false -> bindingName ^ "$" ^ string_of_int count
  in
  while SM.mem (makeBindingName bindingName !count) !topLevelExprs do
    count := !count + 1
  done;
  let bindingName = makeBindingName bindingName !count in
  let _ = topLevelExprs := SM.add bindingName expr !topLevelExprs in
  bindingName

let depIgnore = [mkAttribute ~loc:!default_loc ~txt:"warning"]
let localModulePrefix = "$Local$"

let genLocal {Resource.name; loc} =
  with_default_loc loc (fun () ->
      Str.modtype
        (Mtd.mk
           ~typ:
             (Mty.typeof_ (Mod.ident {loc = !default_loc; txt = Lident name}))
           {loc = !default_loc; txt = localModulePrefix ^ name}))

let genTopLevelBinding (txt, exp) =
  Vb.mk (Pat.var {loc = !default_loc; txt}) exp

let structure mapper structure =
  if !hasMappedStructure then default_mapper.structure mapper structure
  else
    let _ = hasMappedStructure := true in
    let fileAttributes, restOfStructure =
      List.partition
        (fun str ->
          match str with
          | {pstr_desc = Pstr_attribute attr} -> attributeTxt attr = "bs.config"
          | {pstr_desc = Pstr_extension (({txt = "bs.config"}, _), _)} -> true
          | _ -> false)
        structure
    in
    let newStructure = default_mapper.structure mapper restOfStructure in
    fileAttributes
    @ List.map genLocal (ResourceSet.elements !jsResources)
    @ (match SM.bindings !topLevelExprs with
      | [] -> []
      | exprs -> [Str.value Nonrecursive (List.map genTopLevelBinding exprs)])
    @ newStructure

let nameFromLongident li =
  match li |> Longident.flatten with name :: _ -> name | [] -> "empty"

let expr mapper expr =
  match expr with
  | {
      pexp_desc =
        Pexp_extension
          ( {txt = "reasonResource"; loc},
            PStr
              [
                {
                  pstr_desc =
                    Pstr_eval ({pexp_desc = Pexp_construct ({txt}, _)}, _);
                };
              ] );
    } as pexp ->
    let name = txt |> nameFromLongident in
    let _ = jsResources := ResourceSet.add {Resource.name; loc} !jsResources in
    {
      pexp with
      pexp_desc =
        Pexp_constraint
          ( Exp.apply
              (Exp.ident
                 {
                   loc = !default_loc;
                   txt = Ldot (Lident "JSResource", "jSResource");
                 })
              [(Nolabel, Exp.constant (Pconst_string (name ^ ".bs", None)))],
            Typ.constr
              {loc = !default_loc; txt = Ldot (Lident "JSResource", "t")}
              [
                Typ.package
                  {loc = !default_loc; txt = Lident (localModulePrefix ^ name)}
                  [];
              ] );
    }
  | {
   pexp_desc =
     Pexp_extension
       ( {txt = "requireDeferred"; loc},
         PStr
           [
             {pstr_desc = Pstr_eval ({pexp_desc = Pexp_construct ({txt}, _)}, _)};
           ] );
  } ->
    let name = txt |> nameFromLongident in
    let _ = jsResources := ResourceSet.add {Resource.name; loc} !jsResources in
    let bindingName = "$" ^ name ^ "$Deferred" in
    let actualExp =
      Exp.constraint_
        (Exp.apply
           (Exp.ident
              {
                loc = !default_loc;
                txt = Ldot (Lident "RequireDeferred", "make");
              })
           [(Nolabel, Exp.constant (Pconst_string (name ^ ".bs", None)))])
        (Typ.constr
           {loc = !default_loc; txt = Ldot (Lident "RequireDeferred", "t")}
           [
             Typ.package
               {loc = !default_loc; txt = Lident (localModulePrefix ^ name)}
               [];
           ])
    in
    let bindingName = addTopLevelExpr bindingName actualExp in
    Exp.ident {loc = !default_loc; txt = Lident bindingName}
  | {
   pexp_desc =
     Pexp_extension
       ( {txt = "requireCond"},
         PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {
                       pexp_desc =
                         Pexp_tuple
                           [
                             conditionType;
                             condition;
                             {
                               pexp_desc =
                                 Pexp_extension
                                   ( {txt = "obj"},
                                     PStr
                                       [
                                         {
                                           pstr_desc =
                                             Pstr_eval
                                               ( {
                                                   pexp_desc =
                                                     Pexp_record
                                                       ( ( [
                                                             ( {
                                                                 txt =
                                                                   Longident
                                                                   .Lident
                                                                     "true";
                                                               },
                                                               {
                                                                 pexp_desc =
                                                                   Pexp_construct
                                                                     ( {
                                                                         txt =
                                                                           thenTxt;
                                                                         loc =
                                                                           thenLoc;
                                                                       },
                                                                       _ );
                                                                 pexp_loc =
                                                                   pexp_loc_then;
                                                               } );
                                                             ( {
                                                                 txt =
                                                                   Longident
                                                                   .Lident
                                                                     "false";
                                                               },
                                                               {
                                                                 pexp_desc =
                                                                   Pexp_construct
                                                                     ( {
                                                                         txt =
                                                                           elseTxt;
                                                                         loc =
                                                                           elseLoc;
                                                                       },
                                                                       _ );
                                                                 pexp_loc =
                                                                   pexp_loc_else;
                                                               } );
                                                           ]
                                                         | [
                                                             ( {
                                                                 txt =
                                                                   Longident
                                                                   .Lident
                                                                     "false";
                                                               },
                                                               {
                                                                 pexp_desc =
                                                                   Pexp_construct
                                                                     ( {
                                                                         txt =
                                                                           elseTxt;
                                                                         loc =
                                                                           elseLoc;
                                                                       },
                                                                       _ );
                                                                 pexp_loc =
                                                                   pexp_loc_else;
                                                               } );
                                                             ( {
                                                                 txt =
                                                                   Longident
                                                                   .Lident
                                                                     "true";
                                                               },
                                                               {
                                                                 pexp_desc =
                                                                   Pexp_construct
                                                                     ( {
                                                                         txt =
                                                                           thenTxt;
                                                                         loc =
                                                                           thenLoc;
                                                                       },
                                                                       _ );
                                                                 pexp_loc =
                                                                   pexp_loc_then;
                                                               } );
                                                           ] ),
                                                         _ );
                                                 },
                                                 [] );
                                         };
                                       ] );
                             };
                           ];
                     },
                     _ );
             };
           ] );
  } ->
    let thenModule = thenTxt |> nameFromLongident in
    let elseModule = elseTxt |> nameFromLongident in
    jsResources :=
      ResourceSet.add {Resource.name = thenModule; loc = thenLoc} !jsResources;
    jsResources :=
      ResourceSet.add {Resource.name = elseModule; loc = elseLoc} !jsResources;
    let bindingName = "$" ^ thenModule ^ "$OR$" ^ elseModule ^ "$RequireCond" in
    let actualExp =
      Exp.constraint_ ~loc:pexp_loc_then
        (Exp.constraint_ ~loc:pexp_loc_else
           (Exp.apply
              (Exp.ident ~attrs:depIgnore
                 {
                   loc = !default_loc;
                   txt = Ldot (Lident "RequireCond", "either");
                 })
              [
                (Nolabel, conditionType);
                (Nolabel, condition);
                ( Nolabel,
                  Exp.extension
                    ( Location.mkloc "obj" !default_loc,
                      Parsetree.PStr
                        [
                          Str.eval
                            (Exp.record
                               [
                                 ( Location.mkloc (Longident.Lident "true")
                                     !default_loc,
                                   Exp.constant
                                     (Pconst_string (thenModule ^ ".bs", None))
                                 );
                                 ( Location.mkloc (Longident.Lident "false")
                                     !default_loc,
                                   Exp.constant
                                     (Pconst_string (elseModule ^ ".bs", None))
                                 );
                               ]
                               None);
                        ] ) );
              ])
           (Typ.package
              {
                loc = !default_loc;
                txt = Lident (localModulePrefix ^ elseModule);
              }
              []))
        (Typ.package
           {loc = !default_loc; txt = Lident (localModulePrefix ^ thenModule)}
           [])
    in
    let bindingName = addTopLevelExpr bindingName actualExp in
    Exp.ident {loc = !default_loc; txt = Lident bindingName}
  | {
   pexp_desc =
     Pexp_extension
       ( {txt = "requireCond"},
         PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {
                       pexp_desc =
                         Pexp_tuple
                           [
                             conditionType;
                             condition;
                             {pexp_desc = Pexp_construct ({txt; loc}, _)};
                           ];
                     },
                     _ );
             };
           ] );
  } ->
    let name = txt |> nameFromLongident in
    let _ = jsResources := ResourceSet.add {Resource.name; loc} !jsResources in
    let bindingName = "$" ^ name ^ "$RequireCond" in
    let actualExp =
      Exp.constraint_
        (Exp.apply
           (Exp.ident ~attrs:depIgnore
              {loc = !default_loc; txt = Ldot (Lident "RequireCond", "make")})
           [
             (Nolabel, conditionType);
             (Nolabel, condition);
             (Nolabel, Exp.constant (Pconst_string (name ^ ".bs", None)));
           ])
        (Typ.constr
           {
             loc = !default_loc;
             txt = Ldot (Ldot (Lident "Js", "Nullable"), "t");
           }
           [
             Typ.package
               {loc = !default_loc; txt = Lident (localModulePrefix ^ name)}
               [];
           ])
    in
    let bindingName = addTopLevelExpr bindingName actualExp in
    Exp.ident {loc = !default_loc; txt = Lident bindingName}
  | _ -> default_mapper.expr mapper expr

let module_expr mapper module_expr =
  match module_expr with
  | {
      pmod_desc =
        Pmod_extension
          ( {txt = "lazyLoadComponent"},
            PStr
              [
                {
                  pstr_desc =
                    Pstr_eval ({pexp_desc = Pexp_construct ({txt}, _)}, _);
                };
              ] );
      pmod_loc;
    } as pmod ->
    with_default_loc pmod_loc (fun () ->
        {
          pmod with
          pmod_desc =
            Pmod_structure
              [
                Str.value Nonrecursive
                  [
                    Vb.mk
                      (Pat.var {loc = !default_loc; txt = "reasonResource"})
                      (expr mapper
                         (Exp.extension
                            ( {loc = !default_loc; txt = "reasonResource"},
                              PStr
                                [
                                  Str.eval
                                    (Exp.construct {loc = !default_loc; txt}
                                       None);
                                ] )));
                  ];
                Str.value Nonrecursive
                  [
                    Vb.mk
                      (Pat.var {loc = !default_loc; txt = "makeProps"})
                      (Exp.ident
                         {loc = !default_loc; txt = Ldot (txt, "makeProps")});
                  ];
                Str.value Nonrecursive
                  [
                    Vb.mk
                      (Pat.var {loc = !default_loc; txt = "make"})
                      (Exp.fun_ Nolabel None
                         (Pat.var {loc = !default_loc; txt = "props"})
                         (Exp.apply
                            (Exp.ident
                               {
                                 loc = !default_loc;
                                 txt = Ldot (Lident "React", "createElement");
                               })
                            [
                              ( Nolabel,
                                Exp.letmodule
                                  (makeLoc ~loc:!default_loc ~txt:"Comp")
                                  (Mod.unpack
                                     (Exp.apply
                                        (Exp.ident
                                           {
                                             loc = !default_loc;
                                             txt =
                                               Ldot
                                                 ( Lident "BootloaderResource",
                                                   "read" );
                                           })
                                        [
                                          ( Nolabel,
                                            Exp.ident
                                              {
                                                loc = !default_loc;
                                                txt = Lident "reasonResource";
                                              } );
                                        ]))
                                  (Exp.ident
                                     {
                                       loc = !default_loc;
                                       txt = Ldot (Lident "Comp", "make");
                                     }) );
                              ( Nolabel,
                                Exp.ident
                                  {loc = !default_loc; txt = Lident "props"} );
                            ]));
                  ];
              ];
        })
  | _ -> default_mapper.module_expr mapper module_expr

let () =
  Ast_mapper.register "lazyLoad" (fun _argv ->
      {default_mapper with structure; expr; module_expr})
