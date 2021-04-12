let processCallee ~env ~funDef ~loc callee =
  match callee with
  | Path.Pident id -> (
    let id = Ident.name id in
    match env |> Il.Env.find ~id with
    | Some (FunDef funDefCallee) ->
      funDef |> Il.FunDef.emit ~instr:(Il.Call funDefCallee.id)
    | _ ->
      Log_.info ~count:false ~loc ~name:"Noalloc" (fun ppf () ->
          Format.fprintf ppf "Callee not recognized: %s" id);
      assert false)
  | _ -> (
    match callee |> Path.name with
    | "Pervasives.+" | "Stdlib.+" -> funDef |> Il.FunDef.emit ~instr:Il.I32Add
    | "Pervasives.+." | "Stdlib.+." -> funDef |> Il.FunDef.emit ~instr:Il.F64Add
    | "Pervasives.*." | "Stdlib.*." -> funDef |> Il.FunDef.emit ~instr:Il.F64Mul
    | name ->
      Log_.info ~count:false ~loc ~name:"Noalloc" (fun ppf () ->
          Format.fprintf ppf "Callee not recognized: %s" name);
      assert false)

let rec processTyp ~(funDef : Il.funDef) ~loc (typ : Types.type_expr) =
  match typ.desc with
  | Ttuple ts ->
    let scopes = ts |> List.map (processTyp ~funDef ~loc) in
    Il.Tuple scopes
  | Tlink t | Tsubst t -> t |> processTyp ~funDef ~loc
  | Tconstr _ | Tvar _ ->
    let offset = funDef.nextOffset in
    funDef.nextOffset <- offset + 1;
    Il.Local offset
  | _ ->
    Log_.info ~count:false ~loc ~name:"Noalloc" (fun ppf () ->
        Format.fprintf ppf "Type not supported");
    assert false

let rec sizeOfTyp ~loc (typ : Types.type_expr) =
  match typ.desc with
  | Tlink t | Tsubst t -> t |> sizeOfTyp ~loc
  | Tconstr (Pident id, [], _) -> (
    match Ident.name id with
    | "int" -> 4
    | "string" -> 4
    | name ->
      Log_.info ~count:false ~loc ~name:"Noalloc" (fun ppf () ->
          Format.fprintf ppf "Size of type %s not supported" name);
      assert false)
  | _ ->
    Log_.info ~count:false ~loc ~name:"Noalloc" (fun ppf () ->
        Format.fprintf ppf "Size of type not supported");
    assert false

let rec emitLocalSetBackwards ~(funDef : Il.funDef) ~(scope : Il.scope) =
  match scope with
  | Tuple scopes ->
    List.rev scopes
    |> List.iter (fun s -> emitLocalSetBackwards ~funDef ~scope:s)
  | Local offset ->
    let instr = Il.LocalSet offset in
    funDef |> Il.FunDef.emit ~instr

let rec processFunDefPat ~funDef ~env ~mem (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Tpat_var (id, _) | Tpat_alias ({pat_desc = Tpat_any}, id, _) ->
    let scope = pat.pat_type |> processTyp ~funDef ~loc:pat.pat_loc in
    let newEnv =
      env |> Il.Env.add ~id:(id |> Ident.name) ~def:(LocalScope scope)
    in
    (newEnv, scope)
  | Tpat_construct ({txt = Longident.Lident "()"}, _, _) -> (env, Il.Tuple [])
  | Tpat_tuple pats ->
    let newEnv, scopes =
      pats
      |> List.fold_left
           (fun (e, scopes) p ->
             let newEnv, scope = p |> processFunDefPat ~funDef ~env:e ~mem in
             (newEnv, scope :: scopes))
           (env, [])
    in
    (newEnv, Il.Tuple scopes)
  | _ ->
    Log_.info ~count:false ~loc:pat.pat_loc ~name:"Noalloc" (fun ppf () ->
        Format.fprintf ppf "Argument pattern not supported");
    assert false

let rec processFunDef ~funDef ~env ~mem ~params (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_function
      {
        arg_label = Nolabel;
        param;
        cases = [{c_lhs; c_guard = None; c_rhs}];
        partial = Total;
      } ->
    let newEnv, typ = c_lhs |> processFunDefPat ~funDef ~env ~mem in
    c_rhs
    |> processFunDef ~funDef ~env:newEnv ~mem ~params:((param, typ) :: params)
  | _ ->
    funDef.numParams <- funDef.nextOffset;
    (env, expr, params)

let translateConst ~loc ~mem (const : Asttypes.constant) =
  match const with
  | Const_int n -> Il.I32 (n |> Int32.of_int)
  | Const_float s ->
    let sWithDecimal =
      match (s.[String.length s - 1] [@doesNotRaise]) = '.' with
      | true -> s ^ "0"
      | false -> s
    in
    Il.F64 sWithDecimal
  | Const_string _ ->
    let index =
      mem |> Il.Mem.allocString ~string:(const |> Compat.getConstString)
    in
    Il.I32 (index |> Int32.of_int)
  | _ ->
    Log_.info ~count:false ~loc ~name:"Noalloc" (fun ppf () ->
        Format.fprintf ppf "Constant not supported");
    assert false

let processConst ~funDef ~loc ~mem (const_ : Asttypes.constant) =
  let const = const_ |> translateConst ~loc ~mem in
  funDef |> Il.FunDef.emit ~instr:(Il.Const const)

let rec processLocalBinding ~env ~(pat : Typedtree.pattern) ~(scope : Il.scope)
    =
  match (pat.pat_desc, scope) with
  | Tpat_var (id, _), _ ->
    env |> Il.Env.add ~id:(id |> Ident.name) ~def:(LocalScope scope)
  | Tpat_tuple pats, Tuple scopes ->
    let patsAndScopes = List.combine pats scopes in
    patsAndScopes
    |> List.fold_left
         (fun e (p, s) -> processLocalBinding ~env:e ~pat:p ~scope:s)
         env
  | Tpat_any, _ -> env
  | _ -> assert false

and processExpr ~funDef ~env ~mem (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_constant const -> const |> processConst ~funDef ~loc:expr.exp_loc ~mem
  | Texp_ident (id, _, _) -> (
    let id = Path.name id in
    let rec emitScope (scope : Il.scope) =
      match scope with
      | Local offset -> funDef |> Il.FunDef.emit ~instr:(Il.LocalGet offset)
      | Tuple scopes -> scopes |> List.iter emitScope
    in
    match env |> Il.Env.find ~id with
    | Some (LocalScope scope) -> emitScope scope
    | Some (GlobalDef {init}) ->
      let rec emitInit (init : Il.Init.t) =
        match init with
        | Const const -> funDef |> Il.FunDef.emit ~instr:(Il.Const const)
        | Tuple is -> is |> List.iter emitInit
      in
      emitInit init
    | _ ->
      Log_.info ~count:false ~loc:expr.exp_loc ~name:"Noalloc" (fun ppf () ->
          Format.fprintf ppf "Id not found: %s" id);
      assert false)
  | Texp_apply
      ({exp_desc = Texp_ident (callee, _, vd); exp_loc = callee_loc}, args) ->
    let kind = vd.val_type |> Il.Kind.fromType in
    args
    |> List.iteri (fun i ((argLabel : Asttypes.arg_label), argOpt) ->
           match (argLabel, argOpt) with
           | Nolabel, Some (arg : Typedtree.expression) ->
             (match kind with
             | Arrow (declKinds, _) ->
               let declKind = declKinds.(i) in
               let argKind = arg.exp_type |> Il.Kind.fromType in
               if argKind <> declKind then
                 Log_.info ~count:true ~loc:arg.exp_loc ~name:"Noalloc"
                   (fun ppf () ->
                     Format.fprintf ppf
                       "Function call to @{<info>%s@}: parameter %d has kind \
                        @{<info>%s@} but the supplied argument has kind \
                        @{<info>%s@}"
                       (callee |> Path.name) i
                       (declKind |> Il.Kind.toString)
                       (argKind |> Il.Kind.toString))
             | _ -> assert false);
             arg |> processExpr ~funDef ~env ~mem
           | _ ->
             Log_.info ~count:false ~loc:expr.exp_loc ~name:"Noalloc"
               (fun ppf () -> Format.fprintf ppf "Argument not supported"));
    callee |> processCallee ~env ~funDef ~loc:callee_loc
  | Texp_function _ ->
    let env, body, params =
      expr |> processFunDef ~funDef ~env ~mem ~params:[]
    in
    if params = [] then (
      Log_.info ~count:false ~loc:expr.exp_loc ~name:"Noalloc" (fun ppf () ->
          Format.fprintf ppf "Cannot decode function parameters");
      assert false);
    funDef.params <- params;
    body |> processExpr ~funDef ~env ~mem
  | Texp_tuple l -> l |> List.iter (processExpr ~funDef ~env ~mem)
  | Texp_let (Nonrecursive, [vb], inExpr) ->
    let scope =
      vb.vb_expr.exp_type |> processTyp ~funDef ~loc:vb.vb_expr.exp_loc
    in
    vb.vb_expr |> processExpr ~funDef ~env ~mem;
    emitLocalSetBackwards ~funDef ~scope;
    let newEnv = processLocalBinding ~env ~pat:vb.vb_pat ~scope in
    inExpr |> processExpr ~funDef ~env:newEnv ~mem
  | Texp_record {fields; extended_expression = None} ->
    let firstIndex = ref 0 in
    fields
    |> Array.iteri (fun i (_ld, (rld : Typedtree.record_label_definition)) ->
           match rld with
           | Kept _ -> assert false
           | Overridden ({loc}, e) ->
             let size = e.exp_type |> sizeOfTyp ~loc in
             let index = mem |> Il.Mem.alloc ~size in
             if i = 0 then firstIndex := index;
             funDef |> Il.FunDef.emit ~instr:(Il.Const (I32 0l));
             e |> processExpr ~funDef ~env ~mem;
             funDef |> Il.FunDef.emit ~instr:(Il.I32Store index));
    funDef
    |> Il.FunDef.emit ~instr:(Il.Const (I32 (!firstIndex |> Int32.of_int)))
  | Texp_field (e, {loc}, {lbl_name; lbl_all}) ->
    let offset = ref 0 in
    lbl_all
    |> Array.exists (fun (ld : Types.label_description) ->
           if ld.lbl_name = lbl_name then true
           else
             let size = ld.lbl_arg |> sizeOfTyp ~loc in
             offset := !offset + size;
             false)
    |> ignore;
    let index = !offset in
    funDef |> Il.FunDef.emit ~instr:(Il.Const (I32 0l));
    e |> processExpr ~funDef ~env ~mem;
    funDef |> Il.FunDef.emit ~instr:(Il.I32Load index)
  | _ ->
    Log_.info ~count:false ~loc:expr.exp_loc ~name:"Noalloc" (fun ppf () ->
        Format.fprintf ppf "Expression not supported");
    assert false

let rec processGlobal ~env ~id ~mem (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_constant const_ ->
    let const = const_ |> translateConst ~loc:expr.exp_loc ~mem in
    Il.Init.Const const
  | Texp_ident (id1, _, _) -> (
    let id1 = Path.name id1 in
    match env |> Il.Env.find ~id:id1 with
    | Some (GlobalDef globalDef) -> globalDef.init
    | _ ->
      Log_.info ~count:false ~loc:expr.exp_loc ~name:"Noalloc" (fun ppf () ->
          Format.fprintf ppf "processGlobal Id not found: %s" id);
      assert false)
  | Texp_tuple es -> Tuple (es |> List.map (processGlobal ~env ~id ~mem))
  | _ ->
    Log_.info ~count:false ~loc:expr.exp_loc ~name:"Noalloc" (fun ppf () ->
        Format.fprintf ppf "processGlobal not supported yet");
    assert false

let envRef = ref (Il.Env.create ())

let memRef = ref (Il.Mem.create ())

let processValueBinding ~id ~loc ~(expr : Typedtree.expression) =
  let id = Ident.name id in
  Log_.item "no-alloc binding for %s@." id;
  let kind = Il.Kind.fromType expr.exp_type in
  match kind with
  | Arrow _ ->
    let funDef = Il.FunDef.create ~id ~loc ~kind in
    envRef := !envRef |> Il.Env.add ~id ~def:(FunDef funDef);
    expr |> processExpr ~funDef ~env:!envRef ~mem:!memRef
  | _ ->
    let init = expr |> processGlobal ~env:!envRef ~id ~mem:!memRef in
    envRef := !envRef |> Il.Env.add ~id ~def:(GlobalDef {id; init})

let collectValueBinding super self (vb : Typedtree.value_binding) =
  (match vb.vb_pat.pat_desc with
  | Tpat_var (id, _)
    when vb.vb_attributes |> Annotation.hasAttribute (( = ) "noalloc") ->
    processValueBinding ~loc:vb.vb_loc ~id ~expr:vb.Typedtree.vb_expr
  | _ -> ());
  let r = super.Tast_mapper.value_binding self vb in
  r

let traverseStructure =
  let super = Tast_mapper.default in
  let value_binding self vb = vb |> collectValueBinding super self in
  let open Tast_mapper in
  {super with value_binding}

let processCmt (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Interface _ -> ()
  | Implementation structure ->
    structure |> traverseStructure.structure traverseStructure |> ignore
  | _ -> ()

let reportResults ~ppf =
  !memRef |> Il.Mem.dump ~ppf;
  !envRef |> Il.Env.dump ~ppf
