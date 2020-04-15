open DeadCommon;

let (+++) = Filename.concat;

let rec getSignature = (~isfunc=false, moduleType: Types.module_type) =>
  switch (moduleType) {
  | Mty_signature(signature) => signature
  | Mty_functor(_, tOpt, _) when isfunc =>
    switch (tOpt) {
    | None => []
    | Some(moduleType) => getSignature(moduleType)
    }
  | Mty_functor(_, _, moduleType) => getSignature(moduleType)
  | _ => []
  };

let rec collectExportFromSignatureItem = (~path, si: Types.signature_item) =>
  switch (si) {
#if OCAML_MINOR >= 8
  | Sig_value(id, {Types.val_loc, val_kind}, _)
#else
  | Sig_value(id, {Types.val_loc, val_kind})
#endif
  when !val_loc.Location.loc_ghost =>
    let isPrimitive =
      switch (val_kind) {
      | Val_prim(_) => true
      | _ => false
      };
    if (!isPrimitive || analyzeExternals) {
      addValueDeclaration(
        ~sideEffects=false,
        ~path,
        ~loc=val_loc,
        Ident.name(id),
      );
    };
#if OCAML_MINOR >= 8
  | Sig_type(id, t, _, _) =>
#else
  | Sig_type(id, t, _) =>
#endif
    if (analyzeTypes^) {
      DeadType.addDeclaration(~path=[id |> Ident.name, ...path], t);
    }
  | (
#if OCAML_MINOR >= 8
      Sig_module(id, _, {Types.md_type: moduleType}, _, _) |
      Sig_modtype(id, _, {Types.mtd_type: Some(moduleType)}, _)
#else
      Sig_module(id, {Types.md_type: moduleType}, _) |
      Sig_modtype(id, {Types.mtd_type: Some(moduleType)})
#endif
    ) as s =>
    let collect =
      switch (s) {
      | Sig_modtype(_) => false
      | _ => true
      };
    if (collect) {
      getSignature(moduleType)
      |> List.iter(
           collectExportFromSignatureItem(~path=[id |> Ident.name, ...path]),
         );
    };
  | _ => ()
  };

let processSignature = (signature: Types.signature) => {
  let module_id = currentModuleName^;
  signature
  |> List.iter(sig_item =>
       collectExportFromSignatureItem(~path=[module_id], sig_item)
     );
};

module FindSourceFile = {
  let rec interface = items =>
    switch (items) {
    | [{Typedtree.sig_loc}, ...rest] =>
      !Sys.file_exists(sig_loc.loc_start.pos_fname)
        ? interface(rest) : Some(sig_loc.loc_start.pos_fname)
    | [] => None
    };
  let rec implementation = items =>
    switch (items) {
    | [{Typedtree.str_loc}, ...rest] =>
      !Sys.file_exists(str_loc.loc_start.pos_fname)
        ? implementation(rest) : Some(str_loc.loc_start.pos_fname)
    | [] => None
    };
  let cmt = cmt_annots =>
    switch (cmt_annots) {
    | Cmt_format.Interface(signature) =>
      if (signature.sig_items == []) {
        Log_.item("Interface %d@.", signature.sig_items |> List.length);
      };
      interface(signature.sig_items);
    | Implementation(structure) =>
      if (structure.str_items == []) {
        Log_.item("Implementation %d@.", structure.str_items |> List.length);
      };
      implementation(structure.str_items);
    | _ => None
    };
};

let loadCmtFile = cmtFilePath => {
  if (verbose) {
    Log_.item("Scanning %s@.", cmtFilePath);
  };

  let {Cmt_format.cmt_annots, cmt_value_dependencies} =
    Cmt_format.read_cmt(cmtFilePath);

  switch (cmt_annots |> FindSourceFile.cmt) {
  | None => ()

  | Some(sourceFile) =>
    FileHash.addFile(fileReferences, sourceFile);
    currentSrc := sourceFile;
    currentModuleName := Paths.getModuleName(sourceFile);

    if (dce^) {
      switch (cmt_annots) {
      | Interface(signature) =>
        ProcessDeadAnnotations.signature(signature);
        processSignature(signature.sig_type);
      | Implementation(structure) =>
        let cmtiExists =
          Sys.file_exists(
            (cmtFilePath |> Filename.chop_extension) ++ ".cmti",
          );
        if (!cmtiExists) {
          ProcessDeadAnnotations.structure(structure);
        };
        processSignature(structure.str_type);
        DeadValue.processStructure(~cmt_value_dependencies, structure);
      | _ => ()
      };
    };
    if (analyzeTermination^) {
      switch (cmt_annots) {
      | Interface(_) => ()
      | Implementation(structure) => Arnold.processStructure(structure)
      | _ => ()
      };
    };
  };
};

let reportResults = () => {
  reportDead();
  WriteDeadAnnotations.write();
};

let runAnalysis = (~cmtRoot) => {
  Log_.Color.setup();
  switch (cmtRoot) {
  | Some(root) =>
    let rec walkSubDirs = dir => {
      let absDir = dir == "" ? root : root +++ dir;
      let skipDir = {
        let base = Filename.basename(dir);
        base == "node_modules" || base == "_esy";
      };
      if (!skipDir && Sys.file_exists(absDir)) {
        if (Sys.is_directory(absDir)) {
          absDir |> Sys.readdir |> Array.iter(d => walkSubDirs(dir +++ d));
        } else if (Filename.check_suffix(absDir, ".cmt")
                   || Filename.check_suffix(absDir, ".cmti")) {
          absDir |> loadCmtFile;
        };
      };
    };
    walkSubDirs("");
    if (dce^) {
      reportResults();
    };
    if (analyzeTermination^) {
      Arnold.reportResults();
    };

  | None =>
    Paths.setProjectRoot();
    let lib_bs = Paths.projectRoot^ +++ "lib" +++ "bs";

    let sourceDirs = Paths.readSourceDirs(~configSources=None);
    sourceDirs
    |> List.iter(sourceDir => {
         let libBsSourceDir = Filename.concat(lib_bs, sourceDir);
         let files =
           switch (Sys.readdir(libBsSourceDir) |> Array.to_list) {
           | files => files
           | exception (Sys_error(_)) => []
           };
         let cmtFiles =
           files
           |> List.filter(x =>
                Filename.check_suffix(x, ".cmt")
                || Filename.check_suffix(x, ".cmti")
              );
         cmtFiles
         |> List.iter(cmtFile => {
              let cmtFilePath = Filename.concat(libBsSourceDir, cmtFile);
              cmtFilePath |> loadCmtFile;
            });
       });

    if (dce^) {
      reportResults();
    };
    if (analyzeTermination^) {
      Arnold.reportResults();
    };
  };
};

let runTerminationAnalysis = (~cmtRoot) => {
  dce := false;
  analyzeTermination := true;
  runAnalysis(~cmtRoot);
};