open DeadCommon;

let (+++) = Filename.concat;

let rec processSignatureItem = (~doValues, ~path, si: Types.signature_item) =>
  switch (si) {
  | Sig_value(_) when doValues =>
    let (id, loc, kind) = si |> Compat.getSigValue;
    if (!loc.Location.loc_ghost) {
      let isPrimitive =
        switch (kind) {
        | Val_prim(_) => true
        | _ => false
        };
      if (!isPrimitive || analyzeExternals) {
        addValueDeclaration(
          ~sideEffects=false,
          ~path,
          ~loc,
          Ident.name(id) |> Name.create,
        );
      };
    };
  | Sig_module(_)
  | Sig_modtype(_) =>
    switch (si |> Compat.getSigModuleModtype) {
    | Some((id, moduleType)) =>
      let collect =
        switch (si) {
        | Sig_modtype(_) => false
        | _ => true
        };
      if (collect) {
        DeadValue.getSignature(moduleType)
        |> List.iter(
             processSignatureItem(
               ~doValues,
               ~path=[id |> Ident.name |> Name.create, ...path],
             ),
           );
      };
    | None => ()
    }
  | _ => ()
  };

let processSignature = (~doValues, signature: Types.signature) => {
  signature
  |> List.iter(sig_item =>
       processSignatureItem(~doValues, ~path=[currentModuleName^], sig_item)
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
    currentModule := Paths.getModuleName(sourceFile);
    currentModuleName :=
      currentModule^
      |> Name.create(~isInterface=Filename.check_suffix(currentSrc^, "i"));

    if (dce^) {
      switch (cmt_annots) {
      | Interface(signature) =>
        ProcessDeadAnnotations.signature(signature);
        processSignature(~doValues=true, signature.sig_type);
      | Implementation(structure) =>
        let cmtiExists =
          Sys.file_exists(
            (cmtFilePath |> Filename.chop_extension) ++ ".cmti",
          );
        ProcessDeadAnnotations.structure(~doGenType=!cmtiExists, structure);
        processSignature(~doValues=true, structure.str_type);
        DeadValue.processStructure(
          ~doTypes=true,
          ~doValues=true,
          ~cmt_value_dependencies,
          structure,
        );
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