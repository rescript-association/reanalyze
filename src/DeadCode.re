open DeadCommon;

let (+++) = Filename.concat;

let processSignature = (~doValues, ~doTypes, signature: Types.signature) => {
  signature
  |> List.iter(sig_item =>
       DeadValue.processSignatureItem(
         ~doValues,
         ~doTypes,
         ~path=[currentModuleName^],
         sig_item,
       )
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
      if (debug^ && signature.sig_items == []) {
        Log_.item("Interface %d@.", signature.sig_items |> List.length);
      };
      interface(signature.sig_items);
    | Implementation(structure) =>
      if (debug^ && structure.str_items == []) {
        Log_.item("Implementation %d@.", structure.str_items |> List.length);
      };
      implementation(structure.str_items);
    | _ => None
    };
};

let loadCmtFile = cmtFilePath => {
  if (debug^) {
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
        processSignature(~doValues=true, ~doTypes=true, signature.sig_type);
      | Implementation(structure) =>
        let cmtiExists =
          Sys.file_exists(
            (cmtFilePath |> Filename.chop_extension) ++ ".cmti",
          );
        ProcessDeadAnnotations.structure(~doGenType=!cmtiExists, structure);
        processSignature(~doValues=true, ~doTypes=false, structure.str_type);
        DeadValue.processStructure(
          ~doTypes=true,
          ~doValues=true,
          ~cmt_value_dependencies,
          structure,
        );
      | _ => ()
      };
    };
    if (termination^) {
      switch (cmt_annots) {
      | Interface(_) => ()
      | Implementation(structure) => Arnold.processStructure(structure)
      | _ => ()
      };
    };
  };
};

let reportResults = ppf => {
  reportDead(ppf);
  WriteDeadAnnotations.write();
};

type analysisType =
  | Dce
  | Termination;

let runAnalysis = (~analysis, ~cmtRoot, ~ppf) => {
  switch (analysis) {
  | Dce => dce := true
  | Termination => termination := true
  };
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
  };
};