/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open Common;

module PosSet =
  Set.Make({
    type t = Lexing.position;
    let compare = compare;
  });

module Config = {
  // Turn on type analysis
  let analyzeTypes = ref(true);

  let analyzeExternals = true;

  let reportUnderscore = false;

  let reportTypesDeadOnlyInInterface = false;

  let recursiveDebug = false;

  let warnOnCircularDependencies = false;
};

module Current = {
  let bindings = ref(PosSet.empty);

  let lastBinding = ref(Location.none);

  let maxValuePosEnd = ref(Lexing.dummy_pos); // max end position of a value reported dead
};

let rec checkSub = (s1, s2, n) =>
  n <= 0
  || (
    try(s1.[n] == s2.[n]) {
    | Invalid_argument(_) => false
    }
  )
  && checkSub(s1, s2, n - 1);
let fileIsImplementationOf = (s1, s2) => {
  let n1 = String.length(s1)
  and n2 = String.length(s2);
  n2 == n1 + 1 && checkSub(s1, s2, n1 - 1);
};

let deadAnnotation = "dead";
let liveAnnotation = "live";

let posToString = posToString;

let posIsReason = Log_.posIsReason;

module PosHash = {
  include Hashtbl.Make({
    type t = Lexing.position;

    let hash = x => {
      let s = Filename.basename(x.Lexing.pos_fname);
      Hashtbl.hash((x.Lexing.pos_cnum, s));
    };

    let equal = (x: t, y) => x == y;
  });

  let findSet = (h, k) =>
    try(find(h, k)) {
    | Not_found => PosSet.empty
    };

  let addSet = (h, k, v) => {
    let set = findSet(h, k);
    replace(h, k, PosSet.add(v, set));
  };
};

module OptionalArgs = {
  type t = {mutable unused: StringSet.t};
  let empty = {unused: StringSet.empty};
  let fromList = l => {unused: StringSet.of_list(l)};
  let isEmpty = x => StringSet.is_empty(x.unused);
  let count = (name, x) => {unused: StringSet.remove(name, x.unused)};
  let combine = (x, y) => {
    let s = StringSet.inter(x.unused, y.unused);
    x.unused = s;
    y.unused = s;
  };
  let iter = (f, x) => StringSet.iter(f, x.unused);
};

module DeclKind = {
  type t =
    | Exception
    | RecordLabel
    | VariantCase
    | Value({
        isToplevel: bool,
        mutable optionalArgs: OptionalArgs.t,
        sideEffects: bool,
      });

  let isType = dk =>
    switch (dk) {
    | RecordLabel
    | VariantCase => true
    | Exception
    | Value(_) => false
    };

  let toString = dk =>
    switch (dk) {
    | Exception => "Exception"
    | RecordLabel => "RecordLabel"
    | VariantCase => "VariantCase"
    | Value(_) => "Value"
    };
};

type decl = {
  declKind: DeclKind.t,
  moduleLoc: Location.t,
  path: Path.t,
  pos: Lexing.position,
  posEnd: Lexing.position,
  posStart: Lexing.position,
  mutable resolved: bool,
};

type decls = PosHash.t(decl);

let decls: decls = PosHash.create(256); /* all exported declarations */

module ValueReferences = {
  let table: PosHash.t(PosSet.t) = PosHash.create(256); /* all value references */

  let add = (posTo, posFrom) => PosHash.addSet(table, posTo, posFrom);

  let find = pos => PosHash.findSet(table, pos);
};

module TypeReferences = {
  let table: PosHash.t(PosSet.t) = PosHash.create(256); /* all type references */

  let add = (posTo, posFrom) => PosHash.addSet(table, posTo, posFrom);

  let find = pos => PosHash.findSet(table, pos);
};

let declGetLoc = decl => {
  Location.loc_start: decl.posStart,
  loc_end: decl.posEnd,
  loc_ghost: false,
};

module ModulePath = {
  type t = {
    loc: Location.t,
    path: Path.t,
  };
  /* Keep track of the module path while traversing with Tast_mapper */
  let current: ref(t) = ref({loc: Location.none, path: []});

  let getCurrent = () => current^;

  let setCurrent = p => current := p;
};

/********   HELPERS   ********/

let addValueReference =
    (~addFileReference, ~locFrom: Location.t, ~locTo: Location.t) => {
  let lastBinding = Current.lastBinding^;
  let locFrom = lastBinding == Location.none ? locFrom : lastBinding;
  if (!locFrom.loc_ghost) {
    if (debug^) {
      Log_.item(
        "addValueReference %s --> %s@.",
        locFrom.loc_start |> posToString,
        locTo.loc_start |> posToString,
      );
    };
    ValueReferences.add(locTo.loc_start, locFrom.loc_start);
    if (addFileReference
        && !locTo.loc_ghost
        && !locFrom.loc_ghost
        && locFrom.loc_start.pos_fname != locTo.loc_start.pos_fname) {
      FileReferences.add(locFrom, locTo);
    };
  };
};

let iterFilesFromRootsToLeaves = iterFun => {
  /* For each file, the number of incoming references */
  let inverseReferences: Hashtbl.t(string, int) = Hashtbl.create(1);
  /* For each number of incoming references, the files */
  let referencesByNumber: Hashtbl.t(int, FileSet.t) = Hashtbl.create(1);

  let getNum = fileName =>
    try(Hashtbl.find(inverseReferences, fileName)) {
    | Not_found => 0
    };

  let getSet = num =>
    try(Hashtbl.find(referencesByNumber, num)) {
    | Not_found => FileSet.empty
    };

  let addIncomingEdge = fileName => {
    let oldNum = getNum(fileName);
    let newNum = oldNum + 1;
    let oldSetAtNum = getSet(oldNum);
    let newSetAtNum = FileSet.remove(fileName, oldSetAtNum);
    let oldSetAtNewNum = getSet(newNum);
    let newSetAtNewNum = FileSet.add(fileName, oldSetAtNewNum);
    Hashtbl.replace(inverseReferences, fileName, newNum);
    Hashtbl.replace(referencesByNumber, oldNum, newSetAtNum);
    Hashtbl.replace(referencesByNumber, newNum, newSetAtNewNum);
  };

  let removeIncomingEdge = fileName => {
    let oldNum = getNum(fileName);
    let newNum = oldNum - 1;
    let oldSetAtNum = getSet(oldNum);
    let newSetAtNum = FileSet.remove(fileName, oldSetAtNum);
    let oldSetAtNewNum = getSet(newNum);
    let newSetAtNewNum = FileSet.add(fileName, oldSetAtNewNum);
    Hashtbl.replace(inverseReferences, fileName, newNum);
    Hashtbl.replace(referencesByNumber, oldNum, newSetAtNum);
    Hashtbl.replace(referencesByNumber, newNum, newSetAtNewNum);
  };

  let addEdge = (fromFile, toFile) =>
    if (FileReferences.exists(fromFile)) {
      addIncomingEdge(toFile);
    };

  let removeEdge = (fromFile, toFile) =>
    if (FileReferences.exists(fromFile)) {
      removeIncomingEdge(toFile);
    };

  FileReferences.iter((fromFile, set) => {
    if (getNum(fromFile) == 0) {
      Hashtbl.replace(
        referencesByNumber,
        0,
        FileSet.add(fromFile, getSet(0)),
      );
    };
    set |> FileSet.iter(toFile => {addEdge(fromFile, toFile)});
  });

  while (getSet(0) != FileSet.empty) {
    let filesWithNoIncomingReferences = getSet(0);
    Hashtbl.remove(referencesByNumber, 0);
    filesWithNoIncomingReferences
    |> FileSet.iter(fileName => {
         iterFun(fileName);
         let references = FileReferences.find(fileName);
         references |> FileSet.iter(toFile => removeEdge(fileName, toFile));
       });
  };
  // Process any remaining items in case of circular references
  referencesByNumber
  |> Hashtbl.iter((_num, set) =>
       if (FileSet.is_empty(set)) {
         ();
       } else {
         set
         |> FileSet.iter(fileName => {
              let pos = {...Lexing.dummy_pos, pos_fname: fileName};
              let loc = {...Location.none, loc_start: pos, loc_end: pos};
              if (Config.warnOnCircularDependencies) {
                Log_.info(
                  ~loc, ~name="Warning Dead Analysis Cycle", (ppf, ()) =>
                  Format.fprintf(
                    ppf,
                    "Results for %s could be inaccurate because of circular references",
                    fileName,
                  )
                );
              };
              iterFun(fileName);
            });
       }
     );
};

/* Keep track of the location of values annotated @genType or @dead */
module ProcessDeadAnnotations = {
  type annotatedAs =
    | GenType
    | Dead
    | Live;

  let positionsAnnotated = PosHash.create(1);

  let isAnnotatedDead = pos =>
    PosHash.find_opt(positionsAnnotated, pos) == Some(Dead);

  let isAnnotatedGenTypeOrLive = pos =>
    switch (PosHash.find_opt(positionsAnnotated, pos)) {
    | Some(Live | GenType) => true
    | Some(Dead)
    | None => false
    };

  let isAnnotatedGenTypeOrDead = pos =>
    switch (PosHash.find_opt(positionsAnnotated, pos)) {
    | Some(Dead | GenType) => true
    | Some(Live)
    | None => false
    };

  let annotateGenType = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotated, pos, GenType);
  };

  let annotateDead = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotated, pos, Dead);
  };

  let annotateLive = (pos: Lexing.position) => {
    PosHash.replace(positionsAnnotated, pos, Live);
  };

  let processAttributes = (~doGenType, ~name, ~pos, attributes) => {
    let getPayloadFun = f => attributes |> Annotation.getAttributePayload(f);
    let getPayload = (x: string) =>
      attributes |> Annotation.getAttributePayload((==)(x));

    if (doGenType
        && getPayloadFun(Annotation.tagIsOneOfTheGenTypeAnnotations) != None) {
      pos |> annotateGenType;
    };

    if (getPayload(deadAnnotation) != None) {
      pos |> annotateDead;
    };

    let nameIsInLiveNamesOrPaths = () =>
      Cli.liveNames^
      |> List.mem(name)
      || {
        let fname = pos.pos_fname;
        let fnameLen = String.length(fname);
        Cli.livePaths^
        |> List.exists(prefix =>
             String.length(prefix) <= fnameLen
             && (
               try(String.sub(fname, 0, String.length(prefix)) == prefix) {
               | Invalid_argument(_) => false
               }
             )
           );
      };

    if (getPayload(liveAnnotation) != None || nameIsInLiveNamesOrPaths()) {
      pos |> annotateLive;
    };

    if (attributes |> Annotation.isOcamlSuppressDeadWarning) {
      pos |> annotateLive;
    };
  };

  let collectExportLocations = (~doGenType) => {
    let super = Tast_mapper.default;
    let currentlyDisableWarnings = ref(false);
    let value_binding =
        (
          self,
          {vb_attributes, vb_pat} as value_binding: Typedtree.value_binding,
        ) => {
      switch (vb_pat.pat_desc) {
      | Tpat_var(id, {loc: {loc_start: pos}}) =>
        if (currentlyDisableWarnings^) {
          pos |> annotateLive;
        };
        vb_attributes
        |> processAttributes(~doGenType, ~name=id |> Ident.name, ~pos);

      | _ => ()
      };
      super.value_binding(self, value_binding);
    };
    let type_kind = (self, typeKind: Typedtree.type_kind) => {
      switch (typeKind) {
      | Ttype_record(labelDeclarations) =>
        labelDeclarations
        |> List.iter(({ld_attributes, ld_loc}: Typedtree.label_declaration) =>
             ld_attributes
             |> processAttributes(~doGenType, ~name="", ~pos=ld_loc.loc_start)
           )
      | Ttype_variant(constructorDeclarations) =>
        constructorDeclarations
        |> List.iter(
             ({cd_attributes, cd_loc}: Typedtree.constructor_declaration) =>
             cd_attributes
             |> processAttributes(~doGenType, ~name="", ~pos=cd_loc.loc_start)
           )
      | _ => ()
      };
      super.type_kind(self, typeKind);
    };
    let value_description =
        (
          self,
          {val_attributes, val_id, val_val: {val_loc: {loc_start: pos}}} as value_description: Typedtree.value_description,
        ) => {
      if (currentlyDisableWarnings^) {
        pos |> annotateLive;
      };
      val_attributes
      |> processAttributes(~doGenType, ~name=val_id |> Ident.name, ~pos);
      super.value_description(self, value_description);
    };
    let structure_item = (self, item: Typedtree.structure_item) => {
      switch (item.str_desc) {
      | Tstr_attribute(attribute)
          when [attribute] |> Annotation.isOcamlSuppressDeadWarning =>
        currentlyDisableWarnings := true
      | _ => ()
      };
      super.structure_item(self, item);
    };
    let structure = (self, structure: Typedtree.structure) => {
      let oldDisableWarnings = currentlyDisableWarnings^;
      super.structure(self, structure) |> ignore;
      currentlyDisableWarnings := oldDisableWarnings;
      structure;
    };
    let signature_item = (self, item: Typedtree.signature_item) => {
      switch (item.sig_desc) {
      | Tsig_attribute(attribute)
          when [attribute] |> Annotation.isOcamlSuppressDeadWarning =>
        currentlyDisableWarnings := true
      | _ => ()
      };
      super.signature_item(self, item);
    };
    let signature = (self, signature: Typedtree.signature) => {
      let oldDisableWarnings = currentlyDisableWarnings^;
      super.signature(self, signature) |> ignore;
      currentlyDisableWarnings := oldDisableWarnings;
      signature;
    };

    {
      ...super,
      signature,
      signature_item,
      structure,
      structure_item,
      type_kind,
      value_binding,
      value_description,
    };
  };

  let structure = (~doGenType, structure) => {
    let collectExportLocations = collectExportLocations(~doGenType);
    structure
    |> collectExportLocations.structure(collectExportLocations)
    |> ignore;
  };
  let signature = signature => {
    let collectExportLocations = collectExportLocations(~doGenType=true);
    signature
    |> collectExportLocations.signature(collectExportLocations)
    |> ignore;
  };
};

/********   PROCESSING  ********/

let annotateAtEnd = (~pos) => !posIsReason(pos);

let getPosAnnotation = decl =>
  annotateAtEnd(~pos=decl.pos) ? decl.posEnd : decl.posStart;

let addDeclaration_ =
    (
      ~posEnd=?,
      ~posStart=?,
      ~declKind,
      ~path,
      ~loc: Location.t,
      ~moduleLoc,
      name: Name.t,
    ) => {
  let pos = loc.loc_start;
  let posStart =
    switch (posStart) {
    | Some(posStart) => posStart
    | None => pos
    };
  let posEnd =
    switch (posEnd) {
    | Some(posEnd) => posEnd
    | None => loc.loc_end
    };

  /* a .cmi file can contain locations from other files.
       For instance:
           module M : Set.S with type elt = int
       will create value definitions whose location is in set.mli
     */
  if (!loc.loc_ghost
      && (currentSrc^ == pos.pos_fname || currentModule^ === "*include*")) {
    if (debug^) {
      Log_.item(
        "add%sDeclaration %s %s path:%s@.",
        declKind |> DeclKind.toString,
        name |> Name.toString,
        pos |> posToString,
        path |> Path.toString,
      );
    };

    let decl = {
      declKind,
      moduleLoc,
      path: [name, ...path],
      pos,
      posEnd,
      posStart,
      resolved: false,
    };
    PosHash.replace(decls, pos, decl);
  };
};

let addValueDeclaration =
    (
      ~isToplevel=true,
      ~loc: Location.t,
      ~moduleLoc,
      ~optionalArgs=OptionalArgs.empty,
      ~path,
      ~sideEffects,
      name,
    ) =>
  name
  |> addDeclaration_(
       ~declKind=Value({isToplevel, optionalArgs, sideEffects}),
       ~loc,
       ~moduleLoc,
       ~path,
     );

/**** REPORTING ****/

let emitWarning = (~decl, ~message, ~name) => {
  Log_.info(~loc=decl |> declGetLoc, ~name, (ppf, ()) =>
    Format.fprintf(
      ppf,
      "@{<info>%s@} %s",
      decl.path |> Path.withoutHead,
      message,
    )
  );
};

module WriteDeadAnnotations = {
  type line = {
    mutable declarations: list(decl),
    original: string,
  };

  let rec lineToString_ = ({original, declarations}) => {
    switch (declarations) {
    | [] => original
    | [{declKind, path, pos} as decl, ...nextDeclarations] =>
      let isReason = posIsReason(pos);
      let annotationStr =
        (isReason ? "" : " ")
        ++ "["
        ++ (isReason || declKind |> DeclKind.isType ? "@" : "@@")
        ++ deadAnnotation
        ++ " \""
        ++ (path |> Path.withoutHead)
        ++ "\"] ";
      let posAnnotation = decl |> getPosAnnotation;
      let col = posAnnotation.pos_cnum - posAnnotation.pos_bol;
      let originalLen = String.length(original);
      {
        original:
          if (String.length(original) >= col && col > 0) {
            let (original1, original2) =
              try((
                String.sub(original, 0, col),
                String.sub(original, col, originalLen - col),
              )) {
              | Invalid_argument(_) => (original, "")
              };
            original1 ++ annotationStr ++ original2;
          } else {
            isReason ? annotationStr ++ original : original ++ annotationStr;
          },
        declarations: nextDeclarations,
      }
      |> lineToString_;
    };
  };

  let lineToString = ({original, declarations}) => {
    let declarations =
      declarations
      |> List.sort((decl1, decl2) =>
           getPosAnnotation(decl2).pos_cnum
           - getPosAnnotation(decl1).pos_cnum
         );
    lineToString_({original, declarations});
  };

  let currentFile = ref("");
  let currentFileLines: ref(array(line)) = ref([||]);

  let readFile = fileName => {
    let channel = open_in(fileName);
    let lines = ref([]);
    [@raises End_of_file]
    let rec loop = () => {
      let line = {original: input_line(channel), declarations: []};
      lines := [line, ...lines^];
      loop();
    };
    try(loop()) {
    | End_of_file =>
      close_in_noerr(channel);
      lines^ |> List.rev |> Array.of_list;
    };
  };

  let writeFile = (fileName, lines) =>
    if (fileName != "" && Cli.write^) {
      let channel = open_out(fileName);
      let lastLine = Array.length(lines);
      lines
      |> Array.iteri((n, line) => {
           output_string(channel, line |> lineToString);
           if (n < lastLine - 1) {
             output_char(channel, '\n');
           };
         });
      close_out_noerr(channel);
    };

  let onDeadDecl = (~ppf, decl) => {
    let fileName = decl.pos.pos_fname;
    if (Sys.file_exists(fileName)) {
      if (fileName != currentFile^) {
        writeFile(currentFile^, currentFileLines^);
        currentFile := fileName;
        currentFileLines := readFile(fileName);
      };

      let indexInLines = (decl |> getPosAnnotation).pos_lnum - 1;

      switch (currentFileLines^[indexInLines]) {
      | line =>
        line.declarations = [decl, ...line.declarations];
        Format.fprintf(
          ppf,
          "  <-- line %d@.  %s@.",
          decl.pos.pos_lnum,
          line |> lineToString,
        );
      | exception (Invalid_argument(_)) =>
        Format.fprintf(ppf, "  <-- Can't find line %d@.", decl.pos.pos_lnum)
      };
    } else {
      Format.fprintf(ppf, "  <-- can't find file@.");
    };
  };

  let write = () => writeFile(currentFile^, currentFileLines^);
};

module Decl = {
  let isValue = decl =>
    switch (decl.declKind) {
    | Value(_) => true
    | _ => false
    };

  let isToplevelValueWithSideEffects = decl =>
    switch (decl.declKind) {
    | Value({isToplevel, sideEffects}) => isToplevel && sideEffects
    | _ => false
    };

  let compareUsingDependencies =
      (
        ~orderedFiles,
        {
          declKind: kind1,
          path: _path1,
          pos: {
            pos_fname: fname1,
            pos_lnum: lnum1,
            pos_bol: bol1,
            pos_cnum: cnum1,
          },
        },
        {
          declKind: kind2,
          path: _path2,
          pos: {
            pos_fname: fname2,
            pos_lnum: lnum2,
            pos_bol: bol2,
            pos_cnum: cnum2,
          },
        },
      ) => {
    [@raises Not_found]
    let findPosition = fn => Hashtbl.find(orderedFiles, fn);

    /* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top. */
    let (position1, position2) =
      try((fname1 |> findPosition, fname2 |> findPosition)) {
      | Not_found => (0, 0)
      };
    compare(
      (position1, lnum2, bol2, cnum2, kind1),
      (position2, lnum1, bol1, cnum1, kind2),
    );
  };

  let compareForReporting =
      (
        {
          declKind: kind1,
          pos: {
            pos_fname: fname1,
            pos_lnum: lnum1,
            pos_bol: bol1,
            pos_cnum: cnum1,
          },
        },
        {
          declKind: kind2,
          pos: {
            pos_fname: fname2,
            pos_lnum: lnum2,
            pos_bol: bol2,
            pos_cnum: cnum2,
          },
        },
      ) => {
    compare(
      (fname1, lnum1, bol1, cnum1, kind1),
      (fname2, lnum2, bol2, cnum2, kind2),
    );
  };

  let isInsideReportedValue = decl => {
    let fileHasChanged =
      Current.maxValuePosEnd^.pos_fname != decl.pos.pos_fname;

    let insideReportedValue =
      decl
      |> isValue
      && !fileHasChanged
      && Current.maxValuePosEnd^.pos_cnum > decl.pos.pos_cnum;

    if (!insideReportedValue) {
      if (decl |> isValue) {
        if (fileHasChanged
            || decl.posEnd.pos_cnum > Current.maxValuePosEnd^.pos_cnum) {
          Current.maxValuePosEnd := decl.posEnd;
        };
      };
    };

    insideReportedValue;
  };

  let report = (~ppf, decl) => {
    let (name, message) =
      switch (decl.declKind) {
      | Exception => (
          "Warning Dead Exception",
          "is never raised or passed as value",
        )
      | Value({sideEffects}) =>
        let noSideEffectsOrUnderscore =
          !sideEffects
          || (
            switch (decl.path) {
            | [hd, ..._] => hd |> Name.startsWithUnderscore
            | [] => false
            }
          );
        (
          "Warning Dead Value"
          ++ (!noSideEffectsOrUnderscore ? " With Side Effects" : ""),
          switch (decl.path) {
          | [name, ..._] when name |> Name.isUnderscore => "has no side effects and can be removed"
          | _ =>
            "is never used"
            ++ (
              !noSideEffectsOrUnderscore ? " and could have side effects" : ""
            )
          },
        );
      | RecordLabel => (
          "Warning Dead Type",
          "is a record label never used to read a value",
        )
      | VariantCase => (
          "Warning Dead Type",
          "is a variant case which is never constructed",
        )
      };

    let insideReportedValue = decl |> isInsideReportedValue;

    let shouldEmitWarning =
      !insideReportedValue
      && (
        switch (decl.path) {
        | [name, ..._] when name |> Name.isUnderscore => Config.reportUnderscore
        | _ => true
        }
      );
    let shouldWriteAnnotation =
      shouldEmitWarning
      && !isToplevelValueWithSideEffects(decl)
      && Suppress.filter(decl.pos);
    if (shouldEmitWarning) {
      decl.path
      |> Path.toModuleName(~isValue=decl |> isValue)
      |> DeadModules.checkModuleDead(~fileName=decl.pos.pos_fname);
      emitWarning(~decl, ~message, ~name);
    };
    if (shouldWriteAnnotation) {
      decl |> WriteDeadAnnotations.onDeadDecl(~ppf);
    };
  };
};

let declIsDead = (~refs, decl) => {
  let liveRefs =
    refs |> PosSet.filter(p => !ProcessDeadAnnotations.isAnnotatedDead(p));
  liveRefs
  |> PosSet.cardinal == 0
  && !ProcessDeadAnnotations.isAnnotatedGenTypeOrLive(decl.pos);
};

let doReportDead = pos =>
  !ProcessDeadAnnotations.isAnnotatedGenTypeOrDead(pos);

let rec resolveRecursiveRefs =
        (
          ~checkOptionalArg,
          ~deadDeclarations,
          ~level,
          ~orderedFiles,
          ~refs,
          ~refsBeingResolved,
          decl,
        )
        : bool => {
  switch (decl.pos) {
  | _ when decl.resolved =>
    if (Config.recursiveDebug) {
      Log_.item(
        "recursiveDebug %s [%d] already resolved@.",
        decl.path |> Path.toString,
        level,
      );
    };
    decl.pos |> ProcessDeadAnnotations.isAnnotatedDead;
  | _ when PosSet.mem(decl.pos, refsBeingResolved^) =>
    if (Config.recursiveDebug) {
      Log_.item(
        "recursiveDebug %s [%d] is being resolved: assume dead@.",
        decl.path |> Path.toString,
        level,
      );
    };
    true;
  | _ =>
    if (Config.recursiveDebug) {
      Log_.item(
        "recursiveDebug resolving %s [%d]@.",
        decl.path |> Path.toString,
        level,
      );
    };
    refsBeingResolved := PosSet.add(decl.pos, refsBeingResolved^);
    let allDepsResolved = ref(true);
    let newRefs =
      refs
      |> PosSet.filter(pos =>
           if (pos == decl.pos) {
             if (Config.recursiveDebug) {
               Log_.item(
                 "recursiveDebug %s ignoring reference to self@.",
                 decl.path |> Path.toString,
               );
             };
             false;
           } else {
             switch (PosHash.find_opt(decls, pos)) {
             | None =>
               if (Config.recursiveDebug) {
                 Log_.item(
                   "recursiveDebug can't find decl for %s@.",
                   pos |> posToString,
                 );
               };
               true;
             | Some(xDecl) =>
               let xRefs =
                 xDecl.declKind |> DeclKind.isType
                   ? TypeReferences.find(pos) : ValueReferences.find(pos);
               let xDeclIsDead =
                 xDecl
                 |> resolveRecursiveRefs(
                      ~checkOptionalArg,
                      ~deadDeclarations,
                      ~level=level + 1,
                      ~orderedFiles,
                      ~refs=xRefs,
                      ~refsBeingResolved,
                    );
               if (!xDecl.resolved) {
                 allDepsResolved := false;
               };
               !xDeclIsDead;
             };
           }
         );

    let isDead = decl |> declIsDead(~refs=newRefs);
    let isResolved = !isDead || allDepsResolved^ || level == 0;

    if (isResolved) {
      decl.resolved = true;

      if (isDead) {
        decl.path
        |> DeadModules.markDead(
             ~isValue=decl |> Decl.isValue,
             ~loc=decl.moduleLoc,
           );

        if (decl.pos |> doReportDead) {
          deadDeclarations := [decl, ...deadDeclarations^];
        };
        if (!Decl.isToplevelValueWithSideEffects(decl)) {
          decl.pos |> ProcessDeadAnnotations.annotateDead;
        };
      } else {
        checkOptionalArg(decl);
        if (decl.pos |> ProcessDeadAnnotations.isAnnotatedDead) {
          emitWarning(
            ~decl,
            ~message=" is annotated @dead but is live",
            ~name="Warning Incorrect Annotation",
          );
        } else {
          decl.path
          |> DeadModules.markLive(
               ~isValue=decl |> Decl.isValue,
               ~loc=decl.moduleLoc,
             );
        };
      };

      if (debug^) {
        let refsString =
          newRefs
          |> PosSet.elements
          |> List.map(posToString)
          |> String.concat(", ");
        Log_.item(
          "%s %s %s: %d references (%s) [%d]@.",
          isDead ? "Dead" : "Live",
          decl.declKind |> DeclKind.toString,
          decl.path |> Path.toString,
          newRefs |> PosSet.cardinal,
          refsString,
          level,
        );
      };
    };

    isDead;
  };
};

let reportDead = (~checkOptionalArg, ppf) => {
  let iterDeclInOrder = (~deadDeclarations, ~orderedFiles, decl) => {
    let refs =
      decl |> Decl.isValue
        ? ValueReferences.find(decl.pos) : TypeReferences.find(decl.pos);
    resolveRecursiveRefs(
      ~checkOptionalArg,
      ~deadDeclarations,
      ~level=0,
      ~orderedFiles,
      ~refsBeingResolved=ref(PosSet.empty),
      ~refs,
      decl,
    )
    |> ignore;
  };

  if (debug^) {
    Log_.item("@.File References@.@.");
    let fileList = ref([]);
    FileReferences.iter((file, files) =>
      fileList := [(file, files), ...fileList^]
    );
    fileList^
    |> List.sort(((f1, _), (f2, _)) => String.compare(f1, f2))
    |> List.iter(((file, files)) =>
         Log_.item(
           "%s -->> %s@.",
           file |> Filename.basename,
           files
           |> FileSet.elements
           |> List.map(Filename.basename)
           |> String.concat(", "),
         )
       );
  };

  let declarations =
    PosHash.fold(
      (_pos, decl, declarations) => [decl, ...declarations],
      decls,
      [],
    );

  let orderedFiles = Hashtbl.create(256);
  iterFilesFromRootsToLeaves(
    {
      let current = ref(0);
      fileName => {
        incr(current);
        Hashtbl.add(orderedFiles, fileName, current^);
      };
    },
  );

  let orderedDeclarations =
    declarations
    |> List.fast_sort(Decl.compareUsingDependencies(~orderedFiles)) /* analyze in reverse order */;

  let deadDeclarations = ref([]);
  orderedDeclarations
  |> List.iter(iterDeclInOrder(~orderedFiles, ~deadDeclarations));

  let sortedDeadDeclarations =
    deadDeclarations^ |> List.fast_sort(Decl.compareForReporting);
  sortedDeadDeclarations |> List.iter(Decl.report(~ppf));
};