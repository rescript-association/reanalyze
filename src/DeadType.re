/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

let typeDependencies = ref([]);

let addTypeReference = (~posFrom, ~posTo) => {
  if (verbose) {
    Log_.item(
      "addTypeReference %s --> %s@.",
      posFrom |> posToString,
      posTo |> posToString,
    );
  };
  PosHash.addSet(typeReferences, posTo, posFrom);
};

let pathModuleToImplementation = path =>
  switch (path |> List.rev) {
  | [moduleName, ...rest] =>
    [moduleName |> Name.toImplementation, ...rest] |> List.rev
  | [] => path
  };

let pathModuleToInterface = path =>
  switch (path |> List.rev) {
  | [moduleName, ...rest] =>
    [moduleName |> Name.toInterface, ...rest] |> List.rev
  | [] => path
  };

let pathTypeToInterface = path =>
  switch (path) {
  | [typeName, ...rest] => [typeName |> Name.toInterface, ...rest]
  | [] => path
  };

let pathTypeToImplementation = path =>
  switch (path) {
  | [typeName, ...rest] => [typeName |> Name.toImplementation, ...rest]
  | [] => path
  };

let extendTypeDependencies = (loc1: Location.t, loc2: Location.t) =>
  if (loc1.loc_start != loc2.loc_start) {
    if (verbose) {
      Log_.item(
        "extendTypeDependencies %s --> %s@.",
        loc1.loc_start |> posToString,
        loc2.loc_start |> posToString,
      );
    };
    typeDependencies := [(loc1, loc2), ...typeDependencies^];
  };

let addTypeDependenciesImplementationInterface = (~loc, ~name, path_) => {
  let isInterface = Filename.check_suffix(currentSrc^, "i");
  if (!isInterface) {
    let path_1 = path_ |> pathModuleToInterface;
    let path_2 = path_1 |> pathTypeToInterface;
    let path1 = [name, ...path_1] |> pathToString;
    let path2 = [name, ...path_2] |> pathToString;

    switch (Hashtbl.find_opt(fields, path1)) {
    | None =>
      switch (Hashtbl.find_opt(fields, path2)) {
      | None => ()
      | Some(loc2) =>
        extendTypeDependencies(loc, loc2);
        if (!reportTypesDeadOnlyInInterface) {
          extendTypeDependencies(loc2, loc);
        };
      }
    | Some(loc1) =>
      extendTypeDependencies(loc, loc1);
      if (!reportTypesDeadOnlyInInterface) {
        extendTypeDependencies(loc1, loc);
      };
    };
  } else {
    let path_1 = path_ |> pathModuleToImplementation;
    let path_2 = path_1 |> pathTypeToImplementation;
    let path1 = [name, ...path_1] |> pathToString;
    let path2 = [name, ...path_2] |> pathToString;
    switch (Hashtbl.find_opt(fields, path1)) {
    | None =>
      switch (Hashtbl.find_opt(fields, path2)) {
      | None => ()
      | Some(loc2) =>
        extendTypeDependencies(loc2, loc);
        if (!reportTypesDeadOnlyInInterface) {
          extendTypeDependencies(loc, loc2);
        };
      }
    | Some(loc1) =>
      extendTypeDependencies(loc1, loc);
      if (!reportTypesDeadOnlyInInterface) {
        extendTypeDependencies(loc, loc1);
      };
    };
  };
};

let addDeclaration = (~path as path_, {type_kind}: Types.type_declaration) => {
  let save = (~declKind, ~loc: Location.t, ~name) => {
    let name = name |> Name.create;
    let path = [name, ...path_];
    addTypeDeclaration(~declKind, ~path=path_, ~loc, name);

    path_ |> addTypeDependenciesImplementationInterface(~loc, ~name);

    Hashtbl.replace(fields, path |> pathToString, loc);
  };

  switch (type_kind) {
  | Type_record(l, _) =>
    List.iter(
      ({Types.ld_id, ld_loc}) =>
        save(~declKind=RecordLabel, ~loc=ld_loc, ~name=Ident.name(ld_id)),
      l,
    )
  | Type_variant(l) =>
    List.iter(
      ({Types.cd_id, cd_loc}) =>
        save(~declKind=VariantCase, ~loc=cd_loc, ~name=Ident.name(cd_id)),
      l,
    )
  | _ => ()
  };
};

let processTypeDeclaration = (typeDeclaration: Typedtree.type_declaration) => {
  let updateDependencies = (name, loc) => {
    let path2 =
      [
        currentModuleName^,
        ...List.rev([
             name.Asttypes.txt |> Name.create,
             typeDeclaration.typ_name.txt |> Name.create,
             ...currentModulePath^,
           ]),
      ]
      |> List.map(Name.toString)
      |> String.concat(".");

    try(
      switch (typeDeclaration.typ_manifest) {
      | Some({ctyp_desc: Ttyp_constr(_, {txt}, _)}) =>
        let path1 =
          [currentModule^, ...Longident.flatten(txt)]
          @ [name.Asttypes.txt]
          |> String.concat(".");
        let loc1 = Hashtbl.find(fields, path1);
        let loc2 = Hashtbl.find(fields, path2);
        extendTypeDependencies(loc, loc1);
        extendTypeDependencies(loc1, loc2);
      | _ => ()
      }
    ) {
    | _ => ()
    };
    switch (Hashtbl.find_opt(fields, path2)) {
    | Some(loc2) =>
      extendTypeDependencies(loc, loc2);
      if (!reportTypesDeadOnlyInInterface) {
        extendTypeDependencies(loc2, loc);
      };
    | None => Hashtbl.add(fields, path2, loc)
    };
  };

  switch (typeDeclaration.typ_kind) {
  | Ttype_record(l) =>
    l
    |> List.iter(({Typedtree.ld_name, ld_loc}) =>
         updateDependencies(ld_name, ld_loc)
       )

  | Ttype_variant(l) =>
    l
    |> List.iter(({Typedtree.cd_name, cd_loc}) =>
         updateDependencies(cd_name, cd_loc)
       )

  | _ => ()
  };
};