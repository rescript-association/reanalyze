/* Adapted from https://github.com/LexiFi/dead_code_analyzer */

open DeadCommon;

let typeDependencies = ref([]);

let addTypeReference = (~posFrom, ~posTo) => {
  if (Common.debug^) {
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

let extendTypeDependencies = (loc1: Location.t, loc2: Location.t) =>
  if (loc1.loc_start != loc2.loc_start) {
    if (Common.debug^) {
      Log_.item(
        "extendTypeDependencies %s --> %s@.",
        loc1.loc_start |> posToString,
        loc2.loc_start |> posToString,
      );
    };
    typeDependencies := [(loc1, loc2), ...typeDependencies^];
  };

// Type dependencies between Foo.re and Foo.rei
let addTypeDependenciesAcrossFiles = (~pahToType, ~loc, ~typeLabelName) => {
  let isInterface = Filename.check_suffix(Common.currentSrc^, "i");
  if (!isInterface) {
    let path_1 = pahToType |> pathModuleToInterface;
    let path_2 = path_1 |> pathTypeToInterface;
    let path1 = [typeLabelName, ...path_1] |> pathToString;
    let path2 = [typeLabelName, ...path_2] |> pathToString;

    switch (Hashtbl.find_opt(typeLabels, path1)) {
    | None =>
      switch (Hashtbl.find_opt(typeLabels, path2)) {
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
    let path_1 = pahToType |> pathModuleToImplementation;
    let path1 = [typeLabelName, ...path_1] |> pathToString;
    switch (Hashtbl.find_opt(typeLabels, path1)) {
    | None => ()
    | Some(loc1) =>
      extendTypeDependencies(loc1, loc);
      if (!reportTypesDeadOnlyInInterface) {
        extendTypeDependencies(loc, loc1);
      };
    };
  };
};

// Add type dependencies between implementation and interface in inner module
let addTypeDependenciesInnerModule = (~pahToType, ~loc, ~typeLabelName) => {
  let typeLabelPath = [typeLabelName, ...pahToType];

  let typeLabelPathStr = typeLabelPath |> pathToString;

  switch (Hashtbl.find_opt(typeLabels, typeLabelPathStr)) {
  | Some(loc2) =>
    extendTypeDependencies(loc, loc2);
    if (!reportTypesDeadOnlyInInterface) {
      extendTypeDependencies(loc2, loc);
    };
  | None => Hashtbl.add(typeLabels, typeLabelPathStr, loc)
  };
};

let addDeclaration = (~typeId: Ident.t, ~typeKind: Types.type_kind) => {
  let pahToType = [
    typeId |> Ident.name |> Name.create,
    ...currentModulePath^ @ [Common.currentModuleName^],
  ];

  let processTypeLabel = (typeLabelName, ~declKind, ~loc: Location.t) => {
    addTypeDeclaration(~declKind, ~path=pahToType, ~loc, typeLabelName);

    addTypeDependenciesAcrossFiles(~pahToType, ~loc, ~typeLabelName);
    addTypeDependenciesInnerModule(~pahToType, ~loc, ~typeLabelName);

    Hashtbl.replace(
      typeLabels,
      [typeLabelName, ...pahToType] |> pathToString,
      loc,
    );
  };

  switch (typeKind) {
  | Type_record(l, _) =>
    List.iter(
      ({Types.ld_id, ld_loc}) => {
        Ident.name(ld_id)
        |> Name.create
        |> processTypeLabel(~declKind=RecordLabel, ~loc=ld_loc)
      },
      l,
    )
  | Type_variant(l) =>
    List.iter(
      ({Types.cd_id, cd_loc}) => {
        Ident.name(cd_id)
        |> Name.create
        |> processTypeLabel(~declKind=VariantCase, ~loc=cd_loc)
      },
      l,
    )
  | _ => ()
  };
};