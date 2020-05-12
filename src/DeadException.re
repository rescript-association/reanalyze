open DeadCommon;

type item = {
  exceptionPath: Path.t,
  locFrom: Location.t,
};

let delayedItems = ref([]);
let declarations = Hashtbl.create(1);

let add = (~path, ~loc, ~strLoc: Location.t, name) => {
  let exceptionPath = [name, ...path];
  Hashtbl.add(declarations, exceptionPath, loc);
  name
  |> addDeclaration_(
       ~posEnd=strLoc.loc_end,
       ~posStart=strLoc.loc_start,
       ~declKind=Exception,
       ~moduleLoc=ModulePath.getCurrent().loc,
       ~path,
       ~loc,
     );
};

let forceDelayedItems = () => {
  let items = delayedItems^ |> List.rev;
  delayedItems := [];
  items
  |> List.iter(({exceptionPath, locFrom}) => {
       switch (Hashtbl.find_opt(declarations, exceptionPath)) {
       | None => ()
       | Some(locTo) =>
         addValueReference(~addFileReference=true, ~locFrom, ~locTo)
       }
     });
};

let markAsUsed = (~locFrom: Location.t, ~locTo: Location.t, path_) =>
  if (locTo.loc_ghost) {
    // Probably defined in another file, delay processing and check at the end
    let exceptionPath = path_ |> Path.fromPathT |> Path.moduleToImplementation;
    delayedItems := [{exceptionPath, locFrom}, ...delayedItems^];
  } else {
    addValueReference(~addFileReference=true, ~locFrom, ~locTo);
  };