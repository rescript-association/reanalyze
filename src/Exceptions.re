module ExnSet = Set.Make(Exn);

type t = ExnSet.t;

let add = ExnSet.add;

let diff = ExnSet.diff;

let empty = ExnSet.empty;

let fromList = ExnSet.of_list;
let toList = ExnSet.elements;
let isEmpty = ExnSet.is_empty;

let iter = ExnSet.iter;

let union = ExnSet.union;

let pp = (~exnTable, ppf, exceptions) => {
  let ppExn = exn => {
    let name = Exn.toString(exn);
    switch (exnTable) {
    | Some(exnTable) =>
      switch (Hashtbl.find_opt(exnTable, exn)) {
      | Some(locSet) =>
        let loc = locSet |> DeadCommon.LocSet.max_elt;
        Format.fprintf(
          ppf,
          " @{<info>%s@} (@{<filename>%s@})",
          name,
          DeadCommon.posToString(loc.Location.loc_start),
        );
      | None => Format.fprintf(ppf, " @{<info>%s@}", name)
      }
    | None => Format.fprintf(ppf, " @{<info>%s@}", name)
    };
  };
  exceptions |> ExnSet.iter(ppExn);
};