open Common;

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
        let positions =
          locSet
          |> Common.LocSet.elements
          |> List.map(loc => loc.Location.loc_start);
        Format.fprintf(
          ppf,
          " @{<info>%s@} (@{<filename>%s@})",
          name,
          positions |> List.map(posToString) |> String.concat(" "),
        );
      | None => Format.fprintf(ppf, " @{<info>%s@}", name)
      }
    | None => Format.fprintf(ppf, " @{<info>%s@}", name)
    };
  };
  exceptions |> ExnSet.iter(ppExn);
};