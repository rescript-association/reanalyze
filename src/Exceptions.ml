open Common
module ExnSet = Set.Make (Exn)

type t = ExnSet.t

let add = ExnSet.add
let diff = ExnSet.diff
let empty = ExnSet.empty
let fromList = ExnSet.of_list
let toList = ExnSet.elements
let isEmpty = ExnSet.is_empty
let iter = ExnSet.iter
let union = ExnSet.union

let pp ~exnTable ppf exceptions =
  let ppExn exn =
    let name = Exn.toString exn in
    match exnTable with
    | Some exnTable -> (
      match Hashtbl.find_opt exnTable exn with
      | Some locSet ->
        let positions =
          locSet |> Common.LocSet.elements
          |> List.map (fun loc -> loc.CL.Location.loc_start)
        in
        Format.fprintf ppf " @{<info>%s@} (@{<filename>%s@})" name
          (positions |> List.map posToString |> String.concat " ")
      | None -> Format.fprintf ppf " @{<info>%s@}" name)
    | None -> Format.fprintf ppf " @{<info>%s@}" name
  in
  exceptions |> ExnSet.iter ppExn
