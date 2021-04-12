let suppress = (ref [] : string list ref)

let unsuppress = (ref [] : string list ref)

let projectRoot = ref ""

let bsbProjectRoot = ref ""

let checkPrefix prefix_ =
  let prefix =
    match !projectRoot = "" with
    | true -> prefix_
    | false -> Filename.concat !projectRoot prefix_
  in
  let prefixLen = prefix |> String.length in
  fun sourceDir ->
    try String.sub sourceDir 0 prefixLen = prefix
    with Invalid_argument _ -> false

let suppressSourceDir =
  lazy
    (fun sourceDir ->
      !suppress |> List.exists (fun prefix -> checkPrefix prefix sourceDir))

let unsuppressSourceDir =
  lazy
    (fun sourceDir ->
      !unsuppress |> List.exists (fun prefix -> checkPrefix prefix sourceDir))

let posInSuppress (pos : Lexing.position) =
  pos.pos_fname |> Lazy.force suppressSourceDir

let posInUnsuppress (pos : Lexing.position) =
  pos.pos_fname |> Lazy.force unsuppressSourceDir

(** First suppress list, then override with unsuppress list *)
let filter pos = (not (posInSuppress pos)) || posInUnsuppress pos
