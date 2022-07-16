(** Prepend nativeBuildTarget to fname when provided (-native-build-target) *)
let nativeFilePath fname =
  match !Common.Cli.nativeBuildTarget with
  | None -> fname
  | Some nativeBuildTarget -> Filename.concat nativeBuildTarget fname

let rec interface items =
  match items with
  | {CL.Typedtree.sig_loc} :: rest -> (
    match
      not (Sys.file_exists (nativeFilePath sig_loc.loc_start.pos_fname))
    with
    | true -> interface rest
    | false -> Some sig_loc.loc_start.pos_fname)
  | [] -> None

let rec implementation items =
  match items with
  | {CL.Typedtree.str_loc} :: rest -> (
    match
      not (Sys.file_exists (nativeFilePath str_loc.loc_start.pos_fname))
    with
    | true -> implementation rest
    | false -> Some str_loc.loc_start.pos_fname)
  | [] -> None

let cmt cmt_annots =
  match cmt_annots with
  | CL.Cmt_format.Interface signature ->
    if !Common.Cli.debug && signature.sig_items = [] then
      Log_.item "Interface %d@." (signature.sig_items |> List.length);
    interface signature.sig_items
  | Implementation structure ->
    if !Common.Cli.debug && structure.str_items = [] then
      Log_.item "Implementation %d@." (structure.str_items |> List.length);
    implementation structure.str_items
  | _ -> None
