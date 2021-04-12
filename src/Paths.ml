module StringMap = Map.Make (String)

let bsconfig = "bsconfig.json"

let rec findProjectRoot ~dir =
  if Sys.file_exists (Filename.concat dir bsconfig) then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ bsconfig ^ ".");
      assert false)
    else findProjectRoot ~dir:parent

let setProjectRoot () =
  Suppress.projectRoot := findProjectRoot ~dir:(Sys.getcwd ());
  Suppress.bsbProjectRoot :=
    match Sys.getenv_opt "BSB_PROJECT_ROOT" with
    | None -> !Suppress.projectRoot
    | Some s -> s

(**
  * Handle namespaces in cmt files.
  * E.g. src/Module-Project.cmt becomes src/Module
  *)
let handleNamespace cmt =
  let cutAfterDash s =
    match String.index s '-' with
    | n -> ( try String.sub s 0 n with Invalid_argument _ -> s)
    | exception Not_found -> s
  in
  let noDir = Filename.basename cmt = cmt in
  if noDir then cmt |> Filename.remove_extension |> cutAfterDash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> Filename.remove_extension |> cutAfterDash
    in
    Filename.concat dir base

let getModuleName cmt = cmt |> handleNamespace |> Filename.basename

let readDirsFromConfig ~configSources =
  let dirs = ref [] in
  let root = !Suppress.projectRoot in
  let rec processDir ~subdirs dir =
    let absDir =
      match dir = "" with true -> root | false -> Filename.concat root dir
    in
    if Sys.file_exists absDir && Sys.is_directory absDir then (
      dirs := dir :: !dirs;
      if subdirs then
        absDir |> Sys.readdir
        |> Array.iter (fun d -> processDir ~subdirs (Filename.concat dir d)))
  in
  let rec processSourceItem (sourceItem : Ext_json_types.t) =
    match sourceItem with
    | Str str -> str |> processDir ~subdirs:false
    | Obj map -> (
      match map |> StringMap.find_opt "dir" with
      | Some (Str str) ->
        let subdirs =
          match map |> StringMap.find_opt "subdirs" with
          | Some (True _) -> true
          | Some (False _) -> false
          | _ -> false
        in
        str |> processDir ~subdirs
      | _ -> ())
    | Arr arr -> arr |> Array.iter processSourceItem
    | _ -> ()
  in
  (match configSources with
  | Some sourceItem -> processSourceItem sourceItem
  | None -> ());
  !dirs

let readSourceDirs ~configSources =
  let sourceDirs =
    ["lib"; "bs"; ".sourcedirs.json"]
    |> List.fold_left Filename.concat !Suppress.bsbProjectRoot
  in
  let dirs = ref [] in
  let readDirs json =
    match json with
    | Ext_json_types.Obj map -> (
      match map |> StringMap.find_opt "dirs" with
      | Some (Arr arr) ->
        arr
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Str str -> dirs := str :: !dirs
               | _ -> ());
        ()
      | _ -> ())
    | _ -> ()
  in
  if sourceDirs |> Sys.file_exists then
    let jsonOpt = sourceDirs |> Ext_json_parse.parse_json_from_file in
    match jsonOpt with
    | Some json ->
      if !Suppress.bsbProjectRoot <> !Suppress.projectRoot then (
        readDirs json;
        dirs := readDirsFromConfig ~configSources)
      else readDirs json
    | None -> ()
  else (
    Log_.item "Warning: can't find source dirs: %s\n" sourceDirs;
    Log_.item "Types for cross-references will not be found by genType.\n";
    dirs := readDirsFromConfig ~configSources);
  !dirs
