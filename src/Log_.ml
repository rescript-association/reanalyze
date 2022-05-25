let useOcamlLocations = true

type language = Ml | Re | Res

let posLanguage (pos : Lexing.position) =
  if
    Filename.check_suffix pos.pos_fname ".re"
    || Filename.check_suffix pos.pos_fname ".rei"
  then Re
  else if
    Filename.check_suffix pos.pos_fname ".ml"
    || Filename.check_suffix pos.pos_fname ".mli"
  then Ml
  else Res

module Color = struct
  let color_enabled = lazy (Unix.isatty Unix.stdout)

  let forceColor = ref false

  let get_color_enabled () = !forceColor || Lazy.force color_enabled

  type color = Red | Yellow | Magenta | Cyan

  type style = FG of color | Bold | Dim

  let code_of_style = function
    | FG Red -> "31"
    | FG Yellow -> "33"
    | FG Magenta -> "35"
    | FG Cyan -> "36"
    | Bold -> "1"
    | Dim -> "2"

  let style_of_tag s =
    match s |> Compat.getStringTag with
    | "error" -> [Bold; FG Red]
    | "warning" -> [Bold; FG Magenta]
    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]
    | _ -> []

  let ansi_of_tag s =
    let l = style_of_tag s in
    let s = String.concat ";" (List.map code_of_style l) in
    "\027[" ^ s ^ "m"

  let reset_lit = "\027[0m"

  let color_functions =
    Compat.setOpenCloseTag
      (fun s -> if get_color_enabled () then ansi_of_tag s else "")
      (fun _ -> if get_color_enabled () then reset_lit else "")

  let setup () =
    Format.pp_set_mark_tags Format.std_formatter true;
    Compat.pp_set_formatter_tag_functions Format.std_formatter color_functions;
    if not (get_color_enabled ()) then CL.Misc.Color.setup (Some Never);
    if useOcamlLocations then
      CL.Location.print_loc Format.str_formatter CL.Location.none

  let error ppf s = Format.fprintf ppf "@{<error>%s@}" s

  let info ppf s = Format.fprintf ppf "@{<info>%s@}" s
end

module Loc = struct
  let print_filename ppf file =
    match file with
    (* modified *)
    | "_none_" | "" -> Format.fprintf ppf "(No file name)"
    | real_file -> Format.fprintf ppf "%s" (CL.Location.show_filename real_file)

  let print_loc ~normalizedRange ppf (loc : CL.Location.t) =
    let file, _, _ = CL.Location.get_pos_info loc.loc_start in
    if useOcamlLocations then
      (* do on extra dummy print, as the first time print_loc is used, flushing is incorrect *)
      let mkPosRelative (pos : Lexing.position) =
        {
          pos with
          pos_fname =
            (let open Filename in
            match is_implicit pos.pos_fname with
            | _ when !Common.Cli.ci ->
              concat current_dir_name (basename pos.pos_fname)
            | true -> concat (Sys.getcwd ()) pos.pos_fname
            | false -> pos.pos_fname);
        }
      in
      CL.Location.print_loc ppf
        {
          loc with
          loc_start = loc.loc_start |> mkPosRelative;
          loc_end = loc.loc_end |> mkPosRelative;
        }
    else
      let dim_loc ppf = function
        | None -> ()
        | Some
            ((start_line, start_line_start_char), (end_line, end_line_end_char))
          ->
          if start_line = end_line then
            if start_line_start_char = end_line_end_char then
              Format.fprintf ppf " @{<dim>%i:%i@}" start_line
                start_line_start_char
            else
              Format.fprintf ppf " @{<dim>%i:%i-%i@}" start_line
                start_line_start_char end_line_end_char
          else
            Format.fprintf ppf " @{<dim>%i:%i-%i:%i@}" start_line
              start_line_start_char end_line end_line_end_char
      in
      Format.fprintf ppf "File \"%a\", line %a" print_filename file dim_loc
        normalizedRange

  let print ppf (loc : CL.Location.t) =
    let _file, start_line, start_char =
      CL.Location.get_pos_info loc.loc_start
    in
    let _, end_line, end_char = CL.Location.get_pos_info loc.loc_end in
    let normalizedRange =
      if start_char == -1 || end_char == -1 then None
      else if start_line = end_line && start_char >= end_char then
        let same_char = start_char + 1 in
        Some ((start_line, same_char), (end_line, same_char))
      else Some ((start_line, start_char + 1), (end_line, end_char))
    in

    Format.fprintf ppf "@[%a@]" (print_loc ~normalizedRange) loc
end

let log x = Format.fprintf Format.std_formatter x

let item x =
  Format.fprintf Format.std_formatter "  ";
  Format.fprintf Format.std_formatter x

module Stats = struct
  let counters = Hashtbl.create 1

  let active = ref true

  let count name =
    if !active then
      match Hashtbl.find_opt counters (name : string) with
      | None -> Hashtbl.add counters name (ref 1)
      | Some cnt -> incr cnt

  let clear () = Hashtbl.clear counters

  let report () =
    if !active then (
      let issues, nIssues =
        Hashtbl.fold
          (fun name cnt (issues, nIssues) ->
            ((name, cnt) :: issues, nIssues + !cnt))
          counters ([], 0)
      in
      let sortedIssues =
        issues |> List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2)
      in
      if sortedIssues <> [] then item "@.";
      item "Analysis reported %d issues%s@." nIssues
        (match sortedIssues with
        | [] -> ""
        | _ :: _ ->
          " ("
          ^ (sortedIssues
            |> List.map (fun (name, cnt) -> name ^ ":" ^ string_of_int !cnt)
            |> String.concat ", ")
          ^ ")"))
end

let logKind body ~count ~color ~(loc : CL.Location.t) ~name =
  if Suppress.filter loc.loc_start then (
    if count then Stats.count name;
    Format.fprintf Format.std_formatter "@[<v 2>@,%a@,%a@,%a@]@." color name
      Loc.print loc body ())

let info ?(count = true) ~loc ~name body =
  body |> logKind ~color:Color.info ~count ~loc ~name

let error ~loc ~name body =
  body |> logKind ~color:Color.error ~count:true ~loc ~name
