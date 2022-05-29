let items = ref 0

let start () = Format.fprintf Format.std_formatter "["

let finish () = Format.fprintf Format.std_formatter "\n]\n"

let emitClose () =
  Format.fprintf Format.std_formatter (if !items = 0 then "\n}" else "\n}")

let emitItem ~name ~kind ~file ~range ~message =
  let open Format in
  items := !items + 1;
  let ppf = std_formatter in
  let startLine, startCharacter, endLine, endCharacter = range in
  fprintf ppf "%s{\n" (if !items = 1 then "\n" else ",\n");
  fprintf ppf "  \"name\": \"%s\",@." name;
  fprintf ppf "  \"kind\": \"%s\",@." kind;
  fprintf ppf "  \"file\": \"%s\",@." file;
  fprintf ppf "  \"range\": [%d,%d,%d,%d],@." startLine startCharacter endLine
    endCharacter;
  fprintf ppf "  \"message\": \"%s\"" message

let emitAnnotate ~line ~character ~text =
  Format.fprintf Format.std_formatter
    ",@.  \"annotate\": { \"line\": %d, \"character\": %d, \"text\": \"%s\"}@."
    line character text
