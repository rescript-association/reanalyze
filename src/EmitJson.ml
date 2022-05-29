let emitItem ~isFirst ~isClosing ~name ~kind ~file ~range ~message =
  let open Format in
  let ppf = std_formatter in
  let startLine, startCharacter, endLine, endCharacter = range in
  fprintf ppf "%s{\n" (if isFirst then "\n" else ",\n");
  fprintf ppf "  \"name\": \"%s\",@." name;
  fprintf ppf "  \"kind\": \"%s\",@." kind;
  fprintf ppf "  \"file\": \"%s\",@." file;
  fprintf ppf "  \"range\": [%d,%d,%d,%d],@." startLine startCharacter endLine
    endCharacter;
  fprintf ppf "  \"message\": \"%s\"" message;
  if isClosing then fprintf ppf "@.}"

let emitClose () = Format.fprintf Format.std_formatter "}\n"

let emitAnnotate ~line ~character ~text =
  Format.fprintf Format.std_formatter
    ",@.  \"annotate\": { \"line\": %d, \"character\": %d, \"text\": \"%s\"}@."
    line character text
