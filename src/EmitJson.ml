let emitItem ~isFirst ~isClosing ~name ~kind ~file ~range ~message ppf =
  let open Format in
  let startLine, startCharacter, endLine, endCharacter = range in
  fprintf ppf "%s{\n" (if isFirst then "\n" else ",\n");
  fprintf ppf "  \"name\": \"%s\",@." name;
  fprintf ppf "  \"kind\": \"%s\",@." kind;
  fprintf ppf "  \"file\": \"%s\",@." file;
  fprintf ppf "  \"range\": [%d,%d,%d,%d],@." startLine startCharacter endLine
    endCharacter;
  fprintf ppf "  \"message\": \"%s\"" message;
  if isClosing then fprintf ppf "@.}"

let emitAnnotate ~line ~character ~text ppf =
  Format.fprintf ppf
    ",@.  \"annotate\": { \"line\": %d, \"character\": %d, \"text\": \"%s\"}@."
    line character text
