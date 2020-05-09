type t = string;
let create = (~isInterface=true, s) => isInterface ? s : "+" ++ s;
let isInterface = s =>
  try(s.[0] != '+') {
  | Invalid_argument(_) => false
  };
let isUnderscore = s => s == "_" || s == "+_";
let startsWithUnderscore = s =>
  s
  |> String.length >= 2
  && (
    try(s.[0] == '_' || s.[0] == '+' && s.[1] == '_') {
    | Invalid_argument(_) => false
    }
  );
let toInterface = s =>
  isInterface(s)
    ? s
    : (
      try(String.sub(s, 1, String.length(s) - 1)) {
      | Invalid_argument(_) => s
      }
    );
let toImplementation = s => isInterface(s) ? "+" ++ s : s;
let toString = s => s;