type t = string;

let compare = String.compare;

let decodeError = {"DecodeError"}

let divisionByZero = "Division_by_zero";

let endOfFile = "End_of_file";

let failure = "Failure";

let invalidArgument = "Invalid_argument";

let jsExnError = "Js.Exn.Error";

let matchFailure = "Match_failure";

let notFound = "Not_found";

let sysError = "Sys_error";

let fromLid = lid =>
  lid.Asttypes.txt |> Longident.flatten |> String.concat(".");

let fromString = s => s;

let toString = s => s;
