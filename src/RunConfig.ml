type t = {
  mutable dce : bool;
  mutable exception_ : bool;
  mutable termination : bool;
  mutable noalloc : bool;
}

let runConfig =
  {dce = false; exception_ = false; noalloc = false; termination = false}

let all () =
  runConfig.dce <- true;
  runConfig.exception_ <- true;
  runConfig.termination <- true

let dce () = runConfig.dce <- true

let exception_ () = runConfig.exception_ <- true

let noalloc () = runConfig.noalloc <- true

let termination () = runConfig.termination <- true
