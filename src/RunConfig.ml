type t = {
  mutable bsbProjectRoot : string;
  mutable dce : bool;
  mutable exception_ : bool;
  mutable noalloc : bool;
  mutable projectRoot : string;
  mutable suppress : string list;
  mutable termination : bool;
  mutable unsuppress : string list;
}

let runConfig =
  {
    bsbProjectRoot = "";
    dce = false;
    exception_ = false;
    noalloc = false;
    projectRoot = "";
    suppress = [];
    termination = false;
    unsuppress = [];
  }

let all () =
  runConfig.dce <- true;
  runConfig.exception_ <- true;
  runConfig.termination <- true

let dce () = runConfig.dce <- true

let exception_ () = runConfig.exception_ <- true

let noalloc () = runConfig.noalloc <- true

let termination () = runConfig.termination <- true
