type t = {
  mutable dce : bool;
  mutable exception_ : bool;
  mutable termination : bool;
  mutable noalloc : bool;
}

let default () =
  {dce = false; exception_ = false; noalloc = false; termination = false}

let fromBsconfig = default ()

let all = {(default ()) with dce = true; exception_ = true; termination = true}

let dce = {(default ()) with dce = true}

let exception_ = {(default ()) with exception_ = true}

let noalloc = {(default ()) with noalloc = true}

let termination = {(default ()) with termination = true}
