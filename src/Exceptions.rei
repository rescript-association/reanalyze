type t;

let add: (Exn.t, t) => t;

let diff: (t, t) => t;
let empty: t;

let isEmpty: t => bool;

let iter: (Exn.t => unit, t) => unit;

let fromList: list(Exn.t) => t;

let pp:
  (~exnTable: option(Hashtbl.t(Exn.t, DeadCommon.LocSet.t)), Format.formatter, t) =>
  unit;

let toList: t => list(Exn.t);

let union: (t, t) => t;