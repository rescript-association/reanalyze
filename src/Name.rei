type t;
let compare: (t, t) => int;
let create: (~isInterface: bool=?, string) => t;
let isUnderscore: t => bool;
let startsWithUnderscore: t => bool;
let toImplementation: t => t;
let toInterface: t => t;
let toString: t => string;
