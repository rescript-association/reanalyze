type t;
let compare: (t, t) => int;

let endOfFile: t;
let failure: t;
let fromLid: Asttypes.loc(Longident.t) => t;
let fromString: string => t;
let matchFailure: t;
let invalidArgument: t;
let notFound: t;
let toString: t => string;