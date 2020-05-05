let raises = () => raise(Not_found);

let catches1 =
  try() {
  | Not_found => ()
  };

let catches2 =
  switch () {
  | _ => ()
  | exception Not_found => ()
  };

let raiseAndCatch =
  try(raise(Not_found)) {
  | _ => ()
  };

[@raises Not_found]
let raisesWithAnnotaion = () => raise(Not_found);

let callsRaiseWithAnnotation = raisesWithAnnotaion();

[@raises A]
let callsRaiseWithAnnotationAndIsAnnotated = raisesWithAnnotaion();

let z = List.hd([]);

let incompleteMatch = l =>
  switch (l) {
  | [] => ()
  };

exception A;
exception B;

let twoRaises = (x, y) => {
  if (x) {
    raise(A);
  };
  if (y) {
    raise(B);
  };
};

let sequencing = () => {
  raise(A);
  try(raise(B)) {
  | _ => ()
  };
};

let wrongCatch = () =>
  try(raise(B)) {
  | A => ()
  };

exception C;
let wrongCatch2 = b =>
  switch (b ? raise(B) : raise(C)) {
  | exception A => ()
  | exception B => ()
  | [] => ()
  };

[@raises (A, B, C)]
let raise2Annotate3 = (x, y) => {
  if (x) {
    raise(A);
  };
  if (y) {
    raise(B);
  };
};

exception Error(string, string, int);

let parse_json_from_file = s => {
  let in_chan = open_in(s);
  switch (34) {
  | exception (Error(p1, p2, e)) =>
    close_in(in_chan);
    raise(Error(p1, p2, e));
  | v =>
    close_in(in_chan);
    v;
  };
};

let reRaise = () =>
  switch (raise(A)) {
  | exception A => raise(B)
  | _ => 11
  };

let switchWithCatchAll =
  switch (raise(A)) {
  | exception _ => 1
  | _ => 2
  };

let raiseInInternalLet = b => {
  let a = b ? raise(A) : 22;
  a + 34;
};

let indirectCall = () => () |> raisesWithAnnotaion;

[@raises Invalid_argument]
let array = a => a[2];

let id = x => x;

let tryChar = v => {
  try(id(Char.chr(v)) |> ignore) {
  | _ => ()
  };
  42;
};

module StringHash =
  Hashtbl.Make({
    include String;
    let hash = Hashtbl.hash;
  });

let specializedHash = tbl => StringHash.find(tbl, "abc");

[@raises Not_found]
let genericHash = tbl => Hashtbl.find(tbl, "abc");

[@raises Not_found]
let raiseAtAt = () => raise @@ Not_found;

[@raises Not_found]
let raisePipe = Not_found |> raise;