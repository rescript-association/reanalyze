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