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

[@raises]
let raisesWithAnnotaion = () => raise(Not_found);

let callsRaiseWithAnnotation = raisesWithAnnotaion();

[@raises]
let callsRaiseWithAnnotationAndIsAnnotated = raisesWithAnnotaion();