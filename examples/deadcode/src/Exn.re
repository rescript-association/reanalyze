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