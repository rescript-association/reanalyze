let valueAlive = 1;
let valueDead = 2;

let valueOnlyInImplementation = 3;

let rec subList = (b, e, l) =>
  switch (l) {
  | [] => failwith("subList")
  | [h, ...t] =>
    let tail =
      if (e == 0) {
        [];
      } else {
        subList(b - 1, e - 1, t);
      };
    if (b > 0) {
      tail;
    } else {
      [h, ...tail];
    };
  };