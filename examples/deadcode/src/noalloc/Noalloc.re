[@noalloc]
let x = 34 + 2;

[@noalloc]
let foo = (x, y) => x + y;

[@noalloc]
let bar = x => foo(x, x) + 1;

[@noalloc]
let pair = (x, y) => (x, y);

[@noalloc]
let unpair = ((x, y)) => x + y;

[@noalloc]
let mixed = ((p0, p1, p2), (p3, (p4, p5, (p6, p7, p8)), p9)) => (
  p0 + p1 + p2,
  p3 + p4 + p5 + p6 + p7 + p8 + p9,
);