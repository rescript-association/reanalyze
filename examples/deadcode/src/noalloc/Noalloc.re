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

[@noalloc]
let duplicate = (x: (int, int)) => (x, x);

[@noalloc]
let local = n => {
  let a = 34;
  a + n;
};

[@noalloc]
let quad = x => {
  let a = (x, x + 1);
  (a, a);
};

[@noalloc]
let fl = 2.;

// [@noalloc]
// let unpair2 = v => {
//   let (x, y) = v;
//   x + y;
// };