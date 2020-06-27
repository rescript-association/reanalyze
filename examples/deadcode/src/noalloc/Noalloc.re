[@noalloc]
let x = 34;

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

[@noalloc]
let unpair2 = v => {
  let (x, y) = v;
  x + y;
};

[@noalloc]
let sumVec = v => {
  let ((x1, x2), (y1, y2)) = v;
  (x1 + y1, x2 + y2);
};

[@noalloc]
let scale = s => ((s, 1.0, 1.0), (1.0, s, 1.0), (1.0, 1.0, s));

[@noalloc]
let rotation = a => (
  (0.0, (-1.0) *. a, 0.0),
  (a, 0.0, 0.0),
  (0.0, 0.0, a),
);

[@noalloc]
let mulVecVec = (v1, v2) => {
  let (x1, y1, z1) = v1;
  let (x2, y2, z2) = v2;
  let x = x1 *. x2;
  let y = y1 *. y2;
  let z = z1 *. z2;
  x +. y +. z;
};

[@noalloc]
let mulMatVec = (m, v) => {
  let (row1, row2, row3) = m;
  let x = mulVecVec(row1, v);
  let y = mulVecVec(row2, v);
  let z = mulVecVec(row3, v);
  (x, y, z);
};

[@noalloc]
let restMatrix = v => mulMatVec(rotation(0.123), mulMatVec(scale(2.0), v));

[@noalloc]
let id = x => x;

[@noalloc]
let id2 = (x: (int, int)) => id(x);

[@noalloc]
let y = x;

[@noalloc]
let retGlobal = () => {
  y + 1;
};

[@noalloc]
let globalTuple = (1,2,3);