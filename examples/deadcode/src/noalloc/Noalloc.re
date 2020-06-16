[@noalloc]
let x = 34 + 2;

[@noalloc]
let foo = (x, y) => x + y;

[@noalloc]
let bar = x => foo(x, x) + 1;