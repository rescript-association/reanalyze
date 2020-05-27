let foo = (~x=1, ~y=2, ~z=3, w) => x + y + z + w;

let bar = (~x=?, ~y, ~z=?, w) => y + w;

Js.log(foo(~x=3, 4));

Js.log(bar(~y=3, 4));

let threeArgs = (~a=1, ~b=2, ~c=3, d) => a + b + c + d;

Js.log(threeArgs(~a=4, ~c=7, 1));
Js.log(threeArgs(~a=4, 1));