let foo = (~x=1, ~y=2, ~z=3, w) => x + y + z + w;

let bar = (~x=?, ~y, ~z=?, w) => y + w;