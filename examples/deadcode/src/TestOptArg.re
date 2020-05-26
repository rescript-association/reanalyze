Js.log(OptArg.bar(~z=3, ~y=3, 4));

let foo = (~x=3, y) => x + y;

let bar = () => {
  let x = Some(12);
  foo(~x?, 3);
};

Js.log(bar);