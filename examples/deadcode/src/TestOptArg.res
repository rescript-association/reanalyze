Js.log(OptArg.bar(~z=3, ~y=3, 4))

let foo = (~x=3, y) => x + y

let bar = () => foo(~x=12, 3)

Js.log(bar)
