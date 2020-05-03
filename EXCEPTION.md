The exception analysis reports a warning any time an exception is raised, and not caught:

```reason
let raises = () => raise(Not_found);
```

reports:

```sh

  Exception Analysis
  File "A.re", line 1, characters 4-10
  raises might raise Not_found (A.re:1:19) and is not annotated with @raises Not_found
```

No warning is reporteed when a `@raises` annotation is added:

```reason
[@raises Not_found]
let raises = () => raise(Not_found);
```

When a function raises multiple exceptions, a tuple annotation is used:


```reason
exception A;
exception B;

[@raises (A, B)]
let twExceptions = (x, y) => {
  if (x) {
    raise(A);
  };
  if (y) {
    raise(B);
  };
};
```


## Limitations

- The libraries currently modeled are limited to `Array`, `Buffer`, `Bytes`, `Char`, `Filename`, `Hashtbl`, `List`, `Pervasives`, `Str`, `String` from the standard library. Models are currently vendored in the analysis, and are easy to add (see [`src/ExnLib.re`](src/ExnLib.re))
- Generic exceptions are not understood by the analysis. For example `exn` is not recognized below (only concrete exceptions are):

```reason
try (foo()) { | exn => raise(exn) }
```

- Uses of e.g. `List.hd` are interpered as belonging to the standard libry. If you re-define `List` in the local scope, the analysis it will think it's dealing with `List` from the standard library.

- There is no special support for functors. So with `Hashtbl.Make(...)` the builtin model will not apply. So the analysis won't report that the following can raise `Not_found`:

```reason
module StringHash =
  Hashtbl.Make({
    include String;
    let hash = Hashtbl.hash;
  });
let specializedHash = tbl => StringHash.find(tbl, "abc");
```