# master
- Add command-line options `-blacklist` and `-whitelist` that apply to all the analyses.
  Example: `../reanalyze.exe -exception -blacklist src -whitelist src/Ex`
  will only show results for files `src/Ex*`.
  The analyses are not affected (e.g. transitively dead code), only the reporting is.

# 1.10.0
- Exception: Add warning when `raise` or `raise_notrace` are used not in a direct call.
- Exception: support `raise @@ Exn` and `Exn |> raise`.
- Exception: model `Js.Json`.
- Exception: model `bs-json`.

# 1.9.0
- Exception analysis: Model exceptions for `Array`, `Buffer`, `Bytes`, `Char`, `Filename`, `Hashtbl`, `Pervasives`, `Str`, `String`.

# 1.8.0
- First feature-complete exception analysis with `-exception` (bucklescript) and `-exception-cmt` for native.

# 1.7.0
- Early preview of exception analysis with `-exception` (bucklescript) and `-exception-cmt` for native.

# 1.6.0
- When file paths are implicit, e.g. `Foo.re` turn them into `./Foo.re` to help editor integration.
- Add command-line option `-debug` and discontinue environment variable `Debug`.
- Add command-line option `-write` and discontinue environment variable `Write`.

# 1.5.0
- Work around issue with flushing when printing first location.

# 1.4.0
- Always use OCaml's location format, which is picked up automatically by most tooling.
- Add warning emitted when a live item is annotated @dead.

# 1.3.0
- Add command-line option `-live-names this,that` to treat items `this` and `that` as globally live.
- Support `@warning "-32"` in addition `@ocaml.warning "-32"` to mean `@live`.
- Add command-line option `live-paths prefix/of/path1,prefix/of/path2` to treat all items in the path prefixes as live.

# 1.2.0
- Turn off reporting on `_` as it interferes with the `@deriving` ppx.
- Add option to report types dead only in the interface, now off by default.
- Support `@ocaml.warning "-32"` to mean `@live`.
- Capture dependencies between types in interface and implementation files.

# 1.1.0
- More robust detection of file names (don't rely on the name from the .cmt file).
- Add support for building with OCaml 4.08.

# 1.0.0
Initial release.
