# master
- Always use OCaml's location format, which is picked up automatically by most tooling.

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
