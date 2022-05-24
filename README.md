# reanalyze

Program analysis for ReScript and OCaml projects targeting JS (ReScript) as well as native code (dune):

- Globally dead values, redundant optional arguments, dead modules, dead types (records and variants).
- Exception analysis.
- Termination.

## Expectations

Early release. While the core functionality is reasonably stable, the CLI and annotations are subject to change. However, this is a tiny surface at the moment.

## Use

The rest of this document describes the dead code analysis.
For the [Exception Analysis](EXCEPTION.md), build instructions are the same, and the command-line invocation is different.

Build and run on existing projects using the Build and Try instructions below. The analysis uses `.cmt[i]` files which are generated during compilation, so should be run _after_ building your project. Remember to rebuild the project before running again.

### CLI for ReScript projects

```sh
# dead code analysis
reanalyze.exe -dce

# exception analysis
reanalyze.exe -exception
```

The requirement is that `bsconfig.json` can be found by walking up the current directory.

### CLI for native projects

```sh
# dead code analysis
reanalyze.exe -dce-cmt root/containing/cmt/files

# exception analysis
reanalyze.exe -exception-cmt root/containing/cmt/files
```

Subdirectories are scanned recursively looking for `.cmt[i]` files.

The requirement is that the _current_ directory is where file paths start from. So if the file path seen by the compiler is relative `src/core/version.ml` then the current directory should contain `src` as a subdirectory. The analysis only reports on existing files, so getting this wrong means no reporting.

### DCE reports

The dead code analysis reports on globally dead values, redundant optional arguments, dead modules, dead types (records and variants).

A value `x` is dead if it is never used, or if it is used by a value which itself is dead (transitivity). At the top level, function calls such as `Js.log(x)`, or other expressions that might cause side effects, keep value `x` live.

An optional argument `~argName` to a function is redundant if all the calls to the function supply the argument, or if no call does.

A module is considered dead if all the elements defined it in are dead.

The type analysis repots on variant cases, and record labels.

- A variant case `| A(int)` is dead if a value such as `A(3)` is never constructed. But it can be deconstructed via pattern matching  `| A(n) => ...` or checked for equality `x == A(3)` without making the case `A` live.

- A record label `x` in `type r = {x:int, y:int}` is dead if it is never read (by direct access `r.x` or pattern matching `| {x:n, y:m} => ...`). However, creating a value `let r = {x:3, y:4}` does not make `x` and `y` live.
Note that reading a value `r` does not make `r.x` or `r.y` live.

While dead values can be removed automatically (see below), dead types require a bit more work. A dead variant case requires changing the type definition, and the various accesses to it. A dead record label requires changing the type definition, and removing the label from any expressions that create a value of that type.

### DCE: controlling reports with Annotations

The dead code analysis supports 2 annotations:

- `@dead` suppresses reporting on the value/type, but can also be used to force the analysis to consider a value as dead. Typically used to acknowledge cases of dead code you are not planning to address right now, but can be searched easily later.

- `@live` tells the analysis that the value should be considered live, even though it might appear to be dead. This is typically used in case of FFI where there are indirect ways to access values. In case of bucklescript projects using `genType`, export annotations immediately qualify values as live, because they are potentially reachable from JS.

The main difference between `@dead` and `@live` is the transitive behaviour: `@dead` values don't keep alive values they use, while `@live` values do.

Several examples can be found in
[`examples/deadcode/src/DeadTest.res`](examples/deadcode/src/DeadTest.res)

## Command-line Interface

### CLI -suppress
Takes a comma-separated list of path-prefixes. Don't report on files whose path has a prefix in the list (but still use them for analysis).

```sh
reanalyze.exe -suppress one/path,another/path
```

### CLI -unsuppress

Takes a comma-separated list of path-prefixes. Report on files whose path has a prefix in the list, overriding `-suppress` (no-op if `-suppress` is not specified).

```sh
reanalyze.exe -unsuppress one/path,another/path/File.res
```

### CLI -debug

Print debug information during the analysis

```sh
reanalyze.exe -debug ...
```

### Add annotations automatically

This overwrites your source files automatically with dead code annotations:

```sh
reanalyze.exe -write ...
```

### Remove code automatically (not interactively)

There's a dead code ppx (values only, not types) in this repository. It can be used to automatically remove code annotated `@dead`, as if it had been commented out.
Can be used after adding annotations automatically. The combination of automatic annotation and automatic elimination is a form of automatic dead code elimination. For projects that use a library, or that in general have code which is dead only temporarily.
There's obviously a level of risk in doing this automatic elimination. The safety net you can rely on is that the code must still compile.

### CLI -live-names

This automatically annotates `@live` all the items called `foo` or `bar`:

```sh
-live-names foo,bar
```

### CLI -live-paths

This automatically annotates `@live` all the items in file `Hello.res`:

```sh
-live-paths Hello.res
```

This automatically annotates `@live` all the items in the `src/test` and `tmp` folders:

```sh
-live-paths src/test,tmp
```

## Configuration via `bsconfig.json`

The `-config` option can be used to read the configuration from `bsconfig.json`: to
set what analyses should be run, as well as `suppress` and `unsuppress` configuration.

Example configuration inside `bsconfig.json`:
```json
{
  "reanalyze": {
    "analysis": ["dce", "exception"],
    "suppress": ["src/ToSuppress.res"],
    "unsuppress": ["this", "that"]
  }
}
```

This is equivalent to adding `-dce -exception -suppress src/ToSuppress.res -unsuppress this,that` to the command line in place of `-config`. Note that the options are additive, so it's possible to use e.g. `-config -exception` to add exception analysis on top of what the configuration does.

## Install with npm for ReScript projects

```
npm add --save-dev reanalyze
```

## Build From Sources


### Build for ReScript

```sh
opam install dune
npm run build406
# _build/default/src/Reanalyze.exe
```

### Build for OCaml native projects using dune

```sh
opam install dune
dune build
# _build/default/src/Reanalyze.exe
```

## Try it

### ReScript Projects (JS output)

```sh
npm run build # or whatever command to build the project
npm add --save-dev reanalyze
npx reanalyze -dce
```

### Native Projects (OCaml)

Make sure that `dune` builds both `.cmt` and `.cmti` files (see https://github.com/ocaml/dune/issues/3182 as to why):

This project is itself written in OCaml and can be analyzed as follows.
```sh
dune build @check @all -p reanalyze # makes sure that both .cmi and .cmti files are created
./_build/default/src/Reanalyze.exe -suppress src/compiler-libs-406 -dce-cmt _build
```