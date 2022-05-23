# reanalyze

Program analysis for ReScript and OCaml projects targeting JS (ReScript) as well as native code (dune):

- Globally dead values, redundant optional arguments, dead modules, dead types (records and variants).
- Exception analysis.
- Termination.

## Expectations

Early release. While the core functionality is reasonably stable, the CLI and annotations are subject to change. However, this is a tiny surface at the moment.

# Requirements
For correct handling of ReasonReact components in the Dead Code Analysis, bucklescript version 7.3.2 is required (fixes the location in the JSX PPX).

## Use

The rest of this document describes the dead code analysis.
For the [Exception Analysis](EXCEPTION.md), build instructions are the same, and the command-line invocation is different.

Build and run on existing projects using the Build and Try instructions below. The analysis uses `.cmt[i]` files which are generated during compilation, so should be run _after_ building your project. Remember to rebuild the project before running again.

### CLI for bucklescript projects

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

## Unstable features

Here are several unstable features, which could change substantially over time. Ask for more information.

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

## Build

## No build required for bucklescript projects

```
npm add --save-dev reanalyze
```

### Build for OCaml 4.06.1 using dune (for bucklescript and native projects)

```sh
opam switch 4.06.1
eval $(opam env)
opam install dune
dune build
# _build/default/src/Reanalyze.exe
```

### Build for other OCaml versions (4.08, 4.09, 4.10) using dune (for native projects)

```sh
opam install dune
dune build
# _build/default/src/Reanalyze.exe
```

## Try it

### Bucklescript Projects (JS output)

```sh
npm run build # or whatever command to build the project
npm add --save-dev reanalyze
npx reanalyze -dce
```

### esy Projects (native)
See for example project [reanalyze-esy-example](https://github.com/cristianoc/reanalyze-esy-example):

```sh
git clone https://github.com/cristianoc/reanalyze-esy-example
cd reanalyze-esy-example
esy
esy dce
esy check-exceptions
```

### Single File Test (native project)

```sh
echo "let unused = 34" > test.ml
ocamlc -c -bin-annot test.ml
reanalyze.exe -dce-cmt ./test.cmt
  Warning Dead Value
  test.ml 1:1-15
  unused is never used
  <-- line 1
  let unused = 34 [@@dead "unused"]
```

### Single Directory Test (native project)

```sh
mkdir test
echo "let unused = 34 let used = 42" > test/test.ml
echo "let _ = Test.used" > test/use.ml
cd test
ocamlc -c -bin-annot *.ml
reanalyze.exe -dce-cmt .
  Warning Dead Value
  test.ml 1:1-15
  unused is never used
  <-- line 1
  let unused = 34 [@@dead "unused"]  let used = 42
  Warning Dead Value
  use.ml 1:1-17
  _ has no side effects and can be removed
  <-- line 1
  let _ = Test.used [@@dead "_"]
```

### Full Project Test: Infer (native project)

How to test on [Infer](https://github.com/facebook/infer) :

- Make sure that `dune` builds both `.cmt` and `.cmti` files (see https://github.com/ocaml/dune/issues/3182 as to why):

```
--- a/infer/src/Makefile
+++ b/infer/src/Makefile
-DUNE_BUILD = dune build --profile=$(BUILD_MODE)
+DUNE_BUILD = dune build @check @all --profile=$(BUILD_MODE)
```

- Build normally

```
make -j BUILD_MODE=dev
```

- Go to the right directory from which file paths start:

```
% cd infer/infer
```

- Run the analysis

```
% path/to/reanalyze.exe -dce-cmt _build/default/src/.InferModules.objs/byte/
```

<img width="1362" alt="Screen Shot 2020-04-14 at 9 28 24 AM" src="https://user-images.githubusercontent.com/7965335/79213744-fb2d8c00-7e49-11ea-9417-3c42bd6a3a79.png">
