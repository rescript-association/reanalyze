# reanalyze

Experimental analyses for OCaml/Reason: for dead values/types, and for termination.
Supports bucklescript projects, as well as native projects (e.g. dune).

**Status `master (v3.*)`:** [![Build
Status](https://dev.azure.com/ccrisccris/reanalyze/_apis/build/status/cristianoc.reanalyze?branchName=master)](https://dev.azure.com/ccrisccris/reanalyze/_build/latest?definitionId=1&branchName=master)

## Expectations

Early release. While the core functionality is reasomably stable, the CLI and annotations are subject to change. However, this is a tiny surface at the moment.

## Use
Build and run on existing projects using the Build and Try instructions below. The analysis uses `.cmt[i]` files which are generated during compilation, so should be run *after* building your project. Remember to rebuild the project before running again.

### CLI for bucklescript projects
```
reanalyze.exe -dce
```
The requirement is that `bsconfig.json` can be found by walking up the current directory.

### CLI for native projects
```
reanalyze.exe -dce-cmt root/containing/cmt/files
```
Subdirectories are scanned recursively looking for `.cmt[i]` files.

The requirement is that the *current* directory is where file paths start from. So if the file path seen by the compiler is relative `src/core/version.ml` then the current directory should contain `src` as a subdirectory. The analysis only reports on existing files, so getting this wrong means no reporting.

### Controlling reports with Annotations
The dead code analysis supports 2 annotations:

* `@dead` suppresses reporting on the value/type, but can also be used to force the analysis to consider a value as dead. Typically used to acknowledge cases of dead code you are not planning to address right now, but can be searched easily later.

* `@live` tells the analysis that the value should be considered live, even though it might appear to be dead. This is typically used in case of FFI where there are indirect ways to access values. In case of bucklescript projects using `genType`, export annotations immediately qualify values as live, because they are potentially reachable from JS.

The main difference between `@dead` and `@live` is the transitive behaviour: `@dead` values don't keep alive values they use, while `@live` values do.

Several examples can be found in
[`examples/deadcode/src/DeadTest.re`](examples/deadcode/src/DeadTest.re)

## Undocumented features
Here are several undocumented features, which could change substantially over time. Ask to for more information.

### Debug
```sh
Debug=1 reanalyze.exe ...
```

### Whitelist and Blacklist
```sh
Whitelist=src Blacklist=src/DeadTestBlacklist.re reanalyze.exe ... 
``` 
This currently only affects final reporting, not the analysis itself.


### Add annotations automatically
This overwrites your source files automatically with dead code annotations:

```sh
Write=1 reanalyze.exe ...
```

### Remove code automatically (not interactively)
There's a dead code ppx (values only, not types) in this repository. It can be used to automatically remove code annotated `@dead`, as if it had been commented out.
Can be used after adding annotations automatically. The combination of automatic annotation and automatic elimination is a form of automatic dead code elimination. For projects that use a library, or that in general have code which is dead only temporarily.
There's obviously a level of risk in doing this automatic elimination. The safety net you can rely on is that the code must still compile.

## Build

### Build for OCaml 4.06.1 using esy (for bucklescript and native projects)
```sh
npm install
npx esy
npx esy x which reanalyze.exe
  /Users/cristianoc/reasonml/reanalyze/_esy/default/store/i/reanalyze-5574f798/bin/reanalyze.exe
```

### Build for OCaml 4.08.1 using esy (for native projects)
```sh
npm install
npx esy @408
npx esy @408 x which reanalyze.exe
  /Users/cristianoc/reasonml/reanalyze/_esy/408/store/i/reanalyze-b543bfd4/bin/reanalyze.exe
```

### Build using opam/dune
```sh
opam switch 4.08.1 # or 4.06.1
opam install reason # for dune
dune build
./_build/install/default/bin/reanalyze.exe -version
  reanalyze version 1.1.0
```

## Try it

### Bucklescript Projects (JS output)
```sh
npm run build # or whatever command to build the project
npm add reanalyze
npx reanalyze -dce 
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

