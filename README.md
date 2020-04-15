# reanalyze

Experimental analyses for OCaml/Reason: for dead values/types, and for termination.
Supports bucklescript projects, as well as native projects (e.g. dune).

**Status `master (v3.*)`:** [![Build
Status](https://dev.azure.com/ccrisccris/reanalyze/_apis/build/status/cristianoc.reanalyze?branchName=master)](https://dev.azure.com/ccrisccris/reanalyze/_build/latest?definitionId=1&branchName=master)


## Build for OCaml 4.06.1 (bucklescript and native projects)
```sh
npm install
npx esy
npx esy x which reanalyze.exe
  /Users/cristianoc/reasonml/reanalyze/_esy/default/store/i/reanalyze-5574f798/bin/reanalyze.exe
```

## Build for OCaml 4.08.1 (native projects)
```sh
npm install
npx esy @408
npx esy @408 x which reanalyze.exe
  /Users/cristianoc/reasonml/reanalyze/_esy/408/store/i/reanalyze-b543bfd4/bin/reanalyze.exe
```

## Bucklescript Projects
```sh
npm run build # or whatever command to build the project
npm add reanalyze
npx reanalyze -dce 
```

## Single File Test (native project)
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

## Single Directory Test (native project)
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

## Full Project Test: Infer (native project)
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

