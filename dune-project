(lang dune 2.0)
(name reanalyze)

(generate_opam_files true)

(license MIT)
(authors "Cristiano Calcagno")
(maintainers "Cristiano Calcagno")
(source (github cristianoc/reanalyze))
(bug_reports "https://github.com/cristianoc/reanalyze/issues")
(homepage "https://github.com/cristianoc/reanalyze")

(package
 (name reanalyze)
 (description "Experimental analyses for OCaml/Reason: for globally dead values/types, exception analysis, and termination analysis.")
 (depends
  (ocaml (>= 4.06))
  (dune (>= 2.0))
))
