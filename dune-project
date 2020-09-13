(lang dune 2.3)
(name reanalyze)

(generate_opam_files true)

(license MIT)
(maintainers "Cristiano Calcagno")
(authors "Cristiano Calcagno")
(source (github reason-association/reanalyze))

(implicit_transitive_deps false)

(package
 (name reanalyze)
 (description "Experimental analyses for OCaml/Reason: for globally dead values/types, exception analysis, and termination analysis.")
 (depends
  (ocaml (and (< 4.11) (>= 4.06.1)))
  (reason (>= 3.6.0))))
