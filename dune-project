(lang dune 3.6)

(name vendredi)

(generate_opam_files true)

(package
 (name vendredi)
 (synopsis "Tool for generating dune projects which vendor given packages for the purpose of testing that their dependencies are vendor-friendly")
 (depends
  dune
  cmdliner
  logs
  sexplib
  ppx_sexp_conv))
